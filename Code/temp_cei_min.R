suppressPackageStartupMessages({
  library(haven)
  library(ggplot2)
  library(dplyr)
  library(cowplot)
})

# Run from Code (temp_cei_min.do invokes this file there).
code_dir <- getwd()
figure_dir <- file.path(code_dir, "results", "figures")

panel_individual <- read_dta(file.path(code_dir, "data", "panel_individual.dta"))
cei <- read.csv(
  file.path(code_dir, "..", "Archive", "CEI-files", "results", "cei_pairwave.csv"),
  colClasses = c(pair_id = "character")
) |>
  filter(spec == "tempered") |>
  transmute(group_id = pair_id, post = wave, cei_g = e_coll)

sig_mark <- function(p) {
  case_when(
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.10 ~ "+",
    TRUE ~ ""
  )
}

pooled_median <- median(panel_individual$ccei_i, na.rm = TRUE)

group_data <- panel_individual |>
  filter(!is.na(group_id), !is.na(post), !is.na(ccei_i)) |>
  group_by(group_id, post) |>
  summarise(
    n_members = n(),
    n_high = sum(ccei_i >= pooled_median),
    .groups = "drop"
  ) |>
  filter(n_members == 2) |>
  left_join(cei, by = c("group_id", "post")) |>
  mutate(
    pair_category = factor(
      n_high,
      levels = c(0, 1, 2),
      labels = c("Low-Low", "Low-High", "High-High")
    )
  )

stopifnot(nrow(group_data) == 1304, !anyNA(group_data$cei_g))

group_stats <- group_data |>
  group_by(pair_category) |>
  summarise(
    mean = mean(cei_g),
    sd = sd(cei_g),
    n = n(),
    se = sd / sqrt(n),
    ci = qt(0.975, n - 1) * se,
    .groups = "drop"
  )

pairwise_diff <- function(data, lower_group, upper_group) {
  comparison <- data |>
    filter(pair_category %in% c(lower_group, upper_group)) |>
    droplevels()
  difference <- mean(
    comparison$cei_g[comparison$pair_category == upper_group]
  ) - mean(
    comparison$cei_g[comparison$pair_category == lower_group]
  )
  test <- t.test(cei_g ~ pair_category, data = comparison)
  tibble(
    comparison = paste(upper_group, "minus", lower_group),
    difference = difference,
    p_value = test$p.value,
    label = sprintf("Diff. = %.3f%s", difference, sig_mark(test$p.value))
  )
}

diff_extreme <- pairwise_diff(group_data, "Low-Low", "High-High")
diff_low_mid <- pairwise_diff(group_data, "Low-Low", "Low-High")
diff_mid_high <- pairwise_diff(group_data, "Low-High", "High-High")

group_y_min <- max(
  0.75,
  floor((min(group_stats$mean - group_stats$ci) - 0.02) * 100) / 100
)
group_y_top <- max(group_stats$mean + group_stats$ci)
bracket_low <- group_y_top + 0.025
bracket_high <- group_y_top + 0.070

group_bar <- ggplot(group_stats, aes(pair_category, mean, fill = pair_category)) +
  geom_col(width = 0.62, colour = "black", linewidth = 0.3) +
  geom_errorbar(
    aes(ymin = mean - ci, ymax = mean + ci),
    width = 0.16, linewidth = 0.45
  ) +
  annotate("segment", x = 1, xend = 2, y = bracket_low, yend = bracket_low) +
  annotate("segment", x = 1, xend = 1,
           y = bracket_low - 0.020, yend = bracket_low) +
  annotate("segment", x = 2, xend = 2,
           y = bracket_low - 0.020, yend = bracket_low) +
  annotate("label", x = 1.48, y = bracket_low + 0.012,
           label = diff_low_mid$label, size = 18 / .pt,
           linewidth = 0, fill = "white") +
  annotate("segment", x = 2, xend = 3, y = bracket_low, yend = bracket_low) +
  annotate("segment", x = 2, xend = 2,
           y = bracket_low - 0.020, yend = bracket_low) +
  annotate("segment", x = 3, xend = 3,
           y = bracket_low - 0.020, yend = bracket_low) +
  annotate("label", x = 2.58, y = bracket_low + 0.012,
           label = diff_mid_high$label, size = 17 / .pt,
           linewidth = 0, fill = "white") +
  annotate("segment", x = 1, xend = 3, y = bracket_high, yend = bracket_high) +
  annotate("segment", x = 1, xend = 1,
           y = bracket_high - 0.020, yend = bracket_high) +
  annotate("segment", x = 3, xend = 3,
           y = bracket_high - 0.020, yend = bracket_high) +
  annotate("label", x = 2, y = bracket_high + 0.012,
           label = diff_extreme$label, size = 18 / .pt,
           linewidth = 0, fill = "white") +
  scale_fill_manual(values = c(
    "Low-Low" = "#E39695",
    "Low-High" = "#D8C98C",
    "High-High" = "#74A9CF"
  )) +
  scale_x_discrete(labels = c(
    "Low-Low" = "Low\nLow",
    "Low-High" = "Low\nHigh",
    "High-High" = "High\nHigh"
  )) +
  scale_y_continuous(
    breaks = seq(0.85, 1.00, by = 0.05),
    labels = scales::label_number(accuracy = 0.01),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(group_y_min, 1.08)) +
  labs(x = NULL, y = "Mean Group CEI") +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "white", colour = NA)
  )

group_cdf <- ggplot(
  group_data,
  aes(cei_g, colour = pair_category, linetype = pair_category)
) +
  stat_ecdf(geom = "step", linewidth = 0.9, pad = FALSE) +
  scale_colour_manual(values = c(
    "Low-Low" = "red",
    "Low-High" = "grey45",
    "High-High" = "blue"
  )) +
  scale_linetype_manual(values = c(
    "Low-Low" = "dashed",
    "Low-High" = "dotdash",
    "High-High" = "solid"
  )) +
  scale_x_continuous(
    limits = c(0.1, 1), breaks = seq(0.2, 1, 0.2), expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
  ) +
  labs(
    x = expression("Group CEI (" * CEI[g] * ")"),
    y = "Cumulative probability",
    colour = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background = element_rect(fill = "white", colour = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA)
  )

panels <- plot_grid(group_bar, group_cdf, nrow = 1, rel_widths = c(1, 1))
combined <- ggdraw() +
  draw_plot(panels, x = 0, y = 0.08, width = 1, height = 0.85) +
  draw_label("Group CEI by Members' Individual CCEI Category",
             x = 0.5, y = 0.975, size = 16) +
  draw_label("(a) Mean Group CEI", x = 0.25, y = 0.025, size = 13) +
  draw_label("(b) CDFs of Group CEI", x = 0.75, y = 0.025, size = 13)

output_file <- file.path(
  figure_dir, "temp_cei_min_group_cei_by_member_ccei_category.png"
)
ggsave(output_file, combined, width = 12, height = 5.4, dpi = 300, bg = "white")

# Assemble the four generated tables and figure into one standalone PDF.
build_dir <- file.path(code_dir, "tmp", "pdfs")
pdf_dir <- file.path(code_dir, "output", "pdf")
dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)

tex_file <- file.path(code_dir, "temp_cei_min_results.tex")
stopifnot(file.exists(tex_file))
pdflatex <- Sys.which("pdflatex")
if (pdflatex == "") stop("pdflatex is required to build the standalone PDF.")

latex_args <- c(
  "-interaction=nonstopmode",
  "-halt-on-error",
  paste0("-output-directory=", shQuote(build_dir)),
  shQuote(tex_file)
)
for (run in 1:2) {
  latex_output <- system2(pdflatex, latex_args, stdout = TRUE, stderr = TRUE)
  status <- attr(latex_output, "status")
  if (!is.null(status) && status != 0) {
    stop(paste(latex_output, collapse = "\n"))
  }
}

built_pdf <- file.path(build_dir, "temp_cei_min_results.pdf")
final_pdf <- file.path(pdf_dir, "temp_cei_min_results.pdf")
stopifnot(file.exists(built_pdf), file.copy(built_pdf, final_pdf, overwrite = TRUE))

print(group_stats)
print(bind_rows(diff_low_mid, diff_mid_high, diff_extreme))
cat(sprintf("Pooled individual CCEI median: %.12f\n", pooled_median))
cat("Created:", output_file, "\n")
cat("Created:", final_pdf, "\n")
