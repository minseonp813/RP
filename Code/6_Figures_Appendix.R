################################################################################
# 6_Figures_Appendix.R
# Latest update: 2026-10-11
# Locked draft order: Figures A4 (CCEI CDFs), A5 (RA CDFs), A6 (I-M histogram),
# A7 (Table 3 Shapley decomposition), A8 (placebo CCEI-difference coefficients),
# A10 (multinomial AMEs), A11 (alternative indices), A12 (joint-outcome R2),
# followed by supplementary figures.
# Purpose: appendix figures, including the current I-M histogram (A6) and
#          the four-specification Shapley decomposition (A7) and placebo panels (A8).
# Inputs: Code/data panels, retained validated CCEI/M panel, and the
#         Figure A7 CSV from 5 and the retained full donor matrix.
#         Collective-outcome AME and R2 exports from 3/5.
# Outputs: results/figures/figure_A7/*.pdf (copied to Overleaf), hist_IminusM_ig.png
#          figure4_placebo_col{5,6}.pdf and the other appendix figures.
#          A10--A12 panels in results/figures/collective_rationality (copied to Overleaf).
#          The A7/A8 review PNGs are retained for chat.
################################################################################

rm(list = ls())

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) == 1) {
  code_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
} else if (requireNamespace("rstudioapi", quietly = TRUE) &&
           rstudioapi::isAvailable()) {
  code_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  code_dir <- getwd()
}

.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
suppressPackageStartupMessages({
  library(haven)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(grid)
})

data_dir <- file.path(code_dir, "data")
result_dir <- file.path(code_dir, "results")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
figure_dir <- file.path(result_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

save_appendix_figure <- function(plot, filename, width = 7, height = 5) {
  output_file <- file.path(figure_dir, filename)
  ggsave(output_file, plot, width = width, height = height, dpi = 300)
  stopifnot(file.copy(output_file,
    file.path(code_dir, "../Overleaf/figures_2025", filename), overwrite = TRUE))
}

panel_individual <- read_dta(file.path(data_dir, "panel_individual.dta"))

sig_mark <- function(p) {
  dplyr::case_when(
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.10 ~ "+",
    TRUE ~ ""
  )
}

cdf_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.18, 0.84),
    legend.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

make_wave_cdf <- function(data, value, x_label, x_limits = c(0, 1)) {
  ggplot(
    data,
    aes(
      x = .data[[value]],
      colour = wave,
      linetype = wave
    )
  ) +
    stat_ecdf(geom = "step", linewidth = 0.8, pad = FALSE) +
    scale_colour_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
    scale_linetype_manual(values = c("Baseline" = "solid", "Endline" = "dashed")) +
    scale_x_continuous(
      limits = x_limits,
      breaks = seq(0, 1, by = 0.2),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      expand = c(0, 0)
    ) +
    labs(
      x = x_label,
      y = "Cumulative Frequency",
      colour = NULL,
      linetype = NULL
    ) +
    cdf_theme
}

################################################################################
# Figure A4: CDF Of Individual And Group CCEIs
################################################################################

individual_ccei_data <- panel_individual |>
  filter(!is.na(ccei_i), !is.na(post)) |>
  distinct(id, post, ccei_i) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

group_ccei_data <- panel_individual |>
  filter(!is.na(ccei_g), !is.na(post)) |>
  distinct(group_id, post, ccei_g) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

individual_ccei_cdf <- make_wave_cdf(
  individual_ccei_data, "ccei_i", "Individual CCEI"
)
group_ccei_cdf <- make_wave_cdf(
  group_ccei_data, "ccei_g", "Group CCEI", c(0.1, 1)
)

save_appendix_figure(
  individual_ccei_cdf, "figure_individual_ccei_cdf_baseline_endline.png"
)
save_appendix_figure(
  group_ccei_cdf, "figure_group_ccei_cdf_baseline_endline.png"
)

################################################################################
# Figure A5: CDF Of Individual And Group Risk Aversion
################################################################################

individual_ra_data <- panel_individual |>
  filter(!is.na(RA_i), !is.na(post)) |>
  distinct(id, post, RA_i) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

group_ra_data <- panel_individual |>
  filter(!is.na(RA_g), !is.na(post)) |>
  distinct(group_id, post, RA_g) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

individual_ra_cdf <- make_wave_cdf(
  individual_ra_data, "RA_i", "Individual Risk Aversion"
)
group_ra_cdf <- make_wave_cdf(
  group_ra_data, "RA_g", "Group Risk Aversion"
)

save_appendix_figure(
  individual_ra_cdf, "figure_individual_ra_cdf_baseline_endline.png"
)
save_appendix_figure(
  group_ra_cdf, "figure_group_ra_cdf_baseline_endline.png"
)

################################################################################
# Figure A6: same I-M data and pooled member-category percentages as the main figure.
################################################################################
panel <- read_dta(file.path(code_dir, "data/panel_individual.dta"))
stopifnot(nrow(panel) == 2608,
          max(abs(panel$Istar_ccei - (panel$I_ccei - panel$M_ccei)), na.rm = TRUE) < 1e-12,
          all(panel$n_ccei_donors == 651))
defined <- panel[!is.na(panel$Istar_ccei), ]
stopifnot(nrow(defined) == 2560,
          max(abs(aggregate(Istar_ccei ~ group_id + post, defined, sum)$Istar_ccei)) < 1e-7)
defined$member <- factor(defined$HighCCEI_both_high, levels = c(1, 0),
                        labels = c("Higher CCEI", "Lower CCEI"))
# Normalize percentages within member category, pooling both waves.
defined$weight <- 100 / as.numeric(table(defined$member)[defined$member])
histogram <- ggplot(defined, aes(Istar_ccei, weight = weight,
                               fill = member, colour = member)) +
  geom_histogram(binwidth = .05, boundary = 0, closed = "left",
                 position = "identity", alpha = .35, linewidth = .4) +
  scale_fill_manual(values = c("#4C78A8", "#E69191")) +
  scale_colour_manual(values = c("#2C6FA0", "#E69191")) +
  scale_x_continuous(breaks = seq(-1, 1, .25)) +
  coord_cartesian(xlim = c(-1, 1), expand = FALSE) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, .05))) +
  labs(x = expression("Placebo-adjusted revealed-preference distance (" * I[ig] - M[ig] * ")"),
       y = "Percent", fill = NULL, colour = NULL) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom", plot.margin = margin(6, 18, 6, 8),
        plot.background = element_rect(fill = "white", colour = NA))
save_appendix_figure(histogram, "hist_IminusM_ig.png", width = 8, height = 5.1)

################################################################################
# Figure A7: four Table 3 specifications, with separate manuscript subfigures.
# Run independently from Code after the matching section of 5.
################################################################################
plot_table3_shapley_review <- function(
    input_file = file.path(result_dir, "figure_A7_review/table3_shapley.csv"),
    output_file = file.path(result_dir, "figure_A7_review/table3_shapley.png"),
    panel_dir = file.path(figure_dir, "figure_A7")) {
  decomposition <- read.csv(input_file)
  columns <- c(2, 3, 5, 6)
  stopifnot(nrow(decomposition) == 20, all(decomposition$N == 2560),
            all(decomposition$clusters == 64),
            all(decomposition$shapley_value >= -1e-10))
  # Top to bottom, matching the legend.
  blocks <- c("Rationality", "Individual/Friendship", "Corner/Midpoint shares",
              "M benchmark", "Fixed effects")
  fills <- c("#7FB2D8", adjustcolor("#D94B4B", alpha.f = .55),
             adjustcolor("#D94B4B", alpha.f = .8), "#E4E4E4", "#C8C8C8")
  titles <- c("Column (2): Higher CCEI / class FE",
              "Column (3): Higher CCEI / individual FE",
              "Column (5): CCEI difference / class FE",
              "Column (6): CCEI difference / individual FE")
  legend_labels <- c("Individual CCEI", "Individual / friendship",
                     "Corner / midpoint shares", "Placebo benchmark M", "Fixed effects")
  ymax <- ceiling(max(decomposition$total_r2) / .1) * .1
  draw_block <- function(x0, y0, x1, y1, block) {
    rect(x0, y0, x1, y1, col = fills[block], border = "#333333", lwd = 1,
         density = if (block == 3) 18 else NULL, angle = 45)
  }
  draw_panel <- function(panel, show_title = FALSE) {
    d <- subset(decomposition, column == columns[panel])
    d$block[d$block %in% c("Class FE", "Individual FE")] <- "Fixed effects"
    d <- d[match(rev(blocks), d$block), ]
    stopifnot(!anyNA(d$block), abs(sum(d$shapley_value) - d$total_r2[1]) < 1e-10,
              abs(sum(d$shapley_percent) - 100) < 1e-8)
    d$bottom <- c(0, head(cumsum(d$shapley_value), -1))
    d$top <- cumsum(d$shapley_value)
    d$middle <- (d$bottom + d$top) / 2
    values <- ifelse(d$shapley_value < .0005, "<0.001", sprintf("%.3f", d$shapley_value))
    percentages <- ifelse(d$shapley_percent < .05, "<0.1", sprintf("%.1f", d$shapley_percent))
    d$label <- sprintf("%s (%s%%)", values, percentages)
    par(mar = c(.6, 4.1, if (show_title) 2.5 else .6, .5), family = "sans")
    plot.new()
    plot.window(xlim = c(0, 1.65), ylim = c(0, ymax), xaxs = "i", yaxs = "i")
    ticks <- seq(0, ymax, .1)
    abline(h = ticks, col = "#ECECEC", lwd = 1)
    axis(2, at = ticks, labels = sprintf("%.1f", ticks), las = 1,
         col = NA, col.axis = "#555555", cex.axis = .9)
    mtext(expression(R^2 ~ "Contribution"), side = 2, line = 2.8, cex = .95)
    if (show_title) title(main = titles[panel], cex.main = 1, font.main = 1, line = .7)
    for (row in seq_len(nrow(d))) {
      draw_block(.32, d$bottom[row], .90, d$top[row], match(d$block[row], blocks))
    }
    inside <- d$shapley_value >= .05
    text(.61, d$middle[inside], d$label[inside], cex = .82)
    small <- which(!inside)
    label_y <- d$middle[small]
    label_y[1] <- max(label_y[1], .025)
    if (length(small) > 1) {
      for (k in 2:length(small)) label_y[k] <- max(label_y[k], label_y[k - 1] + .042)
      label_y <- label_y - max(0, max(label_y) - ymax + .025)
    }
    segments(.90, d$middle[small], 1.02, label_y, col = "#777777", lwd = .7)
    text(1.05, label_y, d$label[small], adj = 0, cex = .78)
  }
  draw_legend <- function() {
    par(mar = c(0, 0, 0, 0), family = "sans")
    plot.new()
    # Base legend fills columns first; this yields the requested row-wise order.
    order <- c(1, 4, 2, 5, 3)
    legend("center", legend = legend_labels[order], fill = fills[order],
           border = "#333333", density = c(NA, NA, 18, NA, NA)[order],
           angle = 45, bty = "n", ncol = 3, cex = .9)
  }
  dir.create(panel_dir, recursive = TRUE, showWarnings = FALSE)
  for (panel in seq_along(columns)) {
    pdf(file.path(panel_dir, sprintf("table3_col%d.pdf", columns[panel])),
        width = 5, height = 3.6, pointsize = 14, useDingbats = FALSE)
    draw_panel(panel)
    dev.off()
  }
  pdf(file.path(panel_dir, "legend.pdf"), width = 10, height = .8,
      pointsize = 14, useDingbats = FALSE)
  draw_legend()
  dev.off()
  png(output_file, width = 3300, height = 2550, res = 300, pointsize = 14)
  par(oma = c(0, 0, 2, 0))
  layout(matrix(c(1, 2, 3, 4, 5, 5), nrow = 3, byrow = TRUE),
         heights = c(1, 1, .18))
  for (panel in seq_along(columns)) draw_panel(panel, show_title = TRUE)
  draw_legend()
  mtext("Revealed-Preference Distance: Shorrocks–Shapley Decomposition",
        outer = TRUE, side = 3, line = .6, cex = 1.2)
  dev.off()
  manuscript_dir <- file.path(code_dir, "../Overleaf/figures_2025/figure_A7")
  dir.create(manuscript_dir, recursive = TRUE, showWarnings = FALSE)
  panel_files <- file.path(panel_dir, c(sprintf("table3_col%d.pdf", columns), "legend.pdf"))
  stopifnot(all(file.copy(panel_files, manuscript_dir, overwrite = TRUE)))
  invisible(decomposition)
}
plot_table3_shapley_review()

################################################################################
# Figure A8: Placebo tests for Table 3 Columns (5)/(6), immediately after A7.
# Run from Code after 3_Tables_Main.do; main Figure 4 is not a prerequisite.
if (!exists("code_dir")) code_dir <- getwd()
.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(ggplot2)
library(dplyr)
source(file.path(code_dir, "programs/estimate_table3_placebo.R"))
estimate_table3_placebo(code_dir, c(5L, 6L), "figureA8_placebo")
source(file.path(code_dir, "programs/plot_table3_placebo.R"))
export_table3_placebo_panels(code_dir, c(5L, 6L), "figure_A8_placebo_coefficients.png",
                            input_prefix = "figureA8_placebo")

################################################################################
# Supplementary figure: CDFs of absolute risk-preference distance, by wave
################################################################################

make_absolute_ra_distance_cdf <- function(data, wave_value) {
  plot_data <- data |>
    filter(
      post == wave_value,
      !is.na(RA_i),
      !is.na(RA_g),
      !is.na(HighCCEI_both_high)
    ) |>
    mutate(
      absolute_RA_distance = abs(RA_i - RA_g),
      rationality_group = factor(
        if_else(HighCCEI_both_high == 1, "High Rationality", "Low Rationality"),
        levels = c("Low Rationality", "High Rationality")
      )
    )

  ggplot(
    plot_data,
    aes(
      absolute_RA_distance,
      colour = rationality_group,
      linetype = rationality_group
    )
  ) +
    stat_ecdf(geom = "step", linewidth = 0.9, pad = FALSE) +
    scale_colour_manual(
      values = c("Low Rationality" = "blue", "High Rationality" = "red")
    ) +
    scale_linetype_manual(
      values = c("Low Rationality" = "dashed", "High Rationality" = "solid")
    ) +
    scale_x_continuous(
      limits = c(0, 0.5),
      breaks = seq(0, 0.5, 0.1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      expand = c(0, 0)
    ) +
    labs(
      x = expression("|" * RA[i] - RA[g] * "|"),
      y = "Cumulative Frequency",
      colour = NULL,
      linetype = NULL
    ) +
    cdf_theme
}

absolute_ra_cdf_baseline <- make_absolute_ra_distance_cdf(
  panel_individual, 0
)
absolute_ra_cdf_endline <- make_absolute_ra_distance_cdf(
  panel_individual, 1
)

ggsave(
  file.path(result_dir, "risk_preference_distance_cdf_baseline.png"),
  absolute_ra_cdf_baseline, width = 7, height = 5, dpi = 300
)
ggsave(
  file.path(result_dir, "risk_preference_distance_cdf_endline.png"),
  absolute_ra_cdf_endline, width = 7, height = 5, dpi = 300
)

################################################################################
# Supplementary figure: Normalized Risk-Aversion Distance By Members' CCEI
################################################################################

ra_data <- panel_individual |>
  mutate(
    denominator = (RA_i - RA_g)^2 + (RA_j - RA_g)^2,
    RA_I_ig = if_else(
      denominator > 0,
      (RA_i - RA_g)^2 / denominator,
      NA_real_
    ),
    ccei_group = factor(
      if_else(HighCCEI_both_high == 1, "Higher CCEI", "Lower CCEI"),
      levels = c("Lower CCEI", "Higher CCEI")
    )
  ) |>
  filter(!is.na(RA_I_ig), !is.na(ccei_group))

ra_stats <- ra_data |>
  group_by(ccei_group) |>
  summarise(
    mean = mean(RA_I_ig),
    sd = sd(RA_I_ig),
    n = n(),
    se = sd / sqrt(n),
    ci = qt(0.975, n - 1) * se,
    .groups = "drop"
  )

# A paired comparison requires a higher/lower ranking, so exclude CCEI ties.
ra_test_data <- ra_data |>
  filter(ccei_i != ccei_j) |>
  select(group_id, post, ccei_group, RA_I_ig) |>
  distinct() |>
  pivot_wider(names_from = ccei_group, values_from = RA_I_ig) |>
  filter(!is.na(`Lower CCEI`), !is.na(`Higher CCEI`))
ra_test <- t.test(
  ra_test_data$`Lower CCEI`,
  ra_test_data$`Higher CCEI`,
  paired = TRUE
)
ra_difference <- mean(
  ra_test_data$`Lower CCEI` - ra_test_data$`Higher CCEI`
)
ra_y <- max(ra_stats$mean + ra_stats$ci) + 0.045

ra_bar <- ggplot(ra_stats, aes(ccei_group, mean, fill = ccei_group)) +
  geom_col(width = 0.62, colour = "black", linewidth = 0.3) +
  geom_errorbar(
    aes(ymin = mean - ci, ymax = mean + ci),
    width = 0.16, linewidth = 0.45
  ) +
  annotate("segment", x = 1, xend = 2, y = ra_y, yend = ra_y) +
  annotate("segment", x = 1, xend = 1, y = ra_y - 0.008, yend = ra_y) +
  annotate("segment", x = 2, xend = 2, y = ra_y - 0.008, yend = ra_y) +
  annotate(
    "text", x = 1.5, y = ra_y + 0.025,
    label = sprintf("Diff. = %.3f%s", ra_difference, sig_mark(ra_test$p.value)),
    size = 5
  ) +
  scale_fill_manual(values = c("Lower CCEI" = "#D99A99", "Higher CCEI" = "#80ADD0")) +
  scale_x_discrete(labels = c("Lower\nCCEI", "Higher\nCCEI")) +
  scale_y_continuous(limits = c(0, ra_y + 0.06), expand = c(0, 0)) +
  labs(
    x = NULL,
    y = expression("Mean risk-aversion distance d(" * RA[i] * "," * RA[g] * ")")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 28),
    plot.background = element_rect(fill = "white", colour = NA)
  )

ra_cdf <- ggplot(
  ra_data,
  aes(RA_I_ig, colour = ccei_group, linetype = ccei_group)
) +
  stat_ecdf(geom = "step", linewidth = 0.8, pad = FALSE) +
  scale_colour_manual(values = c("Lower CCEI" = "red", "Higher CCEI" = "blue")) +
  scale_linetype_manual(values = c("Lower CCEI" = "dashed", "Higher CCEI" = "solid")) +
  scale_x_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
  ) +
  labs(
    x = expression("Risk-aversion distance d(" * RA[i] * "," * RA[g] * ")"),
    y = "Cumulative probability",
    colour = NULL,
    linetype = NULL
  ) +
  cdf_theme

ggsave(
  file.path(result_dir, "ra_distance_by_ccei_bar.png"),
  ra_bar, width = 6, height = 5, dpi = 300
)
ggsave(
  file.path(result_dir, "ra_distance_by_ccei_cdf.png"),
  ra_cdf, width = 6, height = 5, dpi = 300
)

################################################################################
# Figure A10: Multinomial-Logit Individual Rationality And Joint Outcomes
################################################################################
# Run this section from Code after the collective-outcomes section in 5.
if (!exists("code_dir")) code_dir <- getwd()
.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(ggplot2)
source(file.path(code_dir, "programs/plot_collective_ame.R"))
sample <- haven::read_dta(file.path(code_dir, "data/collective_analysis_sample.dta"))
member_sd <- c(maximum = sd(sample$ccei_max), minimum = sd(sample$ccei_min))
collective_result_dir <- file.path(code_dir, "results/collective_quality")
collective_figure_dir <- file.path(code_dir, "results/figures/collective_rationality")
dir.create(collective_figure_dir, recursive = TRUE, showWarnings = FALSE)
ame_files <- plot_cei_ame(file.path(collective_result_dir, "joint_outcome_ame.csv"),
                         collective_figure_dir, classification = "a", portrait = TRUE,
                         member_sd = member_sd)
draft_figure_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(draft_figure_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(ame_files, file.path(draft_figure_dir, basename(ame_files)), overwrite = TRUE))

################################################################################
# Figure A11: Alternative Rationality And Joint Collective Outcomes
# Run this section from Code after the alternative-index section in 5.
################################################################################
if (!exists("code_dir")) code_dir <- getwd()
.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(ggplot2)
source(file.path(code_dir, "programs/plot_collective_ame.R"))
out <- file.path(code_dir, "results/collective_quality/extensions")
figure_dir <- file.path(code_dir, "results/figures/collective_rationality")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
# Alternative-index appendix panels use Figure 8's LPM specifications and style.
a <- read.csv(file.path(out, "alternative_linear_ame.csv"))
stopifnot(nrow(a) == 48, setequal(a$measure, c("HM", "MaxMPI")))
sample <- haven::read_dta(file.path(code_dir, "data/collective_analysis_sample.dta"))
indices <- list(HM = cbind(1 - sample$hm_i / 18, 1 - sample$hm_j / 18),
                MaxMPI = cbind(1 - sample$maxmpi_i, 1 - sample$maxmpi_j))
labels <- c(HM = "HM index", MaxMPI = "RevMaxMPI")
alternative_files <- character()
units <- data.frame()
for (measure in names(indices)) {
  scores <- indices[[measure]]
  member_sd <- c(maximum = sd(pmax(scores[, 1], scores[, 2])),
                 minimum = sd(pmin(scores[, 1], scores[, 2])))
  units <- rbind(units, data.frame(measure = measure, member = names(member_sd),
                                  sd = unname(member_sd), n = nrow(sample)))
  input <- tempfile(fileext = ".csv")
  write.csv(a[a$measure == measure, ], input, row.names = FALSE)
  alternative_files <- c(alternative_files,
    plot_cei_ame(input, figure_dir, classification = "a", portrait = TRUE,
                 prefix = paste0("alternative_", measure, "_lpm"),
                 member_sd = member_sd, consistency_label = labels[[measure]]))
  unlink(input)
}
write.csv(units, file.path(out, "alternative_sd_units.csv"), row.names = FALSE)
draft_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(alternative_files, file.path(draft_dir, basename(alternative_files)), overwrite = TRUE))

################################################################################
# Figure A12: Joint-Outcome Overall-R2 Decomposition
# Run this section from Code after the collective-outcomes section in 5.
################################################################################
if (!exists("code_dir")) code_dir <- getwd()
out <- file.path(code_dir, "results/collective_quality/extensions")
figure_dir <- file.path(code_dir, "results/figures/collective_rationality")
draft_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(draft_dir, recursive = TRUE, showWarnings = FALSE)

# Figure A12 follows A7: overall R2, with fixed effects as a separate block.
plot_joint_outcome_shapley <- function(input_dir = out, output_dir = figure_dir) {
  d <- read.csv(file.path(input_dir, "joint_outcome_shapley_r2.csv"))
  fits <- read.csv(file.path(input_dir, "joint_outcome_shapley_models.csv"))
  previous <- read.csv(file.path(code_dir, "results/collective_quality/joint_outcome_linear_ame.csv"))
  stopifnot(nrow(d) == 40, nrow(fits) == 8, all(d$n == 1304),
            all(d$pairs == 652), all(d$clusters == 64), all(d$shapley_value >= -1e-10))
  for (member in c("maximum", "minimum")) {
    check <- merge(fits, previous[previous$member == member, ], by = c("specification", "outcome"))
    stopifnot(nrow(check) == 8,
              all(abs(check[[paste0(member, "_beta")]] - check$estimate) < 1e-12),
              all(abs(check[[paste0(member, "_se")]] - check$se) < 5e-9))
  }

  # Borrow A7's exact overall-R2 decomposition, colours, hatching, and leader labels.
  blocks <- c("Individual CCEI", "Student characteristics", "Friendship measures",
              "Corner/Midpoint shares", "Fixed effects")
  fills <- c("#7FB2D8", adjustcolor("#D94B4B", alpha.f = .55), "#E4E4E4",
             adjustcolor("#D94B4B", alpha.f = .8), "#C8C8C8")
  outcome_labels <- c("CCEI < 1\nCEIV < 1", "CCEI = 1\nCEIV < 1",
                      "CCEI < 1\nCEIV = 1", "CCEI = 1\nCEIV = 1")
  specifications <- c("full_controls", "pair_fe")
  titles <- c("(a) Class FE + controls", "(b) Pair FE + controls")
  ymax <- ceiling(max(d$total_r2) / .1) * .1
  centres <- c(1.00, 3.55, 6.10, 8.65)

  draw_panel <- function(specification, title) {
    par(mar = c(3.6, 4.3, 2.3, .5), family = "sans")
    plot.new()
    plot.window(xlim = c(0, 10.4), ylim = c(0, ymax), xaxs = "i", yaxs = "i")
    ticks <- seq(0, ymax, .1)
    abline(h = ticks, col = "#ECECEC", lwd = 1)
    axis(2, at = ticks, labels = sprintf("%.1f", ticks), las = 1,
         col = NA, col.axis = "#555555", cex.axis = .95)
    axis(1, at = centres, labels = outcome_labels, tick = FALSE, line = .1,
         col.axis = "black", cex.axis = .95)
    mtext(expression(R^2 ~ "Contribution"), side = 2, line = 2.8, cex = .95)
    title(main = title, cex.main = 1.1, font.main = 1, line = .8)
    for (outcome in 1:4) {
      bar <- d[d$specification == specification & d$outcome == outcome, ]
      bar <- bar[match(rev(blocks), bar$block), ]
      stopifnot(!anyNA(bar$block), abs(sum(bar$shapley_value) - bar$total_r2[1]) < 1e-10,
                abs(sum(bar$shapley_percent) - 100) < 1e-8)
      bottom <- c(0, head(cumsum(bar$shapley_value), -1))
      top <- cumsum(bar$shapley_value)
      middle <- (bottom + top) / 2
      values <- ifelse(bar$shapley_value < .0005, "<0.001", sprintf("%.3f", bar$shapley_value))
      percentages <- ifelse(bar$shapley_percent < .05, "<0.1", sprintf("%.1f", bar$shapley_percent))
      labels <- sprintf("%s (%s%%)", values, percentages)
      centre <- centres[outcome]
      left <- centre - .55
      right <- centre + .55
      for (row in seq_len(nrow(bar))) {
        block <- match(bar$block[row], blocks)
        rect(left, bottom[row], right, top[row], col = fills[block], border = "#333333", lwd = 1,
             density = if (block == 4) 18 else NULL, angle = 45)
      }
      inside <- bar$shapley_value >= .035
      text(centre, middle[inside], labels[inside], cex = .78)
      small <- which(!inside)
      if (length(small)) {
        label_y <- middle[small]
        label_y[1] <- max(label_y[1], .025)
        if (length(small) > 1) {
          for (k in 2:length(small)) label_y[k] <- max(label_y[k], label_y[k - 1] + .029)
        }
        label_y <- label_y - max(0, max(label_y) - ymax + .028)
        segments(right, middle[small], right + .16, label_y, col = "#777777", lwd = .7)
        text(right + .20, label_y, labels[small], adj = 0, cex = .75)
      }
      text(centre, bar$total_r2[1] + .021, sprintf("R² = %.3f", bar$total_r2[1]), cex = .8)
    }
  }

  draw_legend <- function() {
    par(mar = c(0, 0, 0, 0), family = "sans")
    plot.new()
    order <- c(1, 4, 2, 5, 3)
    legend("center", legend = c("Individual CCEI", "Student characteristics", "Friendship measures",
                                "Corner / midpoint shares", "Fixed effects")[order],
           fill = fills[order], border = "#333333", density = c(NA, NA, NA, 18, NA)[order],
           angle = 45, bty = "n", ncol = 3, cex = .9)
  }

  draw_figure <- function(show_title = FALSE) {
    par(oma = c(0, 0, if (show_title) 1.8 else 0, 0))
    layout(matrix(c(1, 2, 3), ncol = 1), heights = c(1, 1, .17))
    for (panel in seq_along(specifications)) draw_panel(specifications[panel], titles[panel])
    draw_legend()
    if (show_title) mtext("Explained Variation in Joint Collective Outcomes", outer = TRUE,
                          side = 3, line = .3, cex = 1.2)
  }
  files <- file.path(output_dir, paste0("joint_outcome_shapley.", c("png", "pdf")))
  png(files[1], width = 3900, height = 2850, res = 300, pointsize = 14)
  draw_figure(show_title = TRUE)
  dev.off()
  # Use larger type at manuscript width; LaTeX supplies the overall caption.
  pdf(files[2], width = 13, height = 9.5, pointsize = 20, useDingbats = FALSE)
  draw_figure()
  dev.off()
  files
}
shapley_files <- plot_joint_outcome_shapley()
stopifnot(file.copy(shapley_files, file.path(draft_dir, basename(shapley_files)), overwrite = TRUE))

################################################################################
# Supplementary historical Shapley decompositions
################################################################################

make_shapley_plot <- function(
    input_file,
    output_file,
    block_order,
    fill_values,
    pattern_values,
    pattern_angles) {
  if (!file.exists(input_file)) {
    warning(
      "Skipping Shapley figure because ", input_file,
      " does not exist. Allow 5_Tables_Appendix.do to finish first."
    )
    return(invisible(FALSE))
  }

  if (!requireNamespace("ggpattern", quietly = TRUE)) {
    warning("Skipping legacy Shapley plot: ggpattern is not installed.")
    return(invisible(FALSE))
  }

  shapley <- read.csv(input_file, stringsAsFactors = FALSE)
  required <- c("block", "shapley_value", "shapley_percent", "total_r2")
  missing <- setdiff(required, names(shapley))
  if (length(missing) > 0) {
    stop("Missing Shapley columns: ", paste(missing, collapse = ", "))
  }

  shapley$block <- factor(shapley$block, levels = block_order)
  shapley$label <- sprintf(
    "%.3f (%.1f%%)",
    shapley$shapley_value,
    shapley$shapley_percent
  )

  plot <- ggplot(
    shapley,
    aes(
      x = "",
      y = shapley_value,
      fill = block,
      pattern = block,
      pattern_angle = block
    )
  ) +
    ggpattern::geom_col_pattern(
      width = 0.50,
      colour = "black",
      linewidth = 0.45,
      pattern_spacing = 0.045,
      pattern_density = 0.35,
      pattern_fill = "white",
      pattern_colour = "white"
    ) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 4
    ) +
    scale_fill_manual(
      name = "Block",
      values = fill_values,
      breaks = block_order,
      drop = FALSE
    ) +
    scale_pattern_manual(
      name = "Block",
      values = pattern_values,
      breaks = block_order,
      drop = FALSE
    ) +
    scale_pattern_angle_manual(values = pattern_angles, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = expression(R^2 ~ "Contribution")) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "right",
      plot.margin = margin(10, 20, 10, 10)
    )

  ggsave(
    output_file,
    plot,
    width = 6,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  invisible(TRUE)
}

bargaining_order <- c(
  "CCEI",
  "Individual/Friendship",
  "Risk Aversion",
  "Corner/Midpoint Shares",
  "Individual FE"
)
make_shapley_plot(
  file.path(result_dir, "shapley_bargaining_index.csv"),
  file.path(result_dir, "shapley_bargaining_index.png"),
  bargaining_order,
  c(
    "CCEI" = "white",
    "Individual/Friendship" = "#B7D5E1",
    "Risk Aversion" = "#A9E891",
    "Corner/Midpoint Shares" = "#F2C6CF",
    "Individual FE" = "#C8C8C8"
  ),
  c(
    "CCEI" = "none",
    "Individual/Friendship" = "stripe",
    "Risk Aversion" = "stripe",
    "Corner/Midpoint Shares" = "stripe",
    "Individual FE" = "none"
  ),
  c(
    "CCEI" = 0,
    "Individual/Friendship" = -45,
    "Risk Aversion" = 45,
    "Corner/Midpoint Shares" = 0,
    "Individual FE" = 0
  )
)

collective_order <- c(
  "CCEI",
  "Group/Friendship",
  "Risk Aversion",
  "Corner/Midpoint Shares",
  "Pair FE"
)
make_shapley_plot(
  file.path(result_dir, "shapley_collective_ccei.csv"),
  file.path(result_dir, "shapley_collective_ccei.png"),
  collective_order,
  c(
    "CCEI" = "white",
    "Group/Friendship" = "#B7D5E1",
    "Risk Aversion" = "#A9E891",
    "Corner/Midpoint Shares" = "#F2C6CF",
    "Pair FE" = "#C8C8C8"
  ),
  c(
    "CCEI" = "none",
    "Group/Friendship" = "stripe",
    "Risk Aversion" = "stripe",
    "Corner/Midpoint Shares" = "stripe",
    "Pair FE" = "none"
  ),
  c(
    "CCEI" = 0,
    "Group/Friendship" = -45,
    "Risk Aversion" = 45,
    "Corner/Midpoint Shares" = 0,
    "Pair FE" = 0
  )
)

################################################################################
# Leave-one-choice-out review: individual-fit intervals and Table 3 comparison.
# Run after all 18 folds have been estimated by the final section of 5.
################################################################################
holdout_dir <- Sys.getenv("LOCO_OUTPUT_DIR", "results/tests/disjoint_choice")
inference_file <- file.path(holdout_dir, "focal_fit_inference.csv")
if (file.exists(inference_file)) {
  fits <- read.csv(inference_file)
  if (nrow(fits) == 108L) {
    reference <- read.csv(file.path(code_dir, "results/tables/table3_focal_estimates.csv"))
    stopifnot(all(table(fits$column) == 18L), !anyDuplicated(fits[c("fold", "column")]),
              all(is.finite(fits$coefficient)), all(fits$se > 0),
              all(fits$df_r == fits$clusters-1), all(reference$N == 2560))
    comparison <- do.call(rbind, lapply(1:6, function(column) {
      d <- fits[fits$column == column, ]
      actual <- reference[reference$specification == column, ]
      data.frame(column = column, table3_coefficient = actual$actual_coefficient,
        table3_se = actual$actual_se, heldout_median = median(d$coefficient),
        heldout_min = min(d$coefficient), heldout_max = max(d$coefficient),
        negative_folds = sum(d$coefficient < 0), significant_5pct_folds = sum(d$p < .05),
        median_n = median(d$n), min_n = min(d$n), max_n = max(d$n),
        min_clusters = min(d$clusters), max_clusters = max(d$clusters))
    }))
    write.csv(comparison, file.path(holdout_dir, "coefficient_stability.csv"), row.names = FALSE)
    fits$low <- fits$coefficient-qt(.975, fits$df_r)*fits$se
    fits$high <- fits$coefficient+qt(.975, fits$df_r)*fits$se
    titles <- c("(1) Higher CCEI: class FE", "(2) Higher CCEI: controls, class FE",
                "(3) Higher CCEI: controls, individual FE", "(4) CCEI gap: class FE",
                "(5) CCEI gap: controls, class FE", "(6) CCEI gap: controls, individual FE")
    fits$panel <- factor(fits$column, levels = 1:6, labels = titles)
    comparison$panel <- factor(comparison$column, levels = 1:6, labels = titles)
    p <- ggplot(fits, aes(fold, coefficient)) +
      geom_hline(yintercept = 0, colour = "grey65", linewidth = .3) +
      geom_hline(data = comparison, aes(yintercept = table3_coefficient), colour = "red", linewidth = .5) +
      geom_errorbar(aes(ymin = low, ymax = high), width = .2, colour = "grey55") +
      geom_point(colour = "#1F4E79", size = 1.7) +
      facet_wrap(~panel, ncol = 3, scales = "free_y") +
      scale_x_continuous(breaks = c(1, 6, 12, 18)) +
      labs(x = "Held-out fold", y = "Focal coefficient",
        caption = "Bars: 95% class-clustered intervals for each separate fit. Red: full-choice Table 3 coefficient.\nFolds overlap; their variation is descriptive and is not independent-sample inference.") +
      theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
    ggsave(file.path(holdout_dir, "fold_coefficient_stability.png"), p,
           width = 13, height = 7, dpi = 240, bg = "white")
  }
}

message("6_Figures_Appendix.R completed. Outputs: ", result_dir)
