################################################################################
# 99_Figures_Appendix.R
# Appendix figures only. Existing 99_* files are intentionally left unchanged.
#
# Run 99_Tables_Appendix.do first so the two Shapley CSV inputs exist.
################################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(haven)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ggpattern)
})

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

# Minseon/Dropbox version (kept for reference):
# replication_dir <- "C:/Users/minseonp/Dropbox/RP/Code_Replication Package_Upload"

# Byunghun/current repository:
replication_dir <- normalizePath(
  file.path(code_dir, "..", "Code_Replication Package_Upload"),
  mustWork = TRUE
)
data_dir <- file.path(replication_dir, "data")
result_dir <- file.path(code_dir, "results")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

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

ggsave(
  file.path(result_dir, "figure_individual_ccei_cdf_baseline_endline.png"),
  individual_ccei_cdf, width = 7, height = 5, dpi = 300
)
ggsave(
  file.path(result_dir, "figure_group_ccei_cdf_baseline_endline.png"),
  group_ccei_cdf, width = 7, height = 5, dpi = 300
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

ggsave(
  file.path(result_dir, "figure_individual_ra_cdf_baseline_endline.png"),
  individual_ra_cdf, width = 7, height = 5, dpi = 300
)
ggsave(
  file.path(result_dir, "figure_group_ra_cdf_baseline_endline.png"),
  group_ra_cdf, width = 7, height = 5, dpi = 300
)

################################################################################
# Figure A6: Histogram of the Revealed Bargaining Index
################################################################################

hist_bargaining_index <- panel_individual |>
  filter(!is.na(I_ig)) |>
  ggplot(aes(I_ig)) +
  geom_histogram(
    binwidth = 0.05,
    boundary = 0,
    closed = "left",
    fill = "#4C78A8",
    colour = "white",
    linewidth = 0.25
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    x = expression("Revealed bargaining index (" * I[ig] * ")"),
    y = "Count"
  ) +
  theme_classic(base_size = 13) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave(
  file.path(result_dir, "hist_bargaining_index.png"),
  hist_bargaining_index, width = 6, height = 4.5, dpi = 300
)

################################################################################
# Figure A8: CDFs of absolute risk-preference distance, by wave
################################################################################

make_absolute_ra_distance_cdf <- function(data, wave_value) {
  plot_data <- data |>
    filter(
      post == wave_value,
      !is.na(RA_i),
      !is.na(RA_g),
      !is.na(HighCCEI)
    ) |>
    mutate(
      absolute_RA_distance = abs(RA_i - RA_g),
      rationality_group = factor(
        if_else(HighCCEI == 1, "High Rationality", "Low Rationality"),
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
# Figure A9: Normalized Risk-Aversion Distance By Members' CCEI
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
      if_else(HighCCEI == 1, "Higher CCEI", "Lower CCEI"),
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

ra_test_data <- ra_data |>
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
# Figures A8 and A10: Shapley decompositions
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
      " does not exist. Allow 999_3_Tables_Appendix.do to finish first."
    )
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
    geom_col_pattern(
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

message("99_4_Figures_Appendix.R completed. Outputs: ", result_dir)
