################################################################################
# 99_10_Figures_Appendix_Backup.R: Plots omitted from the active draft.
# Moved from 6_Figures_Appendix.R on 2026-10-11; run from Code.
# Retains risk-distance plots, legacy Shapley plots, hold-out review and the
# A7/A8/A10 review previews. Run 5/6 first for current figure estimates.
# This backup writes review results only and does not copy assets to Overleaf.
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
      " does not exist. Allow 99_9_Tables_Appendix_Backup.do to finish first."
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
  file.path(result_dir, "tables/shapley_bargaining_index.csv"),
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
  file.path(result_dir, "tables/shapley_collective_ccei.csv"),
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
# Run after all 18 folds have been estimated by the final section of 99_9.
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

################################################################################
# Review composites for the retained appendix figures; no manuscript exports.
################################################################################
source(file.path(code_dir, "programs/plot_table3_shapley.R"))
plot_table3_shapley(code_dir,
  preview_file = file.path(result_dir, "figure_A7_review/table3_shapley.png"),
  export_panels = FALSE)
source(file.path(code_dir, "programs/plot_table3_placebo.R"))
export_table3_placebo_panels(code_dir, c(5L, 6L), "figure_A8_placebo_coefficients.png",
  input_prefix = "figureA8_placebo", export_panels = FALSE)
source(file.path(code_dir, "programs/plot_joint_outcome_shapley.R"))
plot_joint_outcome_shapley(code_dir,
  preview_file = file.path(figure_dir, "collective_rationality/joint_outcome_shapley.png"),
  export_pdf = FALSE)
message("99_10_Figures_Appendix_Backup.R completed. Review outputs: ", result_dir)
