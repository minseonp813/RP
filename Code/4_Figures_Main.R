################################################################################
# 4_Figures_Main.R: Locked main Figures 3--8.
# Latest update: 2026-10-11
# Figures 1/2 are drawn directly in the manuscript's LaTeX.
# Run 3_Tables_Main.do first. This file writes only the main manuscript panels
# and their supporting estimates/counts to results/figures and collective_quality.
# Figure 4 estimates 500 reassignments for Table 3 Columns (2)/(3) only.
# Figure 9 remains in the separate ML workflow.
# All manuscript assets are copied to their existing Overleaf paths.
# Run from Code, or source this file from RStudio.
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
})

data_dir <- file.path(code_dir, "data")
result_dir <- file.path(code_dir, "results")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
figure_dir <- file.path(result_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

panel_individual <- read_dta(file.path(data_dir, "panel_individual.dta"))

sig_mark <- function(p) {
  case_when(
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.10 ~ "+",
    TRUE ~ ""
  )
}

paper_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA)
  )

################################################################################
# Figure 3: Revealed Preference Distance By Members' CCEI
# I-M means/CDFs use all 2,560 defined student-waves, ties assigned High,
# class-adjusted category means with normal SD/sqrt(n) intervals,
# class-clustered CR1 contrasts and ordinary two-sample KS tests.
# This section runs independently after loading haven, ggplot2 and dplyr,
# and defining code_dir, result_dir, figure_dir, paper_theme and sig_mark above.
################################################################################

source(file.path(code_dir, "programs/class_fe_category_summary.R"))
distance_panel <- read_dta(file.path(code_dir,
  "data/panel_individual.dta"))
stopifnot(nrow(distance_panel) == 2608, all(distance_panel$n_ccei_donors == 651),
          !anyDuplicated(distance_panel[c("id", "post")]))
distance_panel$member <- factor(ifelse(distance_panel$HighCCEI_both_high == 1,
  "Higher CCEI", "Lower CCEI"), levels = c("Lower CCEI", "Higher CCEI"))

adjusted_distance_plots <- function(data, value, mean_label, distance_label) {
  summary <- class_fe_category_summary(data, value, "member", "Lower CCEI")
  stats <- rename(summary$stats, member = category)
  mean_fit <- summary$fit
  data$class_adjusted <- summary$adjusted
  clusters <- length(unique(data$class))
  df <- clusters - 1L
  stopifnot(clusters == 64L)
  contrast <- summary$category_X["Higher CCEI", ] - summary$category_X["Lower CCEI", ]
  difference_se <- sqrt(drop(t(contrast) %*% vcov(mean_fit) %*% contrast))
  ks <- suppressWarnings(stats::ks.test(
    data$class_adjusted[data$member == "Lower CCEI"],
    data$class_adjusted[data$member == "Higher CCEI"],
    alternative = "two.sided", exact = FALSE))
  difference <- sum(contrast * coef(mean_fit))
  difference_p <- 2 * pt(-abs(difference / difference_se), df)
  y_min <- min(0, min(stats$low) - .02)
  y_max <- max(0, max(stats$high) + .04)
  bar <- ggplot(stats, aes(member, mean, fill = member)) +
    geom_col(width = .62, colour = "black", linewidth = .3) +
    geom_errorbar(aes(ymin = low, ymax = high), width = .16) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = .4) +
    annotate("text", x = 1.5, y = y_max,
      label = sprintf("High - low = %.3f%s", difference, sig_mark(difference_p)), size = 4.4) +
    scale_fill_manual(values = c("Lower CCEI" = "#D99A99", "Higher CCEI" = "#80ADD0")) +
    scale_y_continuous(breaks = seq(-.05, .10, .05)) +
    coord_cartesian(ylim = c(y_min, y_max + .015)) +
    labs(x = NULL, y = mean_label) + paper_theme +
    theme(legend.position = "none", axis.title.y = element_text(size = 13))
  cdf <- ggplot(data, aes(class_adjusted, colour = member, linetype = member)) +
    stat_ecdf(geom = "step", linewidth = .85, pad = FALSE) +
    annotate("text", x = .92, y = .12, hjust = 1, size = 4.4,
      label = sprintf("KS: D = %.3f\n%s", ks$statistic,
        if (ks$p.value < .001) "p < 0.001" else sprintf("p = %.3f", ks$p.value))) +
    geom_vline(xintercept = 0, colour = "grey65", linewidth = .4) +
    scale_colour_manual(values = c("Lower CCEI" = "red", "Higher CCEI" = "blue")) +
    scale_linetype_manual(values = c("Lower CCEI" = "dashed", "Higher CCEI" = "solid")) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .25), expand = c(0, 0)) +
    labs(x = distance_label, y = "Cumulative probability", colour = NULL, linetype = NULL) +
    paper_theme + theme(legend.position = "inside", legend.position.inside = c(.2, .84),
      plot.margin = margin(5.5, 14, 5.5, 5.5),
      legend.background = element_rect(fill = "white", colour = "black"))
  stats$high_minus_low <- difference
  stats$difference_se <- difference_se
  stats$difference_p <- difference_p
  stats$pooled_sd <- sd(data[[value]])
  stats$difference_pct_sd <- 100 * abs(difference) / stats$pooled_sd
  stats$cdf_D <- unname(ks$statistic)
  stats$cdf_p <- ks$p.value
  stats$difference_inference <- "Class-FE contrast, class-clustered CR1, t(63)"
  stats$cdf_inference <- "Asymptotic two-sample KS; adjusted outcomes treated as observed"
  list(bar = bar, cdf = cdf, stats = stats)
}

actual_distance_data <- filter(distance_panel, !is.na(Istar_ccei))
stopifnot(nrow(actual_distance_data) == 2560)
figure3 <- adjusted_distance_plots(actual_distance_data, "Istar_ccei",
  "Mean Placebo-adjusted Revealed Preference Distance",
  "Placebo-adjusted Revealed Preference Distance")
ggsave(file.path(figure_dir, "ccei_IminusM_by_higher_ccei_bar.png"), figure3$bar,
       width = 6, height = 5, dpi = 300)
ggsave(file.path(figure_dir, "ccei_IminusM_by_higher_ccei_cdf.png"), figure3$cdf,
       width = 6, height = 5, dpi = 300)
write.csv(figure3$stats, file.path(figure_dir, "ccei_IminusM_by_higher_ccei_stats.csv"),
          row.names = FALSE)
for (panel in c("bar", "cdf")) {
  filename <- sprintf("ccei_IminusM_by_higher_ccei_%s.png", panel)
  stopifnot(file.copy(file.path(figure_dir, filename),
    file.path(code_dir, "../Overleaf/figures_2025", filename), overwrite = TRUE))
}

################################################################################
# Figure 4: M-controlled placebo regressions on Table 3's full 2,560-row sample.
################################################################################
source(file.path(code_dir, "programs/estimate_table3_placebo.R"))
estimate_table3_placebo(code_dir, c(2L, 3L), "figure4_placebo")
source(file.path(code_dir, "programs/plot_table3_placebo.R"))
export_table3_placebo_panels(code_dir, c(2L, 3L))

################################################################################
# Figure 5: Self-Reported Influence And Revealed-Preference Distance
# Istar_ccei = I_ccei - M_ccei uses the same validated panel as Figure 3.
################################################################################

figure5_data <- distance_panel |>
  filter(post %in% c(0, 1)) |>
  mutate(
    RA_difference = abs(RA_i - RA_j)
  )

survey_bar <- function(data, outcome_var, survey_var, labels) {
  plot_data <- data |>
    filter(
      !is.na(.data[[outcome_var]]),
      .data[[survey_var]] %in% seq_along(labels)
    ) |>
    group_by(response = .data[[survey_var]]) |>
    summarise(
      mean = mean(.data[[outcome_var]], na.rm = TRUE),
      sd = sd(.data[[outcome_var]], na.rm = TRUE),
      n = sum(!is.na(.data[[outcome_var]])),
      .groups = "drop"
    ) |>
    mutate(
      se = sd / sqrt(n),
      low = mean - qt(0.975, n - 1) * se,
      high = mean + qt(0.975, n - 1) * se,
      percent = 100 * n / sum(n),
      axis_label = paste0(
        labels[as.character(response)],
        "\n(N=", n, ", ", sprintf("%.1f", percent), "%)"
      )
    )

  fill_values <- setNames(
    rep("lightblue", nrow(plot_data)),
    as.character(plot_data$response)
  )
  if (identical(survey_var, "risk_whose_i") && "4" %in% names(fill_values)) {
    fill_values["4"] <- "grey90"
  }
  ggplot(plot_data, aes(factor(response), mean, fill = factor(response))) +
    geom_col(width = 0.85) +
    geom_errorbar(aes(ymin = low, ymax = high), width = 0.15) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.4) +
    scale_x_discrete(labels = setNames(plot_data$axis_label, plot_data$response)) +
    scale_y_continuous(
      limits = c(-0.2, 0.2),
      breaks = seq(-0.2, 0.2, 0.1),
      minor_breaks = seq(-0.15, 0.15, 0.1)
    ) +
    scale_fill_manual(values = fill_values) +
    labs(
      x = NULL,
      y = expression("Mean placebo-adjusted distance " * (I-M))
    ) +
    paper_theme +
    theme(
      legend.position = "none",
      panel.grid.minor.y = element_line(colour = "grey92", linewidth = 0.3)
    )
}

whose_labels <- c(
  "1" = "Mostly Partner's",
  "2" = "Both",
  "3" = "Mostly Mine",
  "4" = "Neither"
)
similar_labels <- c(
  "1" = "Very\nDifferently",
  "2" = "Somewhat\nDifferently",
  "3" = "Somewhat\nSimilar",
  "4" = "Mostly\nSimilar"
)

make_figure5_pair <- function(outcome_var, output_dir) {
  pooled_full <- figure5_data |>
    filter(!is.na(.data[[outcome_var]]))

  pooled_ra_median <- median(pooled_full$RA_difference, na.rm = TRUE)
  pooled_high_ra <- pooled_full |>
    filter(
      !is.na(RA_difference),
      RA_difference >= pooled_ra_median
    )

  whose_plot <- survey_bar(
    pooled_full,
    outcome_var,
    "risk_whose_i",
    whose_labels
  )
  similar_plot <- survey_bar(
    pooled_high_ra,
    outcome_var,
    "risk_similar_i",
    similar_labels
  )

  ggsave(
    file.path(
      output_dir,
      "ccei_bargaining_whose_suggestion.png"
    ),
    whose_plot,
    width = 7,
    height = 5,
    dpi = 300
  )
  ggsave(
    file.path(
      output_dir,
      "ccei_bargaining_had_individual_high.png"
    ),
    similar_plot,
    width = 7,
    height = 5,
    dpi = 300
  )
}

make_figure5_pair("Istar_ccei", figure_dir)
for (filename in c("ccei_bargaining_whose_suggestion.png",
                   "ccei_bargaining_had_individual_high.png")) {
  stopifnot(file.copy(file.path(figure_dir, filename),
    file.path(code_dir, "../Overleaf/figures_2025", filename), overwrite = TRUE))
}

################################################################################
# Figure 6: Joint Collective Outcomes
################################################################################

# Saved inputs are generated by the Figure 8 section in 3.
# To run this section alone, load haven/ggplot2/dplyr and set code_dir.
collective_result_dir <- file.path(code_dir, "results/collective_quality")
collective_figure_dir <- file.path(code_dir, "results/figures/collective_rationality")
dir.create(collective_figure_dir, recursive = TRUE, showWarnings = FALSE)
joint_data <- read_dta(file.path(code_dir, "data/collective_analysis_sample.dta"))
stopifnot(nrow(joint_data) == 1304, length(unique(joint_data$group_id)) == 652,
          !anyDuplicated(joint_data[c("group_id", "post")]),
          all(is.finite(as.matrix(joint_data[c("ccei_g", "ceiv_g", "ceiv_at1")]))),
          all(joint_data$ceiv_at1 %in% 0:1),
          all(joint_data$ceiv_g[joint_data$ceiv_at1 == 1] == 1))
joint_category <- 1L + (joint_data$ccei_g >= 1 - 1e-9) + 2L * joint_data$ceiv_at1
stopifnot(all(joint_category == joint_data$joint_category),
          all(joint_data$ceiv_g >= 0 & joint_data$ceiv_g <= 1))
joint_counts <- data.frame(outcome = 1:4, ccei_one = c(0, 1, 0, 1), ceiv_one = c(0, 0, 1, 1),
                     n = tabulate(joint_category, nbins = 4))
joint_counts$share <- joint_counts$n / nrow(joint_data)
joint_ames <- read.csv(file.path(collective_result_dir, "joint_outcome_linear_ame.csv"))
stopifnot(sum(joint_counts$n) == 1304,
          max(abs(joint_ames$share - joint_counts$share[joint_ames$outcome])) < 1e-12)
write.csv(joint_counts, file.path(collective_result_dir, "joint_outcome_counts.csv"), row.names = FALSE)
plot_joint_outcomes <- function(data, counts) {
  # Count coincident observations within each endpoint category; keep exact coordinates.
  points <- data |>
    count(ccei_g, ceiv_g, joint_category, name = "n") |>
    arrange(joint_category)
  stopifnot(sum(points$n) == nrow(data))
  points$outcome <- factor(points$joint_category, levels = counts$outcome)
  legend_labels <- sprintf("%s: %s (%.1f%%)",
    c("Both < 1", "CCEI = 1 only", "CEIV = 1 only", "Both = 1"),
    format(counts$n, big.mark = ",", trim = TRUE), 100 * counts$share)
  legend_title <- sprintf("Pair-waves (n = %s)", format(nrow(data), big.mark = ",", trim = TRUE))
  point_fill <- c("grey65", "#D73027", "#2C7BB6", "black")
  point_colour <- c("grey35", "grey15", "grey15", "black")
  point_size <- c(1.8, 2.1, 2.3, 4.5)
  point_alpha <- c(.55, 1, 1, .5)

  ggplot(points, aes(ccei_g, ceiv_g)) +
    annotate("segment", x = 0, y = 0, xend = 1, yend = 1,
             colour = "grey65", linewidth = .5) +
    geom_vline(xintercept = 1, colour = "grey35", linetype = "dashed", linewidth = .5) +
    geom_hline(yintercept = 1, colour = "grey35", linetype = "dashed", linewidth = .5) +
    geom_point(aes(fill = outcome, colour = outcome, shape = outcome,
                   size = n, alpha = outcome), stroke = .2) +
    annotate("text", x = .11, y = .14, label = "CEIV = CCEI", angle = 45,
             colour = "grey40", size = 5) +
    scale_fill_manual(name = legend_title, values = point_fill, labels = legend_labels,
                      drop = FALSE) +
    scale_shape_manual(name = legend_title, values = c(21, 21, 23, 22),
                       labels = legend_labels, drop = FALSE) +
    scale_colour_manual(values = point_colour, guide = "none") +
    scale_size(range = c(1.4, 10), guide = "none") +
    scale_alpha_manual(values = point_alpha, guide = "none") +
    guides(fill = guide_legend(override.aes = list(size = point_size, alpha = point_alpha,
                                                 colour = point_colour))) +
    scale_x_continuous(breaks = seq(0, 1, .2), labels = function(x) sprintf("%.1f", x),
                       limits = c(-.05, 1.05), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 1, .2), labels = function(x) sprintf("%.1f", x),
                       limits = c(-.05, 1.05), expand = c(0, 0)) +
    coord_fixed() +
    labs(x = "Group CCEI", y = "Group CEIV") +
    theme_minimal(base_size = 17) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "grey92", linewidth = .7),
          axis.text = element_text(size = 16, colour = "grey30"),
          axis.title = element_text(size = 18),
          axis.title.x = element_text(margin = margin(t = 12)),
          axis.title.y = element_text(margin = margin(r = 12)),
          legend.position = "inside", legend.position.inside = c(.955, .03),
          legend.justification = c(1, 0),
          legend.background = element_rect(fill = "white", colour = "black", linewidth = .5),
          legend.title = element_text(size = 15, margin = margin(b = 6)),
          legend.text = element_text(size = 14),
          legend.key.height = grid::unit(.52, "cm"),
          legend.margin = margin(6, 7, 6, 7),
          plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(12, 12, 12, 12))
}
joint_plot <- plot_joint_outcomes(joint_data, joint_counts)
write.csv(select(joint_plot$data, ccei_g, ceiv_g, joint_category, n),
          file.path(collective_result_dir, "joint_outcome_point_counts.csv"), row.names = FALSE)
figure6_file <- file.path(collective_figure_dir, "joint_outcome_quadrants.pdf")
ggsave(figure6_file, joint_plot, width = 7, height = 6.3)
print(joint_counts)

draft_figure_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(draft_figure_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(figure6_file, file.path(draft_figure_dir, basename(figure6_file)),
                   overwrite = TRUE))

################################################################################
# Figure 7: Collective CCEI And CEIV By Members' Individual CCEI Category
################################################################################

# Saved inputs are generated by the Figure 8 section in 3.
# To run alone, load haven/ggplot2/dplyr, define paper_theme, sig_mark and
# set code_dir to Code. Class-FE adjustment and inference match Figure 3:
# normal SD/sqrt(n) bar CIs, class-clustered mean contrasts and ordinary KS tests.
source(file.path(code_dir, "programs/class_fe_category_summary.R"))
collective_result_dir <- file.path(code_dir, "results/collective_quality")
collective_figure_dir <- file.path(code_dir, "results/figures/collective_rationality")
dir.create(collective_figure_dir, recursive = TRUE, showWarnings = FALSE)
category_data <- read_dta(file.path(code_dir, "data/collective_analysis_sample.dta"))
stopifnot(nrow(category_data) == 1304, !anyDuplicated(category_data[c("group_id", "post")]),
          all(is.finite(as.matrix(category_data[c("ccei_min", "ccei_max", "ccei_g", "ceiv_g")]))))
category_pooled_median <- median(c(category_data$ccei_min, category_data$ccei_max))
stopifnot(!any(c(category_data$ccei_min, category_data$ccei_max) == category_pooled_median),
          max(abs(category_data$pooled_median - category_pooled_median)) < 1e-12,
          all(category_data$pair_category == (category_data$ccei_min > category_pooled_median) +
                                             (category_data$ccei_max > category_pooled_median)))
category_data$pair_category <- factor(category_data$pair_category, levels = 0:2,
                                      labels = c("Low-Low", "Low-High", "High-High"))
draft_figure_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(draft_figure_dir, recursive = TRUE, showWarnings = FALSE)

for (outcome in c("ccei", "ceiv")) {
  variable <- paste0(outcome, "_g")
  title <- toupper(outcome)
  category_summary <- class_fe_category_summary(category_data, variable, "pair_category", "Low-High")
  category_stats <- rename(category_summary$stats, pair_category = category)
  mean_fit <- category_summary$fit
  category_data$class_adjusted <- category_summary$adjusted
  clusters <- n_distinct(category_data$class)
  df <- clusters - 1L
  stopifnot(clusters == 64L)
  write.csv(category_stats, file.path(collective_result_dir, paste0("group_", outcome, "_category_means.csv")), row.names = FALSE)
  category_comparisons <- list(c("Low-Low", "Low-High"), c("Low-High", "High-High"), c("Low-Low", "High-High"))
  category_diffs <- list()
  for (i in seq_along(category_comparisons)) {
    categories <- category_comparisons[[i]]
    contrast <- category_summary$category_X[categories[2], ] - category_summary$category_X[categories[1], ]
    difference <- sum(contrast * coef(mean_fit))
    difference_se <- sqrt(drop(t(contrast) %*% vcov(mean_fit) %*% contrast))
    p <- 2 * pt(-abs(difference / difference_se), df)
    comparison_data <- filter(category_data, pair_category %in% categories)
    ks <- suppressWarnings(stats::ks.test(
      comparison_data$class_adjusted[comparison_data$pair_category == categories[1]],
      comparison_data$class_adjusted[comparison_data$pair_category == categories[2]],
      alternative = "two.sided", exact = FALSE))
    category_diffs[[i]] <- data.frame(lower = categories[1], upper = categories[2], difference = difference,
               se = difference_se, p = p, clusters = clusters, df = df,
               label = sprintf("Diff. = %.3f%s", difference, sig_mark(p)),
               cdf_D = unname(ks$statistic), cdf_p = ks$p.value,
               mean_inference = "Class-FE contrast, class-clustered CR1, t(63)",
               cdf_inference = "Asymptotic two-sample KS; adjusted outcomes treated as observed")
  }
  category_diffs <- bind_rows(category_diffs)
  write.csv(category_diffs, file.path(collective_result_dir, paste0("group_", outcome, "_category_differences.csv")), row.names = FALSE)
  category_y_top <- max(category_stats$high)
  category_bracket_low <- category_y_top + .025
  category_bracket_high <- category_y_top + .070
  bar <- ggplot(category_stats, aes(pair_category, mean, fill = pair_category)) +
    geom_col(width = .62, colour = "black", linewidth = .3) +
    geom_errorbar(aes(ymin = low, ymax = high), width = .16, linewidth = .45) +
    scale_fill_manual(values = c("Low-Low" = "#D99A99", "Low-High" = "#D8C98C", "High-High" = "#80ADD0")) +
    scale_x_discrete(labels = c("Low-Low" = "Low\nLow", "Low-High" = "Low\nHigh", "High-High" = "High\nHigh")) +
    scale_y_continuous(breaks = seq(.85, 1, .05), labels = scales::label_number(accuracy = .01), expand = c(0, 0)) +
    coord_cartesian(ylim = c(.84, 1.08)) +
    labs(x = NULL, y = paste("Mean Collective", title)) +
    paper_theme +
    theme(legend.position = "none",
          plot.margin = margin(12, 12, 12, 12), plot.background = element_rect(fill = "white", colour = NA))
  for (i in 1:3) {
    x1 <- c(1, 2, 1)[i]
    x2 <- c(2, 3, 3)[i]
    y <- if (i == 3) category_bracket_high else category_bracket_low
    bar <- bar +
      annotate("segment", x = x1, xend = x2, y = y, yend = y) +
      annotate("segment", x = x1, xend = x1, y = y - .020, yend = y) +
      annotate("segment", x = x2, xend = x2, y = y - .020, yend = y) +
      annotate("label", x = mean(c(x1, x2)), y = y + .012,
               label = category_diffs$label[i], size = 4.2, fill = "white")
  }
  cdf_comparisons <- category_diffs[1:2, ]
  cdf_labels <- sprintf("%s vs %s:\nD = %.3f, %s",
    cdf_comparisons$lower, cdf_comparisons$upper, cdf_comparisons$cdf_D,
    ifelse(cdf_comparisons$cdf_p < .001, "p < 0.001", sprintf("p = %.3f", cdf_comparisons$cdf_p)))
  cdf <- ggplot(category_data, aes(class_adjusted, colour = pair_category, linetype = pair_category)) +
    stat_ecdf(geom = "step", linewidth = .9, pad = FALSE) +
    annotate("text", x = .035, y = .48, hjust = 0, vjust = 1, size = 3.8,
      label = paste(c("KS tests", cdf_labels), collapse = "\n")) +
    scale_colour_manual(values = c("Low-Low" = "red", "Low-High" = "grey45", "High-High" = "blue")) +
    scale_linetype_manual(values = c("Low-Low" = "dashed", "Low-High" = "dotdash", "High-High" = "solid")) +
    scale_x_continuous(breaks = c(seq(0, 1, .25), 1.2), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .25), expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, 1.2)) +
    labs(x = paste("Collective", title), y = "Cumulative probability", colour = NULL, linetype = NULL) +
    paper_theme +
    theme(legend.position = "inside", legend.position.inside = c(.02, .98),
          legend.justification = c("left", "top"),
          legend.background = element_rect(fill = "white", colour = "black"),
          panel.grid.minor = element_blank(), plot.margin = margin(12, 18, 12, 12),
          plot.background = element_rect(fill = "white", colour = NA))

  for (kind in c("bar", "cdf")) {
    plot <- if (kind == "bar") bar else cdf
    plot <- plot + theme(text = element_text(size = 16))
    ggsave(file.path(collective_figure_dir, paste0("group_", outcome, "_", kind, ".pdf")), plot,
           width = 5, height = 4.5)
  }

  print(category_stats)
  print(category_diffs)
  panel_files <- file.path(collective_figure_dir, paste0("group_", outcome, "_", c("bar", "cdf"), ".pdf"))
  stopifnot(file.copy(panel_files, file.path(draft_figure_dir, basename(panel_files)), overwrite = TRUE))
}
cat("Pooled individual CCEI median:", category_pooled_median, "\n")

################################################################################
# Figure 8: Individual Rationality And Joint Collective Outcomes
################################################################################

# Run this section from Code after the Figure 8 section in 3.
if (!exists("code_dir")) code_dir <- getwd()
.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(ggplot2)
source(file.path(code_dir, "programs/plot_collective_ame.R"))
sample <- haven::read_dta(file.path(code_dir, "data/collective_analysis_sample.dta"))
member_sd <- c(maximum = sd(sample$ccei_max), minimum = sd(sample$ccei_min))
collective_result_dir <- file.path(code_dir, "results/collective_quality")
collective_figure_dir <- file.path(code_dir, "results/figures/collective_rationality")
dir.create(collective_figure_dir, recursive = TRUE, showWarnings = FALSE)
ame_files <- plot_cei_ame(file.path(collective_result_dir, "joint_outcome_linear_ame.csv"),
                         collective_figure_dir, classification = "a", portrait = TRUE,
                         prefix = "figure8_lpm", member_sd = member_sd)
draft_figure_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(draft_figure_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(ame_files, file.path(draft_figure_dir, basename(ame_files)), overwrite = TRUE))

message("4_Figures_Main.R completed. Outputs: ", file.path(code_dir, "results"))
