################################################################################
# 07_Figures_Main.R
# Latest update: 2026-10-11
# Main-paper figures begin with Figure 3 (I-M means/CDFs); Figures 1/2 are in the draft.
# Figure 4 repeats all six M-controlled Table 3 fits on its full 2,560-row sample.
# Columns (2)/(3) are drawn here; 09 draws Columns (5)/(6) as Figure A8.
# Inputs: Code/data panels, retained validated CCEI/M panel and full donor matrix.
# Outputs: results/figures/figure4_placebo_col{2,3}.pdf, the two-panel review composite
#          figure4_placebo_coefficients.png and coefficient draws,
#          current Figure 3 mean/CDF images, Figure 5 I-M survey bars,
#          and the other main-paper figures.
#
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
  library(tidyr)
  library(grid)
})

data_dir <- file.path(code_dir, "data")
result_dir <- file.path(code_dir, "results")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
figure_dir <- file.path(result_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
ihat_result_dir <- file.path(result_dir, "ihat")
dir.create(ihat_result_dir, recursive = TRUE, showWarnings = FALSE)

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
# class-clustered mean inference and 9,999 centered whole-class KS draws.
# This section runs independently after loading haven, ggplot2, dplyr and grid,
# and defining code_dir, result_dir, figure_dir, paper_theme and sig_mark above.
################################################################################

distance_panel <- read_dta(file.path(code_dir,
  "data/panel_individual.dta"))
stopifnot(nrow(distance_panel) == 2608, all(distance_panel$n_ccei_donors == 651),
          !anyDuplicated(distance_panel[c("id", "post")]))
distance_panel$member <- factor(ifelse(distance_panel$HighCCEI_both_high == 1,
  "Higher CCEI", "Lower CCEI"), levels = c("Lower CCEI", "Higher CCEI"))

# Center the bootstrap CDF difference to approximate the equality null.
# Each draw resamples whole classes jointly across both member groups and waves.
clustered_ks_test <- function(y, higher, cluster, repetitions = 9999L, seed = 20260812L) {
  stopifnot(!anyNA(y), !anyNA(higher), !anyNA(cluster), all(higher %in% c(FALSE, TRUE)))
  grid <- sort(unique(y))
  k <- length(grid)
  class_index <- match(cluster, unique(cluster))
  clusters <- max(class_index)
  y_index <- match(y, grid)
  counts <- lapply(c(FALSE, TRUE), function(group) {
    use <- higher == group
    histogram <- matrix(tabulate(y_index[use] + (class_index[use] - 1L) * k,
                                 nbins = k * clusters), nrow = k)
    apply(histogram, 2, cumsum)
  })
  group_n <- lapply(counts, function(x) x[k, ])
  difference <- rowSums(counts[[2]]) / sum(group_n[[2]]) -
    rowSums(counts[[1]]) / sum(group_n[[1]])
  observed <- max(abs(difference))
  set.seed(seed)
  weights <- rmultinom(repetitions, size = clusters, prob = rep(1 / clusters, clusters))
  draws <- numeric(repetitions)
  # Batch draws to avoid storing all bootstrapped CDFs at once.
  for (first in seq.int(1L, repetitions, by = 250L)) {
    indices <- first:min(first + 249L, repetitions)
    w <- weights[, indices, drop = FALSE]
    n_low <- as.vector(group_n[[1]] %*% w)
    n_high <- as.vector(group_n[[2]] %*% w)
    stopifnot(all(n_low > 0), all(n_high > 0))
    boot_difference <- sweep(counts[[2]] %*% w, 2, n_high, "/") -
      sweep(counts[[1]] %*% w, 2, n_low, "/")
    draws[indices] <- apply(abs(sweep(boot_difference, 1, difference, "-")), 2, max)
  }
  exceedances <- sum(draws >= observed)
  list(statistic = observed, p.value = (1 + exceedances) / (repetitions + 1),
       draws = data.frame(replication = seq_len(repetitions), centered_D = draws),
       repetitions = repetitions, seed = seed, clusters = clusters, exceedances = exceedances)
}

adjusted_distance_plots <- function(data, value, mean_label, distance_label) {
  stats <- data |>
    group_by(member) |>
    summarise(mean = mean(.data[[value]]), sd = sd(.data[[value]]), n = n(),
              .groups = "drop")
  mean_fit <- fixest::feols(reformulate("member", value, intercept = FALSE),
    data = data, vcov = ~class,
    ssc = fixest::ssc(K.adj = TRUE, G.adj = TRUE, t.df = "min"))
  clusters <- length(unique(data$class))
  df <- clusters - 1L
  stopifnot(clusters == 64L, nobs(mean_fit) == nrow(data),
            max(abs(unname(coef(mean_fit)) - stats$mean)) < 1e-12)
  stats$se <- unname(fixest::se(mean_fit))
  stats$ci <- qt(.975, df) * stats$se
  contrast <- c(-1, 1)
  difference_se <- sqrt(drop(t(contrast) %*% vcov(mean_fit) %*% contrast))
  ks <- clustered_ks_test(data[[value]], data$member == "Higher CCEI", data$class)
  difference <- stats$mean[stats$member == "Higher CCEI"] -
    stats$mean[stats$member == "Lower CCEI"]
  difference_p <- 2 * pt(-abs(difference / difference_se), df)
  y_min <- min(0, min(stats$mean - stats$ci) - .02)
  y_max <- max(0, max(stats$mean + stats$ci) + .04)
  bar <- ggplot(stats, aes(member, mean, fill = member)) +
    geom_col(width = .62, colour = "black", linewidth = .3) +
    geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = .16) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = .4) +
    annotate("text", x = 1.5, y = y_max,
      label = sprintf("High - low = %.3f%s", difference, sig_mark(difference_p)), size = 4.4) +
    scale_fill_manual(values = c("Lower CCEI" = "#D99A99", "Higher CCEI" = "#80ADD0")) +
    coord_cartesian(ylim = c(y_min, y_max + .015)) +
    labs(x = NULL, y = mean_label) + paper_theme +
    theme(legend.position = "none")
  cdf <- ggplot(data, aes(.data[[value]], colour = member, linetype = member)) +
    stat_ecdf(geom = "step", linewidth = .85, pad = FALSE) +
    annotate("text", x = .92, y = .12, hjust = 1, size = 4.4,
      label = sprintf("KS: D = %.3f\nClass-bootstrap %s", unname(ks$statistic),
        if (ks$p.value < .001) "p < 0.001" else sprintf("p = %.3f", ks$p.value))) +
    geom_vline(xintercept = 0, colour = "grey65", linewidth = .4) +
    scale_colour_manual(values = c("Lower CCEI" = "red", "Higher CCEI" = "blue")) +
    scale_linetype_manual(values = c("Lower CCEI" = "dashed", "Higher CCEI" = "solid")) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .25), expand = c(0, 0)) +
    labs(x = distance_label, y = "Cumulative probability", colour = NULL, linetype = NULL) +
    paper_theme + theme(legend.position = c(.2, .84),
      legend.background = element_rect(fill = "white", colour = "black"))
  stats$high_minus_low <- difference
  stats$difference_se <- difference_se
  stats$difference_p <- difference_p
  stats$clusters <- clusters
  stats$df <- df
  stats$ks_D <- unname(ks$statistic)
  stats$ks_p <- ks$p.value
  stats$ks_exceedances <- ks$exceedances
  stats$ks_repetitions <- ks$repetitions
  stats$ks_seed <- ks$seed
  stats$mean_inference <- "Class-clustered OLS, CR1, t(63)"
  stats$ks_inference <- "Centered pairs bootstrap of classes"
  list(bar = bar, cdf = cdf, stats = stats, ks_draws = ks$draws)
}

actual_distance_data <- filter(distance_panel, !is.na(Istar_ccei))
stopifnot(nrow(actual_distance_data) == 2560)
figure3 <- adjusted_distance_plots(actual_distance_data, "Istar_ccei",
  expression("Mean placebo-adjusted distance " * (I-M)),
  expression("Placebo-adjusted distance " * (I-M)))
ggsave(file.path(figure_dir, "ccei_IminusM_by_higher_ccei_bar.png"), figure3$bar,
       width = 6, height = 5, dpi = 300)
ggsave(file.path(figure_dir, "ccei_IminusM_by_higher_ccei_cdf.png"), figure3$cdf,
       width = 6, height = 5, dpi = 300)
write.csv(figure3$stats, file.path(figure_dir, "ccei_IminusM_by_higher_ccei_stats.csv"),
          row.names = FALSE)
write.csv(figure3$ks_draws, file.path(figure_dir, "figure3_cluster_KS_draws.csv"),
          row.names = FALSE)
for (panel in c("bar", "cdf")) {
  filename <- sprintf("ccei_IminusM_by_higher_ccei_%s.png", panel)
  stopifnot(file.copy(file.path(figure_dir, filename),
    file.path(code_dir, "../Overleaf/figures_2025", filename), overwrite = TRUE))
}

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
  for (destination in c(result_dir, ihat_result_dir,
                        file.path(code_dir, "../Overleaf/figures_2025"))) {
    stopifnot(file.copy(file.path(figure_dir, filename),
      file.path(destination, filename), overwrite = TRUE))
  }
}

################################################################################
