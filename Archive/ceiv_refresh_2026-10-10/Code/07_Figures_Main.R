################################################################################
# 07_Figures_Main.R
# Latest update: 2026-10-10
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
  "IminusM_review/outputs/data/ccei_ra_candidate_analysis.dta"))
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
# Figure 4: M-controlled placebo regressions on Table 3's full 2,560-row sample.
################################################################################
# Reuse the retained full donor matrix; no index recalculation.
donor_file <- file.path(code_dir, "results/placebo_normalized/placebo_donor_matrix.csv")
donors <- readr::read_csv(donor_file, show_col_types = FALSE, progress = FALSE,
  col_select = c(target_group_id, post, member1_id, member2_id, donor_group_id,
                 is_own, Ihat1_donor, Ihat2_donor),
  col_types = readr::cols(.default = readr::col_character(), post = readr::col_integer(),
    is_own = readr::col_integer(), Ihat1_donor = readr::col_double(),
    Ihat2_donor = readr::col_double()))
roster <- donors |>
  distinct(target_group_id, post, member1_id, member2_id) |>
  arrange(post, target_group_id)
stopifnot(nrow(roster) == 1304, !anyDuplicated(roster[c("target_group_id", "post")]))
counts <- donors |>
  filter(is_own == 0) |>
  count(target_group_id, post)
stopifnot(nrow(counts) == 1304, all(counts$n == 651))
matrix_means <- donors |>
  filter(is_own == 0) |>
  group_by(target_group_id, post, member1_id, member2_id) |>
  summarise(M1 = mean(ifelse(is.na(Ihat1_donor), .5, Ihat1_donor)),
            M2 = mean(ifelse(is.na(Ihat2_donor), .5, Ihat2_donor)), .groups = "drop")
benchmark_check <- bind_rows(
  transmute(matrix_means, id = member1_id, post, M = M1),
  transmute(matrix_means, id = member2_id, post, M = M2)) |>
  inner_join(select(distance_panel, id, post, M_ccei), by = c("id", "post"))
stopifnot(nrow(benchmark_check) == 2608,
          max(abs(benchmark_check$M - benchmark_check$M_ccei)) < 1e-10)
# Figure 4: 500 reassignments, six Table 3 specifications in each repetition.
# The retained panel includes the exact corner/midpoint shares prepared for Table 3.
regression_data <- filter(distance_panel, !is.na(I_ccei))
stopifnot(nrow(regression_data) == 2560, n_distinct(regression_data$class) == 64,
          n_distinct(regression_data$id) == 1304,
          sum(table(regression_data$id) == 1L) == 48L,
          all(is.finite(regression_data$I_ccei)))
individual_controls <- c(
  "mathscore_i", "mathscore_diff", "height_i", "height_diff",
  "outgoing_i", "outgoing_diff", "opened_i", "opened_diff",
  "agreeable_i", "agreeable_diff", "conscientious_i", "conscientious_diff",
  "stable_i", "stable_diff")
friendship_controls <- c("inclass_n_friends_i", "inclass_n_diff",
                        "inclass_popularity_i", "inclass_pop_diff")
missing_controls <- c("mathscore_diff_missing", "outgoing_diff_missing",
  "opened_diff_missing", "agreeable_diff_missing", "conscientious_diff_missing",
  "stable_diff_missing")
share_controls <- c("corner_share_i", "corner_share_diff", "mid_share_i", "mid_share_diff")
full_controls <- c(individual_controls, friendship_controls, missing_controls, share_controls)
gender_controls <- c("female_i_male_j", "male_i_female_j")
focal_variables <- rep(c("HighCCEI_both_high", "ccei_gap_ij"), each = 3)
fixed_effects <- rep(c("class", "class", "id"), 2)
specification_controls <- rep(list(character(), c(full_controls, gender_controls),
                                  full_controls), 2)

estimate_table3 <- function(data, outcome) {
  bind_rows(lapply(seq_len(6), function(s) {
    rhs <- c(focal_variables[s], "M_ccei", specification_controls[[s]])
    formula <- as.formula(paste(outcome, "~", paste(rhs, collapse = " + "),
                               "|", fixed_effects[s]))
    # Match reghdfe's keepsingletons and class-cluster small-sample correction.
    fit <- fixest::feols(formula, data = data, vcov = ~class, fixef.rm = "none",
      ssc = fixest::ssc(K.adj = TRUE, K.fixef = "nonnested", G.adj = TRUE, t.df = "min"),
      nthreads = 1, notes = FALSE)
    stopifnot(nobs(fit) == 2560, length(unique(data$class)) == 64,
              all(is.finite(coef(fit))), all(is.finite(fixest::se(fit))))
    tibble(specification = s, focal = focal_variables[s],
      coefficient = unname(coef(fit)[focal_variables[s]]),
      se = unname(fixest::se(fit)[focal_variables[s]]),
      coefficient_M = unname(coef(fit)["M_ccei"]), N = nobs(fit), clusters = 64L)
  }))
}
actual_coefficients <- estimate_table3(regression_data, "I_ccei")
stopifnot(all(is.finite(actual_coefficients$coefficient)))

# Resolve donor/member rows once; reassignment changes only the outcome.
strata <- split(seq_len(nrow(roster)), roster$post)
stopifnot(all(lengths(strata) == 652))
key <- function(group_id, post, donor_group_id) paste(group_id, post, donor_group_id, sep = "|")
donor_keys <- key(donors$target_group_id, donors$post, donors$donor_group_id)
stopifnot(!anyDuplicated(donor_keys))
lookup_row <- setNames(seq_len(nrow(donors)), donor_keys)
member_keys <- paste(c(roster$member1_id, roster$member2_id), rep(roster$post, 2), sep = "|")
member_rows <- match(paste(regression_data$id, regression_data$post, sep = "|"), member_keys)
stopifnot(!anyDuplicated(member_keys), !anyNA(member_rows))

seed <- 20260812L
n_repetitions <- 500L
set.seed(seed)
coefficient_draws <- vector("list", n_repetitions)
undefined_counts <- integer(n_repetitions)
for (r in seq_len(n_repetitions)) {
  donor_assignment <- character(nrow(roster))
  for (indices in strata) {
    randomized <- sample(indices, length(indices), replace = FALSE)
    donor_indices <- c(randomized[-1L], randomized[1L])
    stopifnot(!anyDuplicated(donor_indices), setequal(donor_indices, indices),
              all(roster$post[donor_indices] == roster$post[randomized]))
    donor_assignment[randomized] <- roster$target_group_id[donor_indices]
  }
  stopifnot(all(donor_assignment != roster$target_group_id))
  rows <- unname(lookup_row[key(roster$target_group_id, roster$post, donor_assignment)])
  stopifnot(!anyNA(rows), all(donors$is_own[rows] == 0))
  donor_distance <- c(donors$Ihat1_donor[rows], donors$Ihat2_donor[rows])[member_rows]
  undefined_counts[r] <- sum(is.na(donor_distance))
  regression_data$I_placebo <- ifelse(is.na(donor_distance), .5, donor_distance)
  coefficient_draws[[r]] <- estimate_table3(regression_data, "I_placebo") |>
    mutate(repetition = r, undefined_donor_observations = undefined_counts[r])
  if (r == 1 || r %% 25 == 0) message("Figure 4: repetition ", r, "/", n_repetitions)
}
placebo_coefficients <- bind_rows(coefficient_draws)
stopifnot(nrow(placebo_coefficients) == 3000,
          all(is.finite(placebo_coefficients$coefficient)),
          all(table(placebo_coefficients$specification) == 500),
          all(placebo_coefficients$N == 2560), all(placebo_coefficients$clusters == 64))
figure4_summary <- placebo_coefficients |>
  group_by(specification) |>
  summarise(placebo_median = median(coefficient),
    placebo_lower = unname(quantile(coefficient, .025)),
    placebo_upper = unname(quantile(coefficient, .975)),
    repetitions = n(), placebo_sd = sd(coefficient),
    .groups = "drop") |>
  left_join(rename(actual_coefficients, actual_coefficient = coefficient,
                   actual_se = se, actual_coefficient_M = coefficient_M), by = "specification")
readr::write_csv(placebo_coefficients, file.path(figure_dir, "figure4_placebo_coefficients.csv"))
readr::write_csv(figure4_summary, file.path(figure_dir, "figure4_placebo_summary.csv"))
readr::write_csv(tibble(repetitions = n_repetitions, seed = seed, sample_N = 2560,
  clusters = 64, matching = "cyclic reassignment within wave across all classes",
  undefined_donor_value = .5), file.path(figure_dir, "figure4_placebo_run_config.csv"))

# Figure 4 follows Figure 3; Figure A8 exports Columns (5)/(6) after A7 in 09.
source(file.path(code_dir, "programs/plot_table3_placebo.R"))
export_table3_placebo_panels(code_dir, c(2L, 3L), "figure4_placebo_coefficients.png")
message("Figure 4 completed: 500 repetitions x 6 M-controlled fits; N=2560, 64 classes; seed ", seed)
print(figure4_summary)

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
# Supplementary figure: Collective CCEI By Members' Individual CCEI Category
################################################################################

pooled_median <- median(panel_individual$ccei_i, na.rm = TRUE)

group_data <- panel_individual |>
  filter(!is.na(group_id), !is.na(post), !is.na(ccei_i), !is.na(ccei_g)) |>
  group_by(group_id, post) |>
  summarise(
    ccei_g = first(ccei_g),
    n_members = n(),
    n_high = sum(ccei_i >= pooled_median),
    .groups = "drop"
  ) |>
  filter(n_members == 2) |>
  mutate(
    pair_category = factor(
      n_high,
      levels = c(0, 1, 2),
      labels = c("Low-Low", "Low-High", "High-High")
    )
  )

group_stats <- group_data |>
  group_by(pair_category) |>
  summarise(
    mean = mean(ccei_g),
    sd = sd(ccei_g),
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
    comparison$ccei_g[comparison$pair_category == upper_group]
  ) - mean(
    comparison$ccei_g[comparison$pair_category == lower_group]
  )
  test <- t.test(ccei_g ~ pair_category, data = comparison)
  tibble(
    difference = difference,
    label = sprintf("Diff. = %.3f%s", difference, sig_mark(test$p.value))
  )
}

diff_extreme <- pairwise_diff(group_data, "Low-Low", "High-High")
diff_low_mid <- pairwise_diff(group_data, "Low-Low", "Low-High")
diff_mid_high <- pairwise_diff(group_data, "Low-High", "High-High")

group_y_min <- max(
  0.75,
  floor((min(group_stats$mean - group_stats$ci, na.rm = TRUE) - 0.02) * 100) / 100
)
group_y_top <- max(group_stats$mean + group_stats$ci, na.rm = TRUE)
bracket_low <- group_y_top + 0.025
bracket_high <- group_y_top + 0.070

group_bar <- ggplot(group_stats, aes(pair_category, mean, fill = pair_category)) +
  geom_col(width = 0.62, colour = "black", linewidth = 0.3) +
  geom_errorbar(
    aes(ymin = mean - ci, ymax = mean + ci),
    width = 0.16, linewidth = 0.45
  ) +
  annotate("segment", x = 1, xend = 2, y = bracket_low, yend = bracket_low) +
  annotate("segment", x = 1, xend = 1, y = bracket_low - 0.020, yend = bracket_low) +
  annotate("segment", x = 2, xend = 2, y = bracket_low - 0.020, yend = bracket_low) +
  annotate("label", x = 1.48, y = bracket_low + 0.012,
           label = diff_low_mid$label, size = 18 / .pt,
           label.size = 0, fill = "white") +
  annotate("segment", x = 2, xend = 3, y = bracket_low, yend = bracket_low) +
  annotate("segment", x = 2, xend = 2, y = bracket_low - 0.020, yend = bracket_low) +
  annotate("segment", x = 3, xend = 3, y = bracket_low - 0.020, yend = bracket_low) +
  annotate("label", x = 2.58, y = bracket_low + 0.012,
           label = diff_mid_high$label, size = 17 / .pt,
           label.size = 0, fill = "white") +
  annotate("segment", x = 1, xend = 3, y = bracket_high, yend = bracket_high) +
  annotate("segment", x = 1, xend = 1, y = bracket_high - 0.020, yend = bracket_high) +
  annotate("segment", x = 3, xend = 3, y = bracket_high - 0.020, yend = bracket_high) +
  annotate("label", x = 2, y = bracket_high + 0.012,
           label = diff_extreme$label, size = 18 / .pt,
           label.size = 0, fill = "white") +
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
  labs(x = NULL, y = "Mean Collective CCEI") +
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
  aes(ccei_g, colour = pair_category, linetype = pair_category)
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
    x = expression("Collective CCEI (" * CCEI[g] * ")"),
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

ggsave(
  file.path(result_dir, "group_ccei_by_member_ccei_median_bar.png"),
  group_bar, width = 6, height = 5, dpi = 300
)
ggsave(
  file.path(result_dir, "group_ccei_by_member_ccei_median_cdf.png"),
  group_cdf, width = 6, height = 5, dpi = 300
)

################################################################################
# Figure 6: Joint Collective Outcomes
################################################################################

# Saved inputs are generated by 11_collective_quality.do.
# To run this section and its appendix alone, load haven/ggplot2/dplyr and set code_dir.
collective_result_dir <- file.path(code_dir, "results/new_indices/collective_rationality_summary")
joint_data <- read_dta(file.path(collective_result_dir, "analysis_sample.dta"))
stopifnot(nrow(joint_data) == 1304, length(unique(joint_data$group_id)) == 652,
          !anyDuplicated(joint_data[c("group_id", "post")]),
          all(is.finite(as.matrix(joint_data[c("ccei_g", "ceiv_g", "CEIV_lower", "CEIV_upper")]))))
joint_category <- 1L + (joint_data$ccei_g >= 1 - 1e-9) + 2L * (joint_data$ceiv_g >= 1 - 1e-9)
stopifnot(all(joint_category == joint_data$joint_category),
          all((joint_data$ceiv_g >= 1 - 1e-9) == (joint_data$CEIV_lower >= 1 - 1e-9)),
          all((joint_data$ceiv_g >= 1 - 1e-9) == (joint_data$CEIV_upper >= 1 - 1e-9)))
joint_counts <- data.frame(outcome = 1:4, ccei_one = c(0, 1, 0, 1), ceiv_one = c(0, 0, 1, 1),
                     n = tabulate(joint_category, nbins = 4))
joint_counts$share <- joint_counts$n / nrow(joint_data)
joint_ames <- read.csv(file.path(collective_result_dir, "figure6_ame.csv"))
stopifnot(sum(joint_counts$n) == 1304,
          max(abs(joint_ames$share - joint_counts$share[joint_ames$outcome])) < 1e-12)
write.csv(joint_counts, file.path(collective_result_dir, "joint_outcome_counts.csv"), row.names = FALSE)
plot_joint_outcomes <- function(data, counts) {
  # Draw endpoint categories last, without jittering the observed index values.
  points <- data[order(data$joint_category), ]
  points$outcome <- factor(points$joint_category, levels = counts$outcome)
  legend_labels <- sprintf("%s: %s (%.1f%%)",
    c("Both < 1", "CCEI = 1 only", "CEIV = 1 only", "Both = 1"),
    format(counts$n, big.mark = ",", trim = TRUE), 100 * counts$share)
  legend_title <- sprintf("Pair-waves (n = %s)", format(nrow(data), big.mark = ",", trim = TRUE))
  point_fill <- c("grey65", "#D73027", "#2C7BB6", "black")
  point_colour <- c("grey35", "grey15", "grey15", "black")
  point_size <- c(1.8, 2.1, 2.3, 4.5)
  point_alpha <- c(.55, 1, 1, 1)

  ggplot(points, aes(ccei_g, ceiv_g)) +
    annotate("segment", x = 0, y = 0, xend = 1, yend = 1,
             colour = "grey65", linewidth = .5) +
    geom_vline(xintercept = 1, colour = "grey35", linetype = "dashed", linewidth = .5) +
    geom_hline(yintercept = 1, colour = "grey35", linetype = "dashed", linewidth = .5) +
    geom_point(aes(fill = outcome, colour = outcome, shape = outcome,
                   size = outcome, alpha = outcome), stroke = .4) +
    annotate("text", x = .11, y = .14, label = "CEIV = CCEI", angle = 45,
             colour = "grey40", size = 5) +
    scale_fill_manual(name = legend_title, values = point_fill, labels = legend_labels,
                      drop = FALSE) +
    scale_shape_manual(name = legend_title, values = c(21, 21, 23, 22),
                       labels = legend_labels, drop = FALSE) +
    scale_colour_manual(values = point_colour, guide = "none") +
    scale_size_manual(values = point_size, guide = "none") +
    scale_alpha_manual(values = point_alpha, guide = "none") +
    guides(fill = guide_legend(override.aes = list(size = point_size, alpha = point_alpha,
                                                 colour = point_colour))) +
    scale_x_continuous(breaks = seq(0, 1, .2), labels = function(x) sprintf("%.1f", x),
                       limits = c(-.025, 1.025), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 1, .2), labels = function(x) sprintf("%.1f", x),
                       limits = c(-.025, 1.025), expand = c(0, 0)) +
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
figure6_file <- file.path(collective_result_dir, "joint_outcome_quadrants.pdf")
ggsave(figure6_file, joint_plot, width = 7, height = 6.3)
print(joint_counts)

draft_figure_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(draft_figure_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(figure6_file, file.path(draft_figure_dir, basename(figure6_file)),
                   overwrite = TRUE))

################################################################################
# Appendix Figure 6 robustness: exclude all-corner/midpoint collective choices.
# Exact rule and inclusive 2.5-percentage-point payoff-share buffer.
################################################################################
choice_patterns <- bind_rows(lapply(c("base", "end"), function(wave) {
  raw <- read_dta(file.path(code_dir, "data", paste0(wave, "_raw.dta"))) |>
    filter(round_number >= 19, mover == 1, group_id %in% joint_data$group_id)
  stopifnot(all(raw$coord_x + raw$coord_y > 0))
  raw |>
    mutate(post = as.integer(wave == "end"), share = coord_x / (coord_x + coord_y),
           exact = share %in% c(0, .5, 1),
           buffered = share <= .025 | share >= .975 | (share >= .475 & share <= .525)) |>
    group_by(group_id, post) |>
    summarise(n_choices = n(), all_exact = all(exact), all_buffered = all(buffered),
              .groups = "drop")
}))
stopifnot(nrow(choice_patterns) == 1304, all(choice_patterns$n_choices == 18),
          all(!choice_patterns$all_exact | choice_patterns$all_buffered))
exclusion_data <- left_join(joint_data, choice_patterns, by = c("group_id", "post"))
stopifnot(!anyNA(exclusion_data$all_exact), !anyNA(exclusion_data$all_buffered))
appendix_counts <- list()
appendix_plots <- list()
for (rule in c("exact", "buffered")) {
  retained <- exclusion_data[!exclusion_data[[paste0("all_", rule)]], ]
  counts <- data.frame(outcome = 1:4, ccei_one = c(0, 1, 0, 1), ceiv_one = c(0, 0, 1, 1),
                       n = tabulate(retained$joint_category, nbins = 4))
  counts$share <- counts$n / nrow(retained)
  counts$rule <- rule
  counts$retained_n <- nrow(retained)
  counts$excluded_n <- nrow(exclusion_data) - nrow(retained)
  stopifnot(sum(counts$n) == counts$retained_n[1], abs(sum(counts$share) - 1) < 1e-12)
  appendix_counts[[rule]] <- counts
  appendix_plots[[rule]] <- plot_joint_outcomes(retained, counts) +
    labs(title = if (rule == "exact") "(a) Exact corner/midpoint exclusion" else
      "(b) Exclusion with 2.5-point buffer") +
    theme(plot.title = element_text(size = 16))
}
write.csv(bind_rows(appendix_counts), file.path(collective_result_dir, "joint_outcome_exclusion_counts.csv"), row.names = FALSE)
write.csv(choice_patterns, file.path(collective_result_dir, "joint_outcome_exclusion_flags.csv"), row.names = FALSE)
appendix_figure <- cowplot::plot_grid(plotlist = appendix_plots, nrow = 1)
appendix_file <- file.path(collective_result_dir, "joint_outcomes_excluding_simple_choices.png")
ggsave(appendix_file, appendix_figure, width = 13, height = 6.4, dpi = 300, bg = "white")
stopifnot(file.copy(appendix_file, file.path(draft_figure_dir, basename(appendix_file)), overwrite = TRUE))
print(bind_rows(appendix_counts))

################################################################################
# Figure 7: Collective CCEI And CEIV By Members' Individual CCEI Category
################################################################################

# Saved inputs are generated by 11_collective_quality.do.
# To run this section alone, load haven/ggplot2/dplyr/grid and set code_dir to Code.
collective_result_dir <- file.path(code_dir, "results/new_indices/collective_rationality_summary")
category_data <- read_dta(file.path(collective_result_dir, "analysis_sample.dta"))
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
  category_stats <- category_data |>
    group_by(pair_category) |>
    summarise(mean = mean(.data[[variable]]), sd = sd(.data[[variable]]), n = n(),
              se = sd / sqrt(n), ci = qt(.975, n - 1) * se, .groups = "drop")
  write.csv(category_stats, file.path(collective_result_dir, paste0("group_", outcome, "_category_means.csv")), row.names = FALSE)
  category_comparisons <- list(c("Low-Low", "Low-High"), c("Low-High", "High-High"), c("Low-Low", "High-High"))
  category_diffs <- bind_rows(lapply(category_comparisons, function(categories) {
    lower <- category_data[[variable]][category_data$pair_category == categories[1]]
    upper <- category_data[[variable]][category_data$pair_category == categories[2]]
    test <- t.test(lower, upper)
    difference <- mean(upper) - mean(lower)
    p <- test$p.value
    mark <- if (p < .01) "**" else if (p < .05) "*" else if (p < .1) "+" else ""
    data.frame(lower = categories[1], upper = categories[2], difference = difference,
               p = p, label = sprintf("Diff. = %.3f%s", difference, mark))
  }))
  write.csv(category_diffs, file.path(collective_result_dir, paste0("group_", outcome, "_category_differences.csv")), row.names = FALSE)

  category_y_min <- max(.75, floor((min(category_stats$mean - category_stats$ci) - .02) * 100) / 100)
  category_y_top <- max(category_stats$mean + category_stats$ci)
  category_bracket_low <- category_y_top + .025
  category_bracket_high <- category_y_top + .070
  bar <- ggplot(category_stats, aes(pair_category, mean, fill = pair_category)) +
    geom_col(width = .62, colour = "black", linewidth = .3) +
    geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = .16, linewidth = .45) +
    scale_fill_manual(values = c("Low-Low" = "#E39695", "Low-High" = "#D8C98C", "High-High" = "#74A9CF")) +
    scale_x_discrete(labels = c("Low-Low" = "Low\nLow", "Low-High" = "Low\nHigh", "High-High" = "High\nHigh")) +
    scale_y_continuous(breaks = seq(.85, 1, .05), labels = scales::label_number(accuracy = .01), expand = c(0, 0)) +
    coord_cartesian(ylim = c(category_y_min, category_bracket_high + .035)) +
    labs(x = NULL, y = paste("Mean Collective", title)) +
    theme_classic(base_size = 16) +
    theme(legend.position = "none", panel.grid.major = element_line(colour = "grey90", linewidth = .4),
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
  cdf <- ggplot(category_data, aes(.data[[variable]], colour = pair_category, linetype = pair_category)) +
    stat_ecdf(geom = "step", linewidth = .9, pad = FALSE) +
    scale_colour_manual(values = c("Low-Low" = "red", "Low-High" = "grey45", "High-High" = "blue")) +
    scale_linetype_manual(values = c("Low-Low" = "dashed", "Low-High" = "dotdash", "High-High" = "solid")) +
    scale_x_continuous(breaks = seq(0, 1, .2), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2), expand = c(0, 0)) +
    coord_cartesian(xlim = c(0, 1)) +
    labs(x = paste("Collective", title), y = "Cumulative probability", colour = NULL, linetype = NULL) +
    theme_minimal(base_size = 16) +
    theme(legend.position = "inside", legend.position.inside = c(.02, .98),
          legend.justification = c("left", "top"),
          legend.background = element_rect(fill = "white", colour = "black"),
          panel.grid.minor = element_blank(), plot.margin = margin(12, 18, 12, 12),
          plot.background = element_rect(fill = "white", colour = NA))

  for (kind in c("bar", "cdf")) {
    plot <- if (kind == "bar") bar else cdf
    plot <- plot + theme(text = element_text(size = 16))
    ggsave(file.path(collective_result_dir, paste0("group_", outcome, "_", kind, ".pdf")), plot,
           width = 5, height = 4.5)
  }

  png(file.path(collective_result_dir, paste0("group_", outcome, "_by_member_ccei_category.png")), width = 3600, height = 2100, res = 300)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(4, 2, heights = unit(c(.10, .75, .06, .09), "npc"))))
  grid.text(paste("Collective", title, "by members' individual CCEI category"), gp = gpar(fontsize = 20, fontface = "bold"),
            vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(bar, newpage = FALSE, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(cdf, newpage = FALSE, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  grid.text(paste("(a) Mean Collective", title), gp = gpar(fontsize = 15),
            vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  grid.text(paste("(b) CDF of Collective", title), gp = gpar(fontsize = 15),
            vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
  grid.text(sprintf("High: individual CCEI above pooled median %.7f across both waves. N = 1,304 pair-waves.\nLow-Low / Low-High / High-High: %s. 95%% t intervals; Welch tests. + p < .10, * p < .05, ** p < .01.",
                    category_pooled_median, paste(category_stats$n, collapse = " / ")),
            gp = gpar(fontsize = 11, col = "grey30"),
            vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2))
  popViewport()
  dev.off()
  print(category_stats)
  print(category_diffs)
  panel_files <- file.path(collective_result_dir, paste0("group_", outcome, "_", c("bar", "cdf"), ".pdf"))
  stopifnot(file.copy(panel_files, file.path(draft_figure_dir, basename(panel_files)), overwrite = TRUE))
}
cat("Pooled individual CCEI median:", category_pooled_median, "\n")

################################################################################
# Figure 8: Individual Rationality And Joint Collective Outcomes
################################################################################

# Saved inputs are generated by 11_collective_quality.do; figure6 filenames are legacy names.
# To run this section alone, load ggplot2 and set code_dir to the Code folder.
# The same plotter supports the saved risk-aversion and communication splits.
plot_cei_ame <- function(input, output_dir, classification = "original", portrait = FALSE) {
  stopifnot(classification %in% c("original", "a", "b"))
  n_categories <- if (classification == "b") 3L else 4L
  prefix <- if (classification == "original") "cei" else paste0("figure6_", classification)
  d <- read.csv(input)
  split_sample <- "ra_high" %in% names(d)
  communication_split <- all(c("moderator", "group") %in% names(d))
  split_fields <- if (communication_split) c("moderator", "group") else if (split_sample) "ra_high" else character()
  panels <- if (length(split_fields)) unique(d[split_fields]) else data.frame(panel = 1)
  keys <- c(split_fields, "outcome", "member")
  sum_groups <- if (length(split_fields)) do.call(interaction, c(d[c(split_fields, "member")], list(drop = TRUE))) else d$member
  stopifnot(nrow(d) == 2 * n_categories * nrow(panels), !anyDuplicated(d[keys]),
            all(is.finite(as.matrix(d[c("estimate", "low", "high")]))),
            all(abs(tapply(d$estimate, sum_groups, sum)) < 1e-8))
  d[c("estimate", "low", "high")] <- 100 * d[c("estimate", "low", "high")]
  d$outcome <- factor(d$outcome, levels = n_categories:1)
  outcome_labels <- c("1" = "CCEI < 1\nCEI < 1", "2" = "CCEI = 1\nCEI < 1",
                      "3" = "CCEI < 1\nCEI = 1", "4" = "CCEI = 1\nCEI = 1")

  if (classification == "a") {
    outcome_labels <- c("1" = "CCEI < 1\nCEIV < 1", "2" = "CCEI = 1\nCEIV < 1",
                        "3" = "CCEI < 1\nCEIV = 1", "4" = "CCEI = 1\nCEIV = 1")
  }
  if (classification == "b") {
    outcome_labels <- c("1" = "CEIV < 1\nCEIC < 1", "2" = "CEIV = 1\nCEIC < 1",
                        "3" = "CEIV = 1\nCEIC = 1")
  }
  # Use a common axis across split samples, expanding only when their intervals require it.
  limits <- c(-20, 30)
  if (length(split_fields)) limits <- c(min(-20, floor(min(d$low) / 10) * 10),
                               max(30, ceiling(max(d$high) / 10) * 10))
  stopifnot(all(d$low >= limits[1]), all(d$high <= limits[2]))

  # Match 07_Figures_Main.R: base size 14, white background, blue/red members,
  # no redundant legend; titles, panel captions, and notes belong in LaTeX.
  paper_theme <- theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.background = element_rect(fill = "white", colour = NA),
          axis.text.y = element_text(colour = "black"),
          axis.title.x = element_text(margin = margin(t = 8)),
          plot.margin = margin(8, 12, 6, 6))
  member_colours <- c(maximum = "blue", minimum = "red")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_files <- character()
  for (panel in seq_len(nrow(panels))) {
    keep <- rep(TRUE, nrow(d))
    for (field in split_fields) keep <- keep & d[[field]] == panels[[field]][panel]
    sample <- d[keep, ]
    sample_prefix <- if (communication_split) paste(prefix, panels$moderator[panel], panels$group[panel], sep = "_") else
      if (split_sample) paste0(prefix, "_", c("similar", "different")[panels$ra_high[panel] + 1]) else prefix
    for (member in names(member_colours)) {
      p <- ggplot(sample[sample$member == member, ], aes(estimate, outcome)) +
        geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.35) +
        geom_errorbar(aes(xmin = low, xmax = high), orientation = "y",
                      width = 0.12, linewidth = 0.55, colour = "grey35") +
        geom_point(size = 3, colour = member_colours[member]) +
        scale_x_continuous(limits = limits, breaks = seq(limits[1], limits[2], 10)) +
        scale_y_discrete(labels = outcome_labels, expand = expansion(add = 0.55)) +
        labs(x = "Average marginal effect (percentage points)", y = NULL) +
        paper_theme
      output_file <- file.path(output_dir, paste0(sample_prefix, "_ame_", member, ".png"))
      ggsave(output_file, p,
             width = if (portrait) 5.5 else 6, height = if (portrait) 3 else 5, dpi = 300)
      output_files <- c(output_files, output_file)
    }
  }
  invisible(output_files)
}

collective_result_dir <- file.path(code_dir, "results/new_indices/collective_rationality_summary")
ame_files <- plot_cei_ame(file.path(collective_result_dir, "figure6_ame.csv"),
                          collective_result_dir, classification = "a", portrait = TRUE)
draft_figure_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(draft_figure_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(ame_files, file.path(draft_figure_dir, basename(ame_files)), overwrite = TRUE))

message("07_Figures_Main.R completed. Outputs: ", file.path(code_dir, "results"))
