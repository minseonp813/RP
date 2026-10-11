rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(haven)
  library(readr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
review_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  normalizePath(getwd())
}
out_dir <- file.path(review_dir, "outputs")
table_dir <- file.path(out_dir, "tables")
figure_dir <- file.path(out_dir, "figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

panel <- read_dta(file.path(out_dir, "data", "ccei_ra_candidate_analysis.dta"))
regressions <- read_csv(file.path(table_dir, "main_comparison.csv"), show_col_types = FALSE)
buffers <- read_csv(file.path(table_dir, "buffer_results.csv"), show_col_types = FALSE)
movers <- read_csv(file.path(table_dir, "mover_results.csv"), show_col_types = FALSE)
shapley <- read_csv(file.path(table_dir, "shapley_results.csv"), show_col_types = FALSE)
alternative_file <- file.path(table_dir, "alternative_measure_comparison.csv")
alternative <- if (file.exists(alternative_file)) {
  read_csv(alternative_file, show_col_types = FALSE)
} else tibble()
stability_file <- file.path(table_dir, "alternative_sample_stability.csv")
stability <- if (file.exists(stability_file)) {
  read_csv(stability_file, show_col_types = FALSE)
} else tibble()
sensitivity_file <- file.path(table_dir, "benchmark_sensitivity.csv")
sensitivity <- if (file.exists(sensitivity_file)) {
  read_csv(sensitivity_file, show_col_types = FALSE)
} else tibble()
maxmpi_benchmark_file <- file.path(
  out_dir, "benchmarks", "maxmpi_sample20", "placebo_normalized_member_wave.dta"
)
maxmpi_diagnostics <- if (file.exists(maxmpi_benchmark_file)) {
  maxmpi_benchmark <- read_dta(maxmpi_benchmark_file)
  tibble(
    mean_undefined_share = mean(maxmpi_benchmark$degfrac_all),
    median_undefined_share = median(maxmpi_benchmark$degfrac_all),
    share_with_all_20_solved = mean(maxmpi_benchmark$nvalid_all == 20),
    minimum_solved_donors = min(maxmpi_benchmark$nvalid_all),
    member_wave_rows = nrow(maxmpi_benchmark)
  )
} else tibble()
if (nrow(maxmpi_diagnostics)) {
  write_csv(maxmpi_diagnostics, file.path(table_dir, "maxmpi_review_diagnostics.csv"))
}

sig <- function(p) {
  ifelse(is.na(p), "", ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, "+", ""))))
}
fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
}
markdown_table <- function(x) {
  header <- paste0("| ", paste(names(x), collapse = " | "), " |")
  rule <- paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |")
  rows <- apply(x, 1, function(z) paste0("| ", paste(z, collapse = " | "), " |"))
  paste(c(header, rule, rows), collapse = "\n")
}

# Main coefficient comparison in a review-friendly wide form.
comparison <- regressions |>
  mutate(
    measure = recode(measure, ccei = "CCEI distance", ra = "Risk-aversion distance"),
    definition = if_else(focal == "HighCCEI_both_high", "Higher CCEI", "CCEI difference"),
    estimate = paste0(fmt(beta), sig(p)),
    standard_error = paste0("(", fmt(se), ")"),
    M_coefficient = if_else(is.na(beta_M), "", paste0(fmt(beta_M), " [p(M=1)=", fmt(p_M_equals_one), "]")),
    coefficient_over_outcome_sd = beta / outcome_sd
  ) |>
  select(measure, definition, specification, outcome_model, estimate, standard_error,
         M_coefficient, coefficient_over_outcome_sd, standardized, N, r2) |>
  arrange(measure, definition, specification, factor(outcome_model, c("raw", "normalized", "adjusted")))
write_csv(comparison, file.path(table_dir, "main_comparison_formatted.csv"))

# Figure and distribution statistics for Section 5.1.
figure_data <- panel |>
  filter(!is.na(I_ccei), !is.na(Istar_ccei), !is.na(HighCCEI_both_high)) |>
  mutate(member = factor(
    if_else(HighCCEI_both_high == 1, "Higher CCEI", "Lower CCEI"),
    levels = c("Lower CCEI", "Higher CCEI")
  ))

distribution_stats <- bind_rows(lapply(c("I_ccei", "Istar_ccei"), function(v) {
  d <- figure_data |>
    group_by(member) |>
    summarise(
      mean = mean(.data[[v]]), sd = sd(.data[[v]]), n = n(),
      se = sd / sqrt(n), ci = qt(.975, n - 1) * se,
      .groups = "drop"
    )
  tt <- t.test(figure_data[[v]] ~ figure_data$member)
  ks <- suppressWarnings(ks.test(
    figure_data[[v]][figure_data$member == "Lower CCEI"],
    figure_data[[v]][figure_data$member == "Higher CCEI"],
    exact = FALSE
  ))
  d |>
    mutate(
      outcome = v,
      high_minus_low = mean[member == "Higher CCEI"] - mean[member == "Lower CCEI"],
      ttest_p = tt$p.value,
      ks_p = ks$p.value
    )
}))
write_csv(distribution_stats, file.path(table_dir, "figure_distribution_stats.csv"))

plot_stats <- distribution_stats |> filter(outcome == "Istar_ccei")
diff_value <- unique(plot_stats$high_minus_low)
diff_p <- unique(plot_stats$ttest_p)
y_min <- min(0, min(plot_stats$mean - plot_stats$ci) - .02)
y_max <- max(0, max(plot_stats$mean + plot_stats$ci) + .04)

bar <- ggplot(plot_stats, aes(member, mean, fill = member)) +
  geom_col(width = .62, colour = "black", linewidth = .3) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = .16) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = .4) +
  annotate(
    "text", x = 1.5, y = y_max,
    label = paste0("High - low = ", fmt(diff_value), sig(diff_p)), size = 4.4
  ) +
  scale_fill_manual(values = c("Lower CCEI" = "#D99A99", "Higher CCEI" = "#80ADD0")) +
  coord_cartesian(ylim = c(y_min, y_max + .015)) +
  labs(x = NULL, y = expression("Mean placebo-adjusted distance "*(I-M))) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", panel.grid.minor = element_blank())

cdf <- ggplot(figure_data, aes(Istar_ccei, colour = member, linetype = member)) +
  stat_ecdf(geom = "step", linewidth = .85, pad = FALSE) +
  geom_vline(xintercept = 0, colour = "grey65", linewidth = .4) +
  scale_colour_manual(values = c("Lower CCEI" = "red", "Higher CCEI" = "blue")) +
  scale_linetype_manual(values = c("Lower CCEI" = "dashed", "Higher CCEI" = "solid")) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .25), expand = c(0, 0)) +
  labs(x = expression("Placebo-adjusted distance "*(I-M)), y = "Cumulative probability",
       colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = c(.2, .84),
        legend.background = element_rect(fill = "white", colour = "black"))

ggsave(file.path(figure_dir, "ccei_IminusM_by_higher_ccei_bar.png"), bar,
       width = 6, height = 5, dpi = 300)
ggsave(file.path(figure_dir, "ccei_IminusM_by_higher_ccei_cdf.png"), cdf,
       width = 6, height = 5, dpi = 300)

# Coefficient comparison plot for the six main specifications.
coefficient_plot_data <- regressions |>
  filter(measure == "ccei") |>
  mutate(
    definition = if_else(focal == "HighCCEI_both_high", "Higher-CCEI indicator", "CCEI difference"),
    model = recode(outcome_model,
      raw = "Raw I", normalized = "I - M", adjusted = "Raw I, controlling for M"
    ),
    low = beta - 1.96 * se,
    high = beta + 1.96 * se
  )

coefficient_plot <- ggplot(
  coefficient_plot_data,
  aes(specification, beta, colour = model, shape = model)
) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = .35) +
  geom_errorbar(aes(ymin = low, ymax = high), width = .05,
                position = position_dodge(width = .25)) +
  geom_point(size = 2.5, position = position_dodge(width = .25)) +
  facet_wrap(~definition, scales = "free_y") +
  scale_x_continuous(breaks = 1:3, labels = c("Class FE", "Controls", "Individual FE")) +
  labs(x = NULL, y = "Coefficient with 95% confidence interval", colour = NULL, shape = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        axis.text.x = element_text(angle = 15, hjust = 1))
ggsave(file.path(figure_dir, "ccei_coefficient_comparison.png"), coefficient_plot,
       width = 9, height = 5.5, dpi = 300)

# Baseline correlation table with I-M replacing raw revealed-preference distance.
corr_variables <- c(
  ccei_i = "CCEI", RA_i = "Risk attitude", Istar_ccei = "Placebo-adjusted distance",
  inclass_n_friends_i = "Out-degree", inclass_popularity_i = "In-degree",
  male_i = "Male", height_i = "Height", mathscore_i = "Math score",
  RAT_strict_i = "RAT score", outgoing_i = "Outgoing", opened_i = "Openness",
  agreeable_i = "Agreeableness", conscientious_i = "Conscientiousness",
  stable_i = "Emotional stability"
)
baseline <- panel |> filter(post == 0)
corr_rows <- list()
k <- 0L
for (i in seq_along(corr_variables)) {
  for (j in seq_len(min(i - 1L, 5L))) {
    x <- baseline[[names(corr_variables)[i]]]
    y <- baseline[[names(corr_variables)[j]]]
    ok <- complete.cases(x, y)
    test <- cor.test(x[ok], y[ok])
    k <- k + 1L
    corr_rows[[k]] <- tibble(
      row = unname(corr_variables[i]), column = unname(corr_variables[j]),
      correlation = unname(test$estimate), p = test$p.value, N = sum(ok)
    )
  }
}
correlations <- bind_rows(corr_rows)
write_csv(correlations, file.path(table_dir, "baseline_correlations_IminusM.csv"))

# Concise automatically generated review memo.
main_rows <- comparison |>
  filter(specification == 3) |>
  mutate(
    estimate_se = paste0(estimate, " ", standard_error),
    coefficient_over_outcome_sd = fmt(coefficient_over_outcome_sd),
    standardized = fmt(standardized), r2 = fmt(r2)
  ) |>
  select(measure, definition, outcome_model, estimate_se, coefficient_over_outcome_sd,
         standardized, M_coefficient, N, r2)

buffer_rows <- buffers |>
  mutate(
    estimate = paste0(fmt(beta), sig(p), " (", fmt(se), ")"),
    focal = if_else(focal == "HighCCEI_both_high", "Higher CCEI", "CCEI difference")
  ) |>
  select(buffer, focal, specification, estimate, N, r2)

mover_rows <- movers |>
  mutate(
    focal = if_else(focal == "HighCCEI_both_high", "Higher CCEI", "CCEI difference"),
    main = paste0(fmt(beta), sig(p), " (", fmt(se), ")"),
    mover = paste0(fmt(beta_mover), sig(p_mover), " (", fmt(se_mover), ")"),
    interaction = paste0(fmt(beta_interaction), sig(p_interaction), " (", fmt(se_interaction), ")")
  ) |>
  select(focal, specification, main, mover, interaction, N, r2)

alternative_rows <- if (nrow(alternative)) {
  alternative |>
    filter(specification == 3) |>
    mutate(
      definition = case_when(
        grepl("High", focal) ~ "Higher-rationality indicator",
        TRUE ~ "Rationality difference"
      ),
      estimate = paste0(fmt(beta), sig(p), " (", fmt(se), ")"),
      coefficient_over_outcome_sd = fmt(beta / outcome_sd),
      standardized = fmt(standardized),
      M_coefficient = if_else(
        is.na(beta_M), "",
        paste0(fmt(beta_M), " [p(M=1)=", fmt(p_M_equals_one), "]")
      )
    ) |>
    select(measure, definition, outcome_model, estimate, coefficient_over_outcome_sd,
           standardized, M_coefficient, donor_count, N, r2)
} else tibble(status = "Alternative-measure regressions have not completed.")

stability_rows <- if (nrow(stability)) {
  stability |>
    filter(specification == 3) |>
    mutate(estimate = paste0(fmt(beta), sig(p), " (", fmt(se), ")")) |>
    select(measure, benchmark, focal, estimate, N, r2)
} else tibble(status = "Split-half results are generated after both review benchmarks complete.")

sensitivity_rows <- if (nrow(sensitivity)) {
  sensitivity |>
    filter(specification == 3) |>
    mutate(
      focal = if_else(focal == "HighCCEI_both_high", "Higher CCEI", "CCEI difference"),
      estimate = paste0(fmt(beta), sig(p), " (", fmt(se), ")")
    ) |>
    select(measure, benchmark, focal, estimate, standardized, N, r2)
} else tibble(status = "Benchmark-pool sensitivity results are unavailable.")

maxmpi_diagnostic_rows <- if (nrow(maxmpi_diagnostics)) {
  maxmpi_diagnostics |>
    mutate(
      mean_undefined_share = fmt(mean_undefined_share),
      median_undefined_share = fmt(median_undefined_share),
      share_with_all_20_solved = fmt(share_with_all_20_solved)
    )
} else tibble(status = "MaxMPI diagnostics are unavailable.")

memo <- c(
  "# Review results: placebo-adjusted distance",
  "",
  "This file is generated from the isolated review package. No manuscript tables or figures are overwritten.",
  "",
  "## Individual-fixed-effects comparison",
  "",
  markdown_table(as.data.frame(main_rows)),
  "",
  "## Choice-buffer results using I - M",
  "",
  markdown_table(as.data.frame(buffer_rows)),
  "",
  "## Mover results using I - M",
  "",
  markdown_table(as.data.frame(mover_rows)),
  "",
  "## Shorrocks-Shapley decomposition",
  "",
  markdown_table(as.data.frame(shapley |>
    mutate(across(c(shapley_value, shapley_percent, total_r2), ~fmt(.x))) )),
  "",
  "## Alternative revealed-preference measures: individual fixed effects",
  "",
  "HM uses 50 reproducibly sampled donors per target. MaxMPI uses 20 when available. These are review-stage benchmarks; exact full builds are retained as a separate resumable step.",
  "",
  markdown_table(as.data.frame(alternative_rows)),
  "",
  "## Alternative-measure donor-sample stability",
  "",
  markdown_table(as.data.frame(stability_rows)),
  "",
  "## Donor-pool and undefined-donor sensitivity",
  "",
  markdown_table(as.data.frame(sensitivity_rows)),
  "",
  "## MaxMPI review-build diagnostics",
  "",
  "The review build uses 20 sampled donors and a two-second cap per exact cross-cost calculation. Unresolved donor distances receive one half in the main normalized outcome; the `normalized_drop` rows above exclude them.",
  "",
  markdown_table(as.data.frame(maxmpi_diagnostic_rows)),
  "",
  "## Output files",
  "",
  "- `tables/main_comparison.csv`: all raw, normalized, and M-adjusted specifications.",
  "- `tables/figure_distribution_stats.csv`: means, t tests, and KS tests.",
  "- `tables/buffer_results.csv`: exact, 2.5-point, and 5-point choice buffers.",
  "- `tables/mover_results.csv`: mover and interaction specifications.",
  "- `tables/baseline_correlations_IminusM.csv`: Section 5.1 correlation inputs.",
  "- `tables/alternative_measure_comparison.csv`: HM and, when complete, MaxMPI comparisons.",
  "- `tables/alternative_sample_stability.csv`: split-half donor-sample checks.",
  "- `tables/benchmark_sensitivity.csv`: undefined-donor and donor-pool checks.",
  "- `tables/maxmpi_review_diagnostics.csv`: timeout/undefined rates for the provisional MaxMPI benchmark.",
  "- `figures/ccei_coefficient_comparison.png`: side-by-side main estimates.",
  "- `figures/ccei_IminusM_by_higher_ccei_bar.png` and `...cdf.png`: candidate main figure."
)
writeLines(memo, file.path(out_dir, "REVIEW_RESULTS.md"))

cat("Review tables, figures, and memo written under:", out_dir, "\n")
