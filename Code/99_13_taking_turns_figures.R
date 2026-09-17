# Taking-turns figures.
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
code_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  normalizePath(getwd())
}
.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(dplyr)
library(ggplot2)
result_dir <- file.path(code_dir, "results", "taking_turns_permutation")
figure_dir <- file.path(code_dir, "results", "figures", "taking_turns")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
summary <- read.csv(file.path(result_dir, "taking_turns_summary.csv"))
cdf_data <- read.csv(file.path(result_dir, "taking_turns_cdf_data.csv"))
ks_results <- read.csv(file.path(result_dir, "ks_tests_by_draw.csv"))
expected_reps <- unique(summary$n_permutations)
stopifnot(length(expected_reps) == 1,
          all(table(ks_results$split, ks_results$sample) == expected_reps),
          all(is.finite(ks_results$p_value)),
          all(ks_results$p_value >= 0 & ks_results$p_value <= 1))

ks_bins <- ks_results %>%
  mutate(bin = cut(p_value, breaks = seq(0, 1, .05),
                   include.lowest = TRUE, labels = FALSE)) %>%
  count(split, sample, bin) %>%
  mutate(midpoint = (bin - .5) * .05, percent = 100 * n / expected_reps)
stopifnot(all(ks_bins %>% group_by(split, sample) %>%
                summarise(total = sum(percent), .groups = "drop") %>% pull(total) == 100))
hist_max <- ceiling(max(ks_bins$percent) / 5) * 5

cdf_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey92"),
        panel.border = element_rect(colour = "grey92", fill = NA),
        axis.text = element_text(colour = "black"),
        legend.position = "inside", legend.position.inside = c(.06, .94),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key = element_rect(fill = "white", colour = NA),
        legend.title = element_blank(), legend.margin = margin(4, 5, 4, 5),
        plot.margin = margin(8, 14, 8, 8))
hist_theme <- theme_classic(base_size = 12) +
  theme(panel.grid.major = element_line(colour = "grey94", linetype = "dashed"),
        axis.text = element_text(colour = "black"))

for (sample_name in c("All pair-waves", "Bottom 25%")) {
  suffix <- if (sample_name == "All pair-waves") "all" else "bottom25"
  d <- filter(cdf_data, sample == sample_name)
  for (split_name in c("odd_even", "first_last")) {
    observed_col <- if (split_name == "odd_even") "observed" else "first_last"
    stem <- if (split_name == "odd_even") "taking_turns" else "taking_turns_first_last"
    curves <- bind_rows(
      transmute(d, gap, cdf = .data[[observed_col]], series = "Observed"),
      transmute(d, gap, cdf = placebo, series = "Mean placebo")) %>%
      mutate(series = factor(series, levels = c("Observed", "Mean placebo")))
    p <- ggplot(d, aes(gap)) +
      geom_ribbon(aes(ymin = placebo_low, ymax = placebo_high),
                  fill = "grey80", alpha = .5) +
      geom_line(data = curves, aes(y = cdf, colour = series, linetype = series),
                linewidth = .8) +
      scale_colour_manual(values = c("Observed" = "blue", "Mean placebo" = "red")) +
      scale_linetype_manual(values = c("Observed" = "solid", "Mean placebo" = "dashed")) +
      scale_x_continuous(breaks = seq(0, 1, .2), limits = c(0, 1), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0, 1),
                         labels = function(x) sprintf("%.1f", x), expand = c(0, 0)) +
      labs(x = "Absolute split-index difference", y = "Cumulative probability") +
      cdf_theme
    ggsave(file.path(figure_dir, paste0(stem, "_cdf_", suffix, ".png")),
           p, width = 5.4, height = 4.5, dpi = 300, bg = "white")

    ks_label <- if (split_name == "odd_even") "Odd-even" else "First 9 vs last 9"
    h <- filter(ks_bins, sample == sample_name, split == ks_label)
    p <- ggplot(h, aes(midpoint, percent)) +
      geom_col(width = .05, fill = "#8CC5E3", colour = "#0099CC", linewidth = .3) +
      scale_x_continuous(breaks = seq(0, 1, .1), limits = c(0, 1),
                         expand = expansion(mult = c(.01, .01))) +
      scale_y_continuous(breaks = seq(0, hist_max, 5), limits = c(0, hist_max),
                         expand = expansion(mult = c(0, .02))) +
      labs(x = "KS-test p-value", y = "Percent") + hist_theme
    ggsave(file.path(figure_dir, paste0(stem, "_ks_pvalues_", suffix, ".png")),
           p, width = 5.4, height = 4.5, dpi = 300, bg = "white")
  }
}
message("Saved eight panels in: ", figure_dir)
