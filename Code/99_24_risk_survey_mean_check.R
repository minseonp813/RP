# Nine descriptive panels: collective CCEI, CEIV and raw RP distance by response.
# Also plots response distributions by strictly higher individual CCEI.
# Run with Rscript Code/99_24_risk_survey_mean_check.R, or from Code in RStudio.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
code_dir <- if (length(file_arg) == 1L) {
  dirname(normalizePath(sub("^--file=", "", file_arg)))
} else {
  getwd()
}
.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

panel <- read_dta(file.path(code_dir, "data/panel_individual.dta")) |>
  select(group_id, post, id, class, ccei_i, ccei_j, ccei_g, Ihat_ig,
         starts_with("risk_"))
collective <- read_dta(file.path(code_dir, "data/panel_group_new_indices.dta")) |>
  select(group_id, post, ceiv_g)
stopifnot(!anyDuplicated(collective[c("group_id", "post")]))
panel <- left_join(panel, collective, by = c("group_id", "post"))
stopifnot(nrow(panel) == 2608L, !anyNA(panel$ceiv_g), !anyNA(panel$ccei_g))

questions <- c("Cooperation score", "Similarity to individual choices",
               "Whose suggestions were reflected?")
variables <- c("risk_cooperation_i", "risk_similar_i", "risk_whose_i")
response_labels <- list(
  as.character(1:5),
  c("Very\ndifferent", "Somewhat\ndifferent", "Somewhat\nsimilar", "Mostly\nsimilar"),
  c("Mostly\npartner's", "Both", "Mostly\nmine", "Neither")
)

means <- bind_rows(lapply(seq_along(variables), function(q) {
  bind_rows(lapply(c("ccei_g", "ceiv_g", "Ihat_ig"), function(outcome) {
    sample <- panel |>
      transmute(class, response = .data[[variables[q]]], value = .data[[outcome]]) |>
      filter(!is.na(response), !is.na(value))
    stopifnot(all(sample$response %in% seq_along(response_labels[[q]])))
    fit <- fixest::feols(value ~ 0 + factor(response), data = sample,
                         vcov = ~class, nthreads = 1, notes = FALSE)
    counts <- count(sample, response) |> arrange(response)
    counts$mean <- unname(coef(fit))
    counts$se <- unname(fixest::se(fit))
    critical <- qt(.975, n_distinct(sample$class) - 1L)
    counts |>
      mutate(question = questions[q], outcome = outcome,
             label = response_labels[[q]][response],
             low = mean - critical * se, high = mean + critical * se,
             total = nrow(sample), clusters = n_distinct(sample$class))
  }))
}))

# Question-specific factor levels keep response categories in questionnaire order.
means <- means |>
  mutate(question = factor(question, levels = questions),
         outcome = factor(outcome, levels = c("ccei_g", "ceiv_g", "Ihat_ig"),
                          labels = c("Collective CCEI", "Collective CEIV", "RP distance")),
         axis_key = paste(question, response, sep = ":"))
axis_order <- means |> distinct(question, response, axis_key, label) |>
  arrange(question, response)
means$axis_key <- factor(means$axis_key, levels = axis_order$axis_key)

plot <- ggplot(means, aes(axis_key, mean)) +
  geom_errorbar(aes(ymin = low, ymax = high), width = .14,
                colour = "#4C83A6", linewidth = .6) +
  geom_point(colour = "#4C83A6", size = 2.8) +
  geom_text(aes(label = paste0("n=", n)), y = -Inf, vjust = -.6, size = 3) +
  facet_grid(outcome ~ question, scales = "free") +
  scale_x_discrete(labels = setNames(axis_order$label, axis_order$axis_key)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 4),
                     expand = expansion(mult = c(.18, .1))) +
  labs(x = NULL, y = "Mean index",
       title = "Collective CCEI, CEIV and revealed-preference distance by risk-survey response",
       subtitle = "Both waves pooled; all observed responses",
       caption = paste0("Points: unadjusted means. Bars: 95% confidence intervals clustered by class (64 classes).\n",
                        "Counts are respondent-waves; samples use each measure's observed values. CCEI/CEIV are shared group outcomes; distance is member-specific.\n",
                        "Distance is raw I_ig: larger values mean farther from the group's choices. Cooperation uses the cleaned score (6 minus Risk_q1).")) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "#EDF4F8"),
        axis.text.x = element_text(size = 10), plot.caption = element_text(hjust = 0),
        panel.spacing = grid::unit(1.2, "lines"))

output_dir <- file.path(code_dir, "results/risk_survey_mean_check")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(means, file.path(output_dir, "response_means.csv"))
ggsave(file.path(output_dir, "nine_panel_means.png"), plot, width = 15, height = 11,
       dpi = 220, bg = "white")
ggsave(file.path(output_dir, "nine_panel_means.pdf"), plot, width = 15, height = 11)
print(select(means, question, outcome, response, n, mean, low, high), n = Inf)

response_shares <- bind_rows(lapply(seq_along(variables), function(q) {
  panel |>
    transmute(response = .data[[variables[q]]],
              rationality = factor(ifelse(ccei_i > ccei_j, "Higher CCEI", "Not higher CCEI"),
                                   levels = c("Higher CCEI", "Not higher CCEI"))) |>
    filter(!is.na(response), !is.na(rationality)) |>
    count(rationality, response) |>
    tidyr::complete(rationality, response = seq_along(response_labels[[q]]),
                    fill = list(n = 0L)) |>
    group_by(rationality) |>
    mutate(total = sum(n), percent = 100 * n / total) |>
    ungroup() |>
    mutate(question = factor(questions[q], levels = questions),
           axis_key = factor(paste(question, response, sep = ":"),
                             levels = axis_order$axis_key))
}))
totals <- response_shares |>
  group_by(question, rationality) |>
  summarise(percent = sum(percent), .groups = "drop")
stopifnot(all(abs(totals$percent - 100) < 1e-10))

distribution_plot <- ggplot(response_shares, aes(axis_key, percent, fill = rationality)) +
  geom_col(position = position_dodge(width = .8), width = .72) +
  geom_text(aes(label = sprintf("%.1f", percent)),
            position = position_dodge(width = .8), vjust = -.4, size = 3.2) +
  facet_grid(. ~ question, scales = "free_x") +
  scale_x_discrete(labels = setNames(axis_order$label, axis_order$axis_key)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, .12))) +
  scale_fill_manual(values = c("Higher CCEI" = "#4C83A6", "Not higher CCEI" = "#B8C4CC")) +
  labs(x = NULL, y = "Share of responses", fill = NULL,
       title = "Risk-survey responses by individual rationality relative to the partner",
       subtitle = "Higher CCEI: CCEI_i > CCEI_j. Not higher CCEI includes ties. Both waves pooled.",
       caption = paste0("Percentages sum to 100% within each rationality group and question; all observed responses are included.\n",
                        "Cooperation uses the cleaned score (6 minus Risk_q1); original response anchors are not documented.")) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "#EDF4F8"),
        legend.position = "bottom", axis.text.x = element_text(size = 10),
        plot.caption = element_text(hjust = 0), panel.spacing = grid::unit(1.2, "lines"))
readr::write_csv(response_shares, file.path(output_dir, "response_shares_by_relative_ccei.csv"))
ggsave(file.path(output_dir, "response_distribution_by_relative_ccei.png"),
       distribution_plot, width = 15, height = 5.5, dpi = 220, bg = "white")
ggsave(file.path(output_dir, "response_distribution_by_relative_ccei.pdf"),
       distribution_plot, width = 15, height = 5.5)
print(response_shares, n = Inf)
cat("Tied CCEI respondent-waves:", sum(panel$ccei_i == panel$ccei_j, na.rm = TRUE), "\n")
