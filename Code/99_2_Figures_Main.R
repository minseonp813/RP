################################################################################
# 99_Figures_Main.R
# Main-paper figures only. Existing 99_* files are intentionally left unchanged.
#
# Run this file from C:/Users/hahn0/RP/Code, or source it from RStudio.
################################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(haven)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
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
# Figure 2: Self-Reported Influence And Revealed-Preference Distance
################################################################################

figure2_data <- panel_individual |>
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
    scale_x_discrete(labels = setNames(plot_data$axis_label, plot_data$response)) +
    scale_y_continuous(
      limits = c(0, 0.8),
      breaks = seq(0, 0.8, 0.2),
      minor_breaks = seq(0.1, 0.7, 0.2)
    ) +
    scale_fill_manual(values = fill_values) +
    labs(
      x = NULL,
      y = "Mean Revealed Preference Distance"
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
  "1" = "Very Differently",
  "2" = "Somewhat Differently",
  "3" = "Somewhat Similar",
  "4" = "Mostly Similar"
)

make_figure2_pair <- function(outcome_var, output_dir) {
  pooled_full <- figure2_data |>
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

make_figure2_pair("I_ig", result_dir)
make_figure2_pair("Ihat_ig", ihat_result_dir)

################################################################################
# Figure 3: Revealed Preference Distance Index By Members' CCEI
################################################################################

rp_data <- panel_individual |>
  filter(!is.na(I_ig), !is.na(HighCCEI)) |>
  mutate(
    ccei_group = factor(
      if_else(HighCCEI == 1, "Higher CCEI", "Lower CCEI"),
      levels = c("Lower CCEI", "Higher CCEI")
    )
  )

rp_stats <- rp_data |>
  group_by(ccei_group) |>
  summarise(
    mean = mean(I_ig),
    sd = sd(I_ig),
    n = n(),
    se = sd / sqrt(n),
    ci = qt(0.975, n - 1) * se,
    .groups = "drop"
  )

rp_test <- t.test(I_ig ~ ccei_group, data = rp_data)
rp_difference <- diff(rev(rp_stats$mean))
rp_label <- sprintf(
  "Diff. = %.3f%s",
  rp_difference,
  sig_mark(rp_test$p.value)
)
rp_y <- max(rp_stats$mean + rp_stats$ci) + 0.08

rp_bar <- ggplot(rp_stats, aes(ccei_group, mean, fill = ccei_group)) +
  geom_col(width = 0.62, colour = "black", linewidth = 0.3) +
  geom_errorbar(
    aes(ymin = mean - ci, ymax = mean + ci),
    width = 0.16, linewidth = 0.45
  ) +
  annotate("segment", x = 1, xend = 2, y = rp_y, yend = rp_y) +
  annotate("segment", x = 1, xend = 1, y = rp_y - 0.025, yend = rp_y) +
  annotate("segment", x = 2, xend = 2, y = rp_y - 0.025, yend = rp_y) +
  annotate("text", x = 1.5, y = rp_y + 0.04, label = rp_label, size = 5) +
  scale_fill_manual(values = c("Lower CCEI" = "#D99A99", "Higher CCEI" = "#80ADD0")) +
  scale_x_discrete(labels = c("Lower\nCCEI", "Higher\nCCEI")) +
  scale_y_continuous(limits = c(0, rp_y + 0.09), expand = c(0, 0)) +
  labs(x = NULL, y = expression("Mean revealed preference distance (" * I[ig] * ")")) +
  paper_theme +
  theme(legend.position = "none")

rp_cdf <- ggplot(rp_data, aes(I_ig, colour = ccei_group, linetype = ccei_group)) +
  stat_ecdf(geom = "step", linewidth = 0.9, pad = FALSE) +
  scale_colour_manual(values = c("Lower CCEI" = "red", "Higher CCEI" = "blue")) +
  scale_linetype_manual(values = c("Lower CCEI" = "dashed", "Higher CCEI" = "solid")) +
  scale_x_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
  ) +
  labs(
    x = expression("Revealed preference distance (" * I[ig] * ")"),
    y = "Cumulative probability",
    colour = NULL,
    linetype = NULL
  ) +
  paper_theme +
  theme(
    legend.position = c(0.22, 0.85),
    legend.background = element_rect(fill = "white", colour = "black")
  )

ggsave(
  file.path(result_dir, "bargaining_index_by_ccei_bar.png"),
  rp_bar, width = 6, height = 5, dpi = 300
)
ggsave(
  file.path(result_dir, "bargaining_index_by_ccei_cdf.png"),
  rp_cdf, width = 6, height = 5, dpi = 300
)

################################################################################
# Figure 5: Collective CCEI By Members' Individual CCEI Category
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

message("99_2_Figures_Main.R completed. Outputs: ", result_dir)
