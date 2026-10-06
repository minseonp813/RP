################################################################################
# 07_Figures_Main.R
# Main-paper figures.
#
# Run from Code, or source this file from RStudio.
################################################################################

rm(list = ls())

suppressPackageStartupMessages({
  library(haven)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(grid)
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

################################################################################
# Figure 6: Joint Collective Outcomes
################################################################################

# Saved inputs are generated by 11_collective_quality.do.
# To run this section alone, load haven/ggplot2 and set code_dir to the Code folder.
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
joint_counts$label <- sprintf("%s pair-waves\n%.1f%%", format(joint_counts$n, trim = TRUE), 100 * joint_counts$share)

# Stripes and dots distinguish the two cells of each colour without extra packages.
joint_stripes <- do.call(rbind, lapply(c(1, 3), function(i) {
  offset <- seq(-.9, .9, .12)
  start <- pmax(-.5, -.5 - offset)
  end <- pmin(.5, .5 - offset)
  data.frame(x = joint_counts$ccei_one[i] + start, xend = joint_counts$ccei_one[i] + end,
             y = joint_counts$ceiv_one[i] + start + offset,
             yend = joint_counts$ceiv_one[i] + end + offset)
}))
joint_dots <- do.call(rbind, lapply(c(2, 4), function(i) {
  points <- expand.grid(x = seq(-.42, .42, .12), y = seq(-.42, .42, .12))
  transform(points, x = x + joint_counts$ccei_one[i], y = y + joint_counts$ceiv_one[i])
}))
joint_plot <- ggplot(joint_counts, aes(ccei_one, ceiv_one)) +
  geom_tile(aes(fill = factor(outcome)), width = 1, height = 1) +
  geom_segment(data = joint_stripes, aes(x, y, xend = xend, yend = yend),
               inherit.aes = FALSE, colour = "grey20", alpha = .35, linewidth = .35) +
  geom_point(data = joint_dots, aes(x, y), inherit.aes = FALSE,
             colour = "grey20", alpha = .4, size = .7) +
  geom_vline(xintercept = .5, colour = "grey25", linewidth = .7) +
  geom_hline(yintercept = .5, colour = "grey25", linewidth = .7) +
  geom_label(aes(label = label, fill = factor(outcome)), size = 6.5,
             lineheight = 1.4, colour = "grey15", linewidth = 0,
             label.padding = grid::unit(.3, "lines")) +
  scale_fill_manual(values = c("#80ADD0", "white", "white", "#80ADD0")) +
  scale_x_continuous(breaks = 0:1, labels = c("CCEI < 1", "CCEI = 1"),
                     limits = c(-.5, 1.5), expand = c(0, 0)) +
  scale_y_continuous(breaks = 0:1, labels = c("CEIV < 1", "CEIV = 1"),
                     limits = c(-.5, 1.5), expand = c(0, 0)) +
  coord_fixed() +
  labs(x = "Group CCEI", y = "Group CEIV") +
  theme_classic(base_size = 17) +
  theme(legend.position = "none", axis.line = element_blank(), axis.ticks = element_blank(),
        axis.text = element_text(colour = "grey15"),
        axis.text.x = element_text(margin = margin(t = 12)),
        axis.text.y = element_text(margin = margin(r = 12)),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        panel.border = element_rect(colour = "grey25", fill = NA, linewidth = .7),
        plot.margin = margin(15, 15, 15, 15))
figure6_file <- file.path(collective_result_dir, "joint_outcome_quadrants.pdf")
ggsave(figure6_file, joint_plot, width = 7, height = 6.3)
print(joint_counts)

draft_figure_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(draft_figure_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(figure6_file, file.path(draft_figure_dir, basename(figure6_file)),
                   overwrite = TRUE))

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
