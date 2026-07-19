##########################################################
# Written By Byunghun Hahn, Nov 2025
# Replication Codes for All Figures
# Last Updated By Minseon Park/Claude Code: 2026-07-19

# Inputs:
# - ../Code_Replication Package_Upload/data/panel_individual.dta
# - data/finalized_panel_individual_251206.dta (Figure 2 survey responses only)


# Outputs:
# - results/ccei_bargaining_whose_suggestion.png
# - results/ccei_bargaining_had_individual_high.png
# - results/bargaining_index_by_ccei_bar.png
# - results/bargaining_index_by_ccei_cdf.png
# - results/group_ccei_by_member_ccei_median_bar.png
# - results/group_ccei_by_member_ccei_median_cdf.png
##########################################################

library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)

replication_panel_path <- "../Code_Replication Package_Upload/data/panel_individual.dta"
validation_responses_path <- "data/finalized_panel_individual_251206.dta"

panel_individual <- read_dta(replication_panel_path)


#########################################################
### 1. FIGURE 2 LEFT
#########################################################

validation_responses <- read_dta(validation_responses_path) %>%
  transmute(
    id = as.character(id),
    post,
    risk_q2_i,
    risk_q3_i
  )

df <- panel_individual %>%
  mutate(id = as.character(id)) %>%
  left_join(validation_responses, by = c("id", "post")) %>%
  filter(!is.na(new2_I_ig))

df <- df %>%
  mutate(RA_dif = abs(RA_i - RA_j))

med <- median(df$RA_dif, na.rm = TRUE)

df <- df %>%
  mutate(RA_dif_high = ifelse(RA_dif >= med, 1, 0))
# High preference-difference panels use pairs at or above the median
# absolute difference in members' risk-aversion measures.


risk_q3_labels <- c(
  "1" = "Mostly Partner's",
  "2" = "Both",
  "3" = "Mostly Mine",
  "4" = "Neither"
)

generate_fig2_left_plot <- function(data) {
  fig_a <- data %>%
    filter(risk_q3_i %in% 1:4) %>%
    group_by(risk_q3_i) %>%
    summarise(
      mean = mean(new2_I_ig),
      sd   = sd(new2_I_ig),
      n    = n(),
      .groups = "drop"
    ) %>%
    mutate(
      se  = sd / sqrt(n),
      hi  = mean + qt(0.975, df = n - 1) * se,
      low = mean - qt(0.975, df = n - 1) * se,
      pct = round(100 * n / sum(n), 1)
    )

  labels_fig_a <- fig_a %>%
    mutate(label = paste0(
      risk_q3_labels[as.character(risk_q3_i)],
      "\n(N=", n, ", ", pct, "%)"
    )) %>%
    select(risk_q3_i, label)

  label_map <- setNames(labels_fig_a$label, labels_fig_a$risk_q3_i)

  ggplot(fig_a, aes(x = factor(risk_q3_i), y = mean, fill = factor(risk_q3_i))) +
    geom_col() +
    geom_errorbar(aes(ymin = low, ymax = hi), width = 0.15) +
    labs(
      y = "Mean Revealed Preference Difference",
      x = "Whose Suggestion"
    ) +
    scale_x_discrete(labels = label_map) +
    scale_y_continuous(
      breaks = seq(0, 0.8, 0.2),
      limits = c(0, 0.8)
    ) +
    scale_fill_manual(values = c(
      "1" = "lightblue",
      "2" = "lightblue",
      "3" = "lightblue",
      "4" = "grey90"
    )) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
}

fig2_left_variants <- list(
  list(
    data = df,
    file = "results/ccei_bargaining_whose_suggestion.png"
  ),
  list(
    data = df %>% filter(RA_dif_high == 1),
    file = "results/ccei_bargaining_whose_suggestion_high_RA_diff.png"
  ),
  list(
    data = df %>% filter(RA_dif_high == 0),
    file = "results/ccei_bargaining_whose_suggestion_low_RA_diff.png"
  )
)

for (spec in fig2_left_variants) {
  plot_obj <- generate_fig2_left_plot(spec$data)
  ggsave(spec$file, plot_obj,
         width = 7, height = 5, dpi = 300)
}


#########################################################
### 2. FIGURE 2 RIGHT — RA Difference Split
#########################################################

risk_q2_labels <- c(
  "1" = "Very Differently",
  "2" = "Somewhat Differently",
  "3" = "Somewhat Similar",
  "4" = "Mostly Similar"
)

generate_fig2_right_plot <- function(data) {
  fig_a <- data %>%
    filter(risk_q2_i %in% 1:4) %>%
    group_by(risk_q2_i) %>%
    summarise(
      mean = mean(new2_I_ig),
      sd   = sd(new2_I_ig),
      n    = n(),
      .groups = "drop"
    ) %>%
    mutate(
      se  = sd / sqrt(n),
      hi  = mean + qt(0.975, df = n - 1) * se,
      low = mean - qt(0.975, df = n - 1) * se,
      pct = round(100 * n / sum(n), 1)
    )

  labels_fig_a <- fig_a %>%
    mutate(label = paste0(
      risk_q2_labels[as.character(risk_q2_i)],
      "\n(N=", n, ", ", pct, "%)"
    )) %>%
    select(risk_q2_i, label)

  label_map <- setNames(labels_fig_a$label, labels_fig_a$risk_q2_i)

  ggplot(fig_a, aes(x = factor(risk_q2_i), y = mean, fill = factor(risk_q2_i))) +
    geom_col() +
    geom_errorbar(aes(ymin = low, ymax = hi), width = 0.15) +
    labs(
      y = "Mean Revealed Preference Difference",
      x = "Had Individually Decided"
    ) +
    scale_x_discrete(labels = label_map) +
    scale_y_continuous(
      breaks = seq(0, 0.8, 0.2),
      limits = c(0, 0.8)
    ) +
    scale_fill_manual(values = c(
      "1" = "lightblue",
      "2" = "lightblue",
      "3" = "lightblue",
      "4" = "lightblue"
    )) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
}

fig2_right_variants <- list(
  list(
    data = df,
    file = "results/ccei_bargaining_had_individual.png"
  ),
  list(
    data = df %>% filter(RA_dif_high == 1),
    file = "results/ccei_bargaining_had_individual_high.png"
  ),
  list(
    data = df %>% filter(RA_dif_high == 0),
    file = "results/ccei_bargaining_had_individual_low_RA_diff.png"
  )
)

for (spec in fig2_right_variants) {
  plot_obj <- generate_fig2_right_plot(spec$data)
  ggsave(spec$file, plot_obj,
         width = 7, height = 5, dpi = 300)
}


##############################################################
# Figure 3 - Revealed Preference Distance Index by Members' CCEI
##############################################################

rp_distance_data <- panel_individual %>%
  filter(!is.na(new2_I_ig), !is.na(HighCCEI), !is.na(post)) %>%
  mutate(
    ccei_group = factor(
      ifelse(HighCCEI == 1, "Higher CCEI", "Lower CCEI"),
      levels = c("Lower CCEI", "Higher CCEI")
    )
  )

rp_distance_stats <- rp_distance_data %>%
  group_by(ccei_group) %>%
  summarise(
    mean = mean(new2_I_ig),
    sd = sd(new2_I_ig),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd / sqrt(n),
    ci = qt(0.975, df = pmax(1, n - 1)) * se
  )

rp_distance_diff <- rp_distance_data %>%
  select(group_id, post, ccei_group, new2_I_ig) %>%
  pivot_wider(names_from = ccei_group, values_from = new2_I_ig) %>%
  mutate(pairwise_difference = `Lower CCEI` - `Higher CCEI`) %>%
  summarise(
    difference = mean(pairwise_difference),
    p_value = t.test(pairwise_difference)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    stars = case_when(
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.10 ~ "+",
      TRUE ~ ""
    ),
      label = sprintf("Diff. = %.3f%s", difference, stars)
  )

rp_y_top <- max(rp_distance_stats$mean + rp_distance_stats$ci, na.rm = TRUE) + 0.08

rp_bar_plot <- ggplot(rp_distance_stats, aes(x = ccei_group, y = mean, fill = ccei_group)) +
  geom_col(width = 0.62, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.16, linewidth = 0.45) +
  annotate("segment", x = 1, xend = 2, y = rp_y_top, yend = rp_y_top, linewidth = 0.45) +
  annotate("segment", x = 1, xend = 1, y = rp_y_top - 0.025, yend = rp_y_top, linewidth = 0.45) +
  annotate("segment", x = 2, xend = 2, y = rp_y_top - 0.025, yend = rp_y_top, linewidth = 0.45) +
  annotate("text", x = 1.5, y = rp_y_top + 0.04, label = rp_distance_diff$label[1], size = 18 / .pt) +
  scale_fill_manual(values = c(
    "Lower CCEI" = "#E39695",
    "Higher CCEI" = "#74A9CF"
  )) +
  scale_x_discrete(labels = c(
    "Lower CCEI" = "Lower\nCCEI",
    "Higher CCEI" = "Higher\nCCEI"
  )) +
  labs(x = NULL, y = expression("Mean revealed preference distance (" * I[ig] * ")")) +
  scale_y_continuous(
    limits = c(0, 0.95),
    breaks = seq(0, 0.9, by = 0.1),
    expand = c(0, 0)
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

rp_cdf_plot <- ggplot(rp_distance_data, aes(x = new2_I_ig, color = ccei_group, linetype = ccei_group)) +
  stat_ecdf(geom = "step", linewidth = 0.9) +
  scale_color_manual(values = c(
    "Lower CCEI" = "red",
    "Higher CCEI" = "blue"
  )) +
  scale_linetype_manual(values = c(
    "Lower CCEI" = "dashed",
    "Higher CCEI" = "solid"
  )) +
  guides(
    color = guide_legend(ncol = 1, byrow = TRUE),
    linetype = guide_legend(ncol = 1, byrow = TRUE)
  ) +
  labs(
    x = expression("Revealed preference distance (" * I[ig] * ")"),
    y = "Cumulative probability",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(
    limits = c(0, 1.0022),
    breaks = seq(0, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.03),
    breaks = seq(0.0, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(1.2, "lines"),
    legend.text = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results/bargaining_index_by_ccei_bar.png", rp_bar_plot,
       width = 6, height = 5, dpi = 300)

ggsave("results/bargaining_index_by_ccei_cdf.png", rp_cdf_plot,
  width = 6, height = 5, dpi = 300)


####################################################################
# Figure 5: Collective CCEI by members' median-CCEI category
#########################################################

# Use pooled individual observations to define the sample median CCEI.
ccei_median <- panel_individual %>%
  summarise(med = median(ccei_i, na.rm = TRUE)) %>%
  pull(med)

pair_wave <- panel_individual %>%
  filter(!is.na(group_id), !is.na(post), !is.na(ccei_i), !is.na(ccei_g)) %>%
  group_by(group_id, post) %>%
  summarise(
    ccei_g = first(ccei_g),
    n_members = n(),
    n_high = sum(ccei_i >= ccei_median),
    .groups = "drop"
  ) %>%
  filter(n_members == 2) %>%
  mutate(
    pair_ccei_group = factor(
      n_high,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

bar_stats <- pair_wave %>%
  group_by(pair_ccei_group) %>%
  summarise(
    mean = mean(ccei_g, na.rm = TRUE),
    sd = sd(ccei_g, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci = qt(0.975, df = pmax(1, n - 1)) * se,
    .groups = "drop"
  )

diff_stats <- pair_wave %>%
  filter(pair_ccei_group %in% c("(Low, Low)", "(High, High)")) %>%
  summarise(
    diff = mean(ccei_g[pair_ccei_group == "(High, High)"]) -
      mean(ccei_g[pair_ccei_group == "(Low, Low)"]),
    p_value = t.test(ccei_g ~ pair_ccei_group)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    stars = case_when(
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.10 ~ "+",
      TRUE ~ ""
    ),
    label = sprintf("Diff. = %.3f%s", diff, stars)
  )

diff_stats_lowhigh <- pair_wave %>%
  filter(pair_ccei_group %in% c("(Low, Low)", "(Low, High)")) %>%
  summarise(
    diff = mean(ccei_g[pair_ccei_group == "(Low, High)"]) -
      mean(ccei_g[pair_ccei_group == "(Low, Low)"]),
    p_value = t.test(ccei_g ~ pair_ccei_group)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    stars = case_when(
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.10 ~ "+",
      TRUE ~ ""
    ),
    label = sprintf("Diff. = %.3f%s", diff, stars)
  )

diff_stats_highhigh <- pair_wave %>%
  filter(pair_ccei_group %in% c("(Low, High)", "(High, High)")) %>%
  summarise(
    diff = mean(ccei_g[pair_ccei_group == "(High, High)"]) -
      mean(ccei_g[pair_ccei_group == "(Low, High)"]),
    p_value = t.test(ccei_g ~ pair_ccei_group)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    stars = case_when(
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.10 ~ "+",
      TRUE ~ ""
    ),
    label = sprintf("Diff. = %.3f%s", diff, stars)
  )

bar_y_min <- max(0.75, floor((min(bar_stats$mean - bar_stats$ci, na.rm = TRUE) - 0.02) * 100) / 100)
bar_y_top <- max(bar_stats$mean + bar_stats$ci, na.rm = TRUE)

brackets <- bar_stats %>%
  summarise(y = bar_y_top + 0.07) %>%
  bind_cols(diff_stats)

brackets_lowhigh <- bar_stats %>%
  summarise(y = bar_y_top + 0.025) %>%
  bind_cols(diff_stats_lowhigh)

brackets_highhigh <- bar_stats %>%
  summarise(y = bar_y_top + 0.085) %>%
  bind_cols(diff_stats_highhigh)

bar_plot <- ggplot(bar_stats, aes(x = pair_ccei_group, y = mean, fill = pair_ccei_group)) +
  geom_col(width = 0.62, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
                width = 0.16, linewidth = 0.45) +
  geom_segment(data = brackets,
               aes(x = "(Low, Low)", xend = "(High, High)", y = y, yend = y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_segment(data = brackets,
               aes(x = "(Low, Low)", xend = "(Low, Low)", y = y - 0.02, yend = y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_segment(data = brackets,
               aes(x = "(High, High)", xend = "(High, High)", y = y - 0.02, yend = y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_label(data = brackets, aes(x = 2, y = y + 0.012, label = label),
             inherit.aes = FALSE, size = 18 / .pt, label.size = 0, fill = "white") +
  geom_segment(data = brackets_lowhigh,
               aes(x = "(Low, Low)", xend = "(Low, High)", y = y, yend = y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_segment(data = brackets_lowhigh,
               aes(x = "(Low, Low)", xend = "(Low, Low)", y = y - 0.02, yend = y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_segment(data = brackets_lowhigh,
               aes(x = "(Low, High)", xend = "(Low, High)", y = y - 0.02, yend = y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_label(data = brackets_lowhigh, aes(x = 1.48, y = y + 0.012, label = label),
             inherit.aes = FALSE, size = 18 / .pt, label.size = 0, fill = "white") +
  geom_segment(data = brackets_highhigh,
               aes(x = "(Low, High)", xend = "(High, High)", y = brackets_lowhigh$y, yend = brackets_lowhigh$y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_segment(data = brackets_highhigh,
               aes(x = "(Low, High)", xend = "(Low, High)", y = brackets_lowhigh$y - 0.02, yend = brackets_lowhigh$y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_segment(data = brackets_highhigh,
               aes(x = "(High, High)", xend = "(High, High)", y = brackets_lowhigh$y - 0.02, yend = brackets_lowhigh$y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_label(data = brackets_highhigh, aes(x = 2.58, y = brackets_lowhigh$y + 0.012, label = label),
             inherit.aes = FALSE, size = 17 / .pt, label.size = 0, fill = "white") +
  scale_fill_manual(values = c(
    "(Low, Low)" = "#E39695",
    "(Low, High)" = "#D8C98C",
    "(High, High)" = "#74A9CF"
  )) +
  scale_x_discrete(labels = c(
    "(Low, Low)" = "Low\nLow",
    "(Low, High)" = "Low\nHigh",
    "(High, High)" = "High\nHigh"
  )) +
  labs(x = NULL, y = "Mean collective CCEI") +
  scale_y_continuous(
    breaks = seq(0.85, 1.00, by = 0.05),
    labels = scales::label_number(accuracy = 0.01),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(bar_y_min, 1.08)) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

cdf_plot <- ggplot(pair_wave, aes(x = ccei_g, color = pair_ccei_group, linetype = pair_ccei_group)) +
  stat_ecdf(geom = "step", linewidth = 0.9) +
  scale_color_manual(values = c(
    "(Low, Low)" = "red",
    "(Low, High)" = "#9E8F4A",
    "(High, High)" = "blue"
  )) +
  scale_linetype_manual(values = c(
    "(Low, Low)" = "dashed",
    "(Low, High)" = "dotdash",
    "(High, High)" = "solid"
  )) +
  labs(
    x = "Collective CCEI",
    y = "Cumulative probability",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0.1, 1.0022),
                     breaks = seq(0.2, 1.0, by = 0.2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.0, 1.03),
                     breaks = seq(0.0, 1.0, by = 0.2),
                     expand = c(0, 0)) +
    guides(color = guide_legend(ncol = 1, byrow = TRUE),
      linetype = guide_legend(ncol = 1, byrow = TRUE)) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key.size = unit(1.2, "lines"),
    legend.text = element_text(size = 18),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("results/group_ccei_by_member_ccei_median_bar.png", bar_plot,
       width = 6, height = 5, dpi = 300)

ggsave("results/group_ccei_by_member_ccei_median_cdf.png", cdf_plot,
       width = 6, height = 5, dpi = 300)