##########################################################
# Written By Byunghun Hahn, Nov 2025
# Replication Codes for All Figures
# Last Updated By Minseon Park/Claude Code: 2026-07-19 


#########################################################
### Figure 2: Had individually decided?
#########################################################

risk_q2_labels <- c(
  "1" = "Very Differently",
  "2" = "Somewhat Differently",
  "3" = "Somewhat Similar",
  "4" = "Mostly Similar"
)

generate_fig1_right_plot <- function(data) {
  fig_a <- data %>%
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

fig1_variants <- list(
  list(
    data = df,
    file = "results/ccei_bargaining_had_individual.png"
  ),
  list(
    data = df %>% filter(RA_dif_high == 1),
    file = "results/ccei_bargaining_had_individual_high_RA_diff.png"
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

for (spec in fig1_variants) {
  plot_obj <- generate_fig1_right_plot(spec$data)
  ggsave(spec$file, plot_obj,
         width = 7, height = 5, dpi = 300)
}



##############################################################
### SUPPLEMENTARY / APPENDIX FIGURES
##############################################################


###############################################

library(ggplot2)
library(ggpattern)

df <- data.frame(
  Block = c("Time", "CCEI", "Group and individual Char., Friendship", "Class FE"),
  Shapley = c(0.02165, 0.07197, 0.02554, 0.07627),
  Contribution = c(11.08, 36.83, 13.07, 39.03)
)

df$Label <- sprintf("%.4f (%.1f%%)", df$Shapley, df$Contribution)

df$Block <- factor(df$Block,
                   levels = c("CCEI", "Group and individual Char., Friendship", "Time", "Class FE"))

ggplot(df, aes(x = "", y = Shapley,
               fill = Block, pattern = Block)) +
  geom_bar_pattern(stat = "identity", width = 0.5,
                   colour = "black",
                   pattern_angle = c(-45, -45, 45, 0),
                   pattern_spacing = 0.12,
                   pattern_density = 0.4,
                   pattern_fill = "white",
                   pattern_colour = "white") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(
    name = "Block",
    values = c("Time" = "lightblue",
               "CCEI" = "white",
               "Group and individual Char., Friendship" = "pink",
               "Class FE" = "grey90")
  ) +
  scale_pattern_manual(
    name = "Block",
    values = c("Time" = "stripe",
               "CCEI" = "stripe",
               "Group and individual Char., Friendship" = "stripe",
               "Class FE" = "none")
  ) +
  labs(y = expression(R^2 ~ " Contribution"),
       x = "") +
  theme_minimal(base_size = 14)

ggsave("results/shapley_1.png", width = 6, height = 5, dpi = 300)


##############################################################

library(ggplot2)
library(ggpattern)
library(scales)

df <- data.frame(
  Block = c("CCEI", "Risk Aversion",
            "Demo/Cog/Non-Cog", "Mover", "Others"),
  Shapley = c(0.23580, 0.01931, 0.08912, 0.07442, 0.00450),
  Contribution = c(55.72, 4.56, 21.06, 17.59, 1.06)
)

df$Label <- sprintf("%.4f (%.1f%%)", df$Shapley, df$Contribution)

# Block 순서 지정: (넣고 싶은 순서대로)
df$Block <- factor(df$Block,
                   levels = c("CCEI",
                              "Demo/Cog/Non-Cog",
                              "Risk Aversion",
                              "Mover",
                              "Others"))

# 패턴 그래프
p <- ggplot(df, aes(x = "", y = Shapley,
                    fill = Block, pattern = Block)) +
  geom_bar_pattern(stat = "identity", width = 0.5,
                   colour = "black",
                   pattern_angle = c(-45, -45, 45, 45, 0),
                   pattern_spacing = 0.12,
                   pattern_density = 0.4,
                   pattern_fill = "white",
                   pattern_colour = "white") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(
    name = "Block",
    values = c("CCEI" = "white",
               "Demo/Cog/Non-Cog" = "lightblue",
               "Risk Aversion" = "pink",
               "Mover" = "grey80",
               "Others" = "grey90")
  ) +
  scale_pattern_manual(
    name = "Block",
    values = c("CCEI" = "stripe",
               "Demo/Cog/Non-Cog" = "stripe",
               "Risk Aversion" = "stripe",
               "Mover" = "stripe",
               "Others" = "none")
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  labs(y = expression(R^2 ~ " Contribution"), x = "") +
  theme_minimal(base_size = 14)

ggsave("results/shapley_2.png", plot = p,
       width = 6, height = 5, dpi = 300)


##############################################################
# Shapley Decomposition: RA-Based Bargaining Distance
##############################################################

shapley_r2 <- function(data, outcome, groups) {
  group_names <- names(groups)
  n_groups <- length(groups)
  model_vars <- unique(c(outcome, unlist(groups, use.names = FALSE)))
  data <- data[complete.cases(data[, model_vars]), ]
  r2_cache <- new.env(parent = emptyenv())

  active_groups <- function(mask) {
    as.logical(as.integer(intToBits(mask))[seq_len(n_groups)])
  }

  r2_for_mask <- function(mask) {
    key <- as.character(mask)
    if (exists(key, envir = r2_cache, inherits = FALSE)) {
      return(get(key, envir = r2_cache))
    }

    active <- active_groups(mask)
    terms <- unlist(groups[active], use.names = FALSE)
    r2 <- if (length(terms) == 0) {
      0
    } else {
      summary(lm(reformulate(terms, response = outcome), data = data))$r.squared
    }

    assign(key, r2, envir = r2_cache)
    r2
  }

  shapley <- numeric(n_groups)
  full_mask <- bitwShiftL(1, n_groups) - 1

  for (j in seq_len(n_groups)) {
    j_mask <- bitwShiftL(1, j - 1)
    for (mask in 0:full_mask) {
      if (bitwAnd(mask, j_mask) == 0) {
        subset_size <- sum(active_groups(mask))
        weight <- factorial(subset_size) *
          factorial(n_groups - subset_size - 1) /
          factorial(n_groups)
        shapley[j] <- shapley[j] +
          weight * (r2_for_mask(bitwOr(mask, j_mask)) - r2_for_mask(mask))
      }
    }
  }

  total <- sum(shapley)
  data.frame(
    Block = group_names,
    Shapley = shapley,
    Contribution = if (total > 0) 100 * shapley / total else 0,
    Total_R2 = r2_for_mask(full_mask)
  )
}

ra_shapley_data <- read_dta("data/finalized_panel_individual_251206.dta") %>%
  mutate(
    across(
      c(mathscore_i, outgoing_i, opened_i, agreeable_i, conscientious_i, stable_i),
      ~ ifelse(is.na(.x), 0, .x)
    ),
    RA_distance_denom = (RA_i - RA_g)^2 + (RA_j - RA_g)^2,
    RA_bargaining_distance = ifelse(
      RA_distance_denom > 0,
      (RA_i - RA_g)^2 / RA_distance_denom,
      NA_real_
    ),
    male_diff = male_i - male_j,
    RA_diff = RA_i - RA_j,
    all_corner_i = as.integer(RA_i < 0.0002),
    all_corner_j = as.integer(RA_j < 0.0002),
    all_mid_i = as.integer(RA_i > 0.4998 & RA_i < 0.5002),
    all_mid_j = as.integer(RA_j > 0.4998 & RA_j < 0.5002),
    all_corner_diff = all_corner_i - all_corner_j,
    all_mid_diff = all_mid_i - all_mid_j,
    class_fe = factor(class)
  )

ra_shapley_groups <- list(
  "Time" = c("post"),
  "CCEI" = c("HighCCEI", "HighCCEI_post", "ccei_i"),
  "Risk Aversion" = c(
    "RA_i", "RA_diff", "all_corner_i", "all_corner_diff",
    "all_mid_i", "all_mid_diff"
  ),
  "Individual Char." = c(
    "mathscore_i", "math_diff", "height_i", "height_diff",
    "outgoing_i", "outgoing_diff", "opened_i", "opened_diff",
    "agreeable_i", "agreeable_diff", "conscientious_i",
    "conscientious_diff", "stable_i", "stable_diff",
    "male_i", "male_diff"
  ),
  "Friendship" = c(
    "inclass_n_friends", "inclass_n_diff",
    "inclass_popularity", "inclass_pop_diff"
  ),
  "Missing Indicators" = c(
    "mathscore_dist_missing", "outgoing_diff_missing",
    "opened_diff_missing", "agreeable_diff_missing",
    "conscientious_diff_missing", "stable_diff_missing"
  ),
  "Class FE" = c("class_fe")
)

ra_shapley <- shapley_r2(
  ra_shapley_data,
  "RA_bargaining_distance",
  ra_shapley_groups
)

ra_shapley$Label <- sprintf("%.4f (%.1f%%)", ra_shapley$Shapley, ra_shapley$Contribution)
ra_shapley$Block <- factor(ra_shapley$Block, levels = ra_shapley$Block)

p <- ggplot(ra_shapley, aes(x = "", y = Shapley, fill = Block)) +
  geom_col(width = 0.5, colour = "black") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 3.7) +
  scale_fill_manual(
    name = "Block",
    values = c(
      "Time" = "lightblue",
      "CCEI" = "white",
      "Risk Aversion" = "pink",
      "Individual Char." = "#B9D7A8",
      "Friendship" = "#F1D28A",
      "Missing Indicators" = "grey80",
      "Class FE" = "grey90"
    )
  ) +
  labs(y = expression(R^2 ~ " Contribution"), x = "") +
  theme_minimal(base_size = 14)

ggsave("results/shapley_ra_bargaining.png", plot = p,
       width = 7, height = 5, dpi = 300)



















































###############################################################



# Figure X - (a)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())

load("data/finalized_panel_final.RData") 

names(panel_final)

# calculate median value of individual ccei
ccei_long <- panel_final %>%
  select(group_id, ccei_1, ccei_2) %>%
  pivot_longer(
    cols = starts_with("ccei_"),
    names_to = "member",
    values_to = "ccei"
  )

ccei_median <- median(ccei_long$ccei, na.rm = TRUE)

# labeling high median / low median
panel_final <- panel_final %>%
  mutate(
    ccei_both_median = as.integer(ccei_1 >= ccei_median) +
      as.integer(ccei_2 >= ccei_median),
    group_label_base = factor(
      ccei_both_median,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

# cdf
cdf_baseline_plot <- ggplot(
  panel_final,
  aes(x = ccei_g,
      colour = group_label_base,
      linetype = group_label_base)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective CCEI",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(
    limits = c(0.1, 1.0022),
    breaks = seq(0.2, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_1a.png",
  plot     = cdf_baseline_plot,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

####################################################################

# Collective CCEI by members' median-CCEI category

library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())

panel_individual <- read_dta("data/finalized_panel_individual_251206.dta")

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
  select(group_id, post, pair_ccei_group, ccei_g) %>%
  pivot_wider(names_from = pair_ccei_group, values_from = ccei_g) %>%
  filter(!is.na(`(Low, Low)`), !is.na(`(High, High)`)) %>%
  summarise(
    diff = mean(`(High, High)` - `(Low, Low)`),
    p_value = t.test(`(High, High)`, `(Low, Low)`)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    label = sprintf("Diff. = %.3f%s", diff, stars)
  )

diff_stats_lowhigh <- pair_wave %>%
  filter(pair_ccei_group %in% c("(Low, Low)", "(Low, High)")) %>%
  select(group_id, post, pair_ccei_group, ccei_g) %>%
  pivot_wider(names_from = pair_ccei_group, values_from = ccei_g) %>%
  filter(!is.na(`(Low, Low)`), !is.na(`(Low, High)`)) %>%
  summarise(
    diff = mean(`(Low, High)` - `(Low, Low)`),
    p_value = t.test(`(Low, High)`, `(Low, Low)`)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    label = sprintf("Diff. = %.3f%s", diff, stars)
  )

diff_stats_highhigh <- pair_wave %>%
  filter(pair_ccei_group %in% c("(Low, High)", "(High, High)")) %>%
  select(group_id, post, pair_ccei_group, ccei_g) %>%
  pivot_wider(names_from = pair_ccei_group, values_from = ccei_g) %>%
  filter(!is.na(`(Low, High)`), !is.na(`(High, High)`)) %>%
  summarise(
    diff = mean(`(High, High)` - `(Low, High)`),
    p_value = t.test(`(High, High)`, `(Low, High)`)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
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
               aes(x = "(Low, High)", xend = "(High, High)", y = y, yend = y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_segment(data = brackets_highhigh,
               aes(x = "(Low, High)", xend = "(Low, High)", y = y - 0.02, yend = y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_segment(data = brackets_highhigh,
               aes(x = "(High, High)", xend = "(High, High)", y = y - 0.02, yend = y),
               inherit.aes = FALSE, linewidth = 0.45) +
  geom_label(data = brackets_highhigh, aes(x = 2.58, y = y + 0.012, label = label),
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
    limits = c(bar_y_min, 1.08),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::label_number(accuracy = 0.01),
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
  guides(color = guide_legend(nrow = 1, byrow = TRUE),
         linetype = guide_legend(nrow = 1, byrow = TRUE)) +
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

#####################################################################

# Figure 2 - (a), (b)

# Note that 8 individuals whose Risk Aversion > 0.5 are excluded in the graph
# X axis is limited to (0, 0.5)

rm(list = ls())

library(ggplot2)
library(dplyr)

load("data/finalized_panel_individual.RData")

# cdf
plot_ra_diff <- function(data, time_val, output_name) {
  df <- data %>%
    filter(time == time_val) %>%
    mutate(RA_ig = abs(RA_i - RA_g))
  
  ggplot(df, aes(x = RA_ig, color = factor(high_dummy), linetype = factor(high_dummy))) +
    stat_ecdf(geom = "step", size = 1) +
    scale_color_manual(
      values = c("blue", "red"),
      labels = c("Low Rationality", "High Rationality")
    ) +
    scale_linetype_manual(
      values = c("dashed", "solid"),
      labels = c("Low Rationality", "High Rationality")
    ) +
    labs(
      x = expression("|RA"["i"] - RA["g"]~"|"),
      y = "Cumulative Frequency",
      color = NULL,
      linetype = NULL
    ) +
    scale_x_continuous(
      limits = c(0, 0.5022),
      breaks = seq(0, 0.5, by = 0.1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = c(0.65, 0.4),
      legend.justification = c("left", "top"),
      legend.text = element_text(size = 12),
      legend.key = element_rect(fill = "white", color = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "white", color = "black"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      plot.background = element_rect(fill = "white", color = NA)
    ) -> plot
  
  ggsave(
    filename = output_name,
    plot     = plot,
    path     = "results",
    width    = 7, height = 5, dpi = 300
  )
}

plot_ra_diff(panel_individual, time_val = 0, output_name = "figure_2a.png")
plot_ra_diff(panel_individual, time_val = 1, output_name = "figure_2b.png")


##############################################################
# Figure 3 - Revealed Preference Distance Index by Members' CCEI
##############################################################

rp_distance_data <- panel_individual %>%
  filter(!is.na(new2_I_ig), !is.na(high_dummy)) %>%
  mutate(
    ccei_group = factor(
      high_dummy,
      levels = c(0, 1),
      labels = c("Lower CCEI", "Higher CCEI")
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
  summarise(
    diff = mean(new2_I_ig[ccei_group == "Higher CCEI"]) -
      mean(new2_I_ig[ccei_group == "Lower CCEI"]),
    p_value = t.test(new2_I_ig ~ ccei_group)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    label = sprintf("Diff. = %.3f%s", diff, stars)
  )

rp_y_top <- max(rp_distance_stats$mean + rp_distance_stats$ci, na.rm = TRUE)
rp_y_min <- max(0, floor((min(rp_distance_stats$mean - rp_distance_stats$ci, na.rm = TRUE) - 0.02) * 100) / 100)

rp_bar_plot <- ggplot(rp_distance_stats, aes(x = ccei_group, y = mean, fill = ccei_group)) +
  geom_col(width = 0.62, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.16, linewidth = 0.45) +
  annotate("segment", x = 1, xend = 2, y = rp_y_top + 0.04, yend = rp_y_top + 0.04, linewidth = 0.45) +
  annotate("segment", x = 1, xend = 1, y = rp_y_top + 0.02, yend = rp_y_top + 0.04, linewidth = 0.45) +
  annotate("segment", x = 2, xend = 2, y = rp_y_top + 0.02, yend = rp_y_top + 0.04, linewidth = 0.45) +
  annotate("label", x = 1.5, y = rp_y_top + 0.055, label = rp_distance_diff$label[1], size = 18 / .pt, label.size = 0, fill = "white") +
  scale_fill_manual(values = c(
    "Lower CCEI" = "#E39695",
    "Higher CCEI" = "#74A9CF"
  )) +
  labs(x = NULL, y = "Mean revealed preference distance") +
  scale_y_continuous(
    limits = c(rp_y_min, rp_y_top + 0.12),
    breaks = scales::pretty_breaks(n = 5),
    labels = scales::label_number(accuracy = 0.01),
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
    x = "Revealed preference distance",
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

hist_bargaining_index <- ggplot(bar_data, aes(x = new2_I_ig)) +
  geom_histogram(binwidth = 0.05, boundary = 0, closed = "left",
                 fill = "#4C78A8", color = "white", linewidth = 0.25) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2),
                     expand = c(0, 0)) +
  labs(x = expression("Revealed bargaining index (" * I[ig] * ")"),
       y = "Count") +
  theme_classic(base_size = 13) +
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave("results/hist_bargaining_index.png", hist_bargaining_index,
       width = 6, height = 4.5, dpi = 300)


group_ccei_cdf_data <- panel_individual %>%
  filter(!is.na(ccei_g), !is.na(post)) %>%
  distinct(group_id, post, ccei_g) %>%
  mutate(source = ifelse(post == 0, "Baseline", "Endline"))

group_ccei_cdf <- ggplot(group_ccei_cdf_data,
                         aes(x = ccei_g, color = source, linetype = source)) +
  stat_ecdf(geom = "step", linewidth = 0.8) +
  scale_color_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
  scale_linetype_manual(values = c("Baseline" = "solid", "Endline" = "dashed")) +
  labs(x = "Group CCEI",
       y = "Cumulative Frequency",
       color = NULL,
       linetype = NULL) +
  scale_x_continuous(limits = c(0.1, 1.0022),
                     breaks = seq(0.2, 1.0, by = 0.2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.0, 1.0),
                     breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave("results/figure_group_ccei_cdf_baseline_endline.png", group_ccei_cdf,
       width = 7, height = 5, dpi = 300)


##############################################################

# Figure A1 - (a)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("data/finalized_panel_final.RData") 

ccei_baseline_long <- panel_final %>%
  select(ccei_1, ccei_2) %>%
  pivot_longer(cols = everything(), values_to = "ccei") %>%
  mutate(source = "Baseline")

ccei_endline_long <- panel_final %>%
  select(ccei_1_end, ccei_2_end) %>%
  pivot_longer(cols = everything(), values_to = "ccei") %>%
  mutate(source = "Endline")

ccei_combined <- bind_rows(ccei_baseline_long, ccei_endline_long)

mean_baseline <- mean(ccei_baseline_long$ccei, na.rm = TRUE)
mean_endline  <- mean(ccei_endline_long$ccei,  na.rm = TRUE)

F_baseline <- ecdf(ccei_baseline_long$ccei)
F_endline  <- ecdf(ccei_endline_long$ccei)

y_baseline <- F_baseline(mean_baseline)
y_endline  <- F_endline(mean_endline)

cdf_ccei <- ggplot(
  ccei_combined,
  aes(x = ccei, color = source)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  geom_segment(
    x = mean_baseline, xend = mean_baseline,
    y = 0, yend = y_baseline,
    color = "blue", linetype = "dashed", size = 0.6
  ) +
  geom_segment(
    x = mean_endline, xend = mean_endline,
    y = 0, yend = y_endline,
    color = "red", linetype = "dashed", size = 0.6
  ) +
  annotate(
    "text", x = mean_baseline, y = y_baseline + 0.05,
    label = paste0("mean = ", format(round(mean_baseline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "blue", size = 4
  ) +
  annotate(
    "text", x = mean_endline, y = y_endline + 0.03,
    label = paste0("mean = ", format(round(mean_endline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "red", size = 4
  ) +
  scale_color_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
  labs(
    x = "Individual CCEI",
    y = "Cumulative Frequency",
    color = NULL
  ) +
  scale_x_continuous(
    limits = c(-0.0022, 1.0022),
    breaks = seq(0.2, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1a.png",
  plot     = cdf_ccei,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

########################################

# Figure A1 - (b)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("data/finalized_panel_final.RData") 

ra_baseline_long <- panel_final %>%
  select(RA_1, RA_2) %>%
  pivot_longer(cols = everything(), values_to = "RA") %>%
  mutate(source = "Baseline")

ra_endline_long <- panel_final %>%
  select(RA_1_end, RA_2_end) %>%
  pivot_longer(cols = everything(), values_to = "RA") %>%
  mutate(source = "Endline")

ra_combined <- bind_rows(ra_baseline_long, ra_endline_long) %>%
  mutate(RA = ifelse(RA < 0 & RA > -1e-6, 0, RA))

mean_baseline <- mean(ra_baseline_long$RA, na.rm = TRUE)
mean_endline  <- mean(ra_endline_long$RA,  na.rm = TRUE)

F_baseline <- ecdf(ra_baseline_long$RA)
F_endline  <- ecdf(ra_endline_long$RA)

y_baseline <- F_baseline(mean_baseline)
y_endline  <- F_endline(mean_endline)

cdf_RA <- ggplot(
  ra_combined,
  aes(x = RA, color = source)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  geom_segment(
    x = mean_baseline, xend = mean_baseline,
    y = 0, yend = y_baseline,
    color = "blue", linetype = "dashed", size = 0.6
  ) +
  geom_segment(
    x = mean_endline, xend = mean_endline,
    y = 0, yend = y_endline,
    color = "red", linetype = "dashed", size = 0.6
  ) +
  annotate(
    "text", x = mean_baseline, y = y_baseline + 0.05,
    label = paste0("mean = ", format(round(mean_baseline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "blue", size = 4
  ) +
  annotate(
    "text", x = mean_endline, y = y_endline + 0.1,
    label = paste0("mean = ", format(round(mean_endline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "red", size = 4
  ) +
  scale_color_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
  labs(
    x = "Individual Risk Aversion",
    y = "Cumulative Frequency",
    color = NULL
  ) +
  scale_x_continuous(
    limits = c(-0.0022, 1.0022),
    breaks = seq(0, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1b.png",
  plot     = cdf_RA,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

########################################

# Figure A1 - (c)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("data/finalized_panel_final.RData") 

fccei_baseline_long <- panel_final %>%
  select(f_ccei_1, f_ccei_2) %>%
  pivot_longer(cols = everything(), values_to = "f_ccei") %>%
  mutate(source = "Baseline")

fccei_endline_long <- panel_final %>%
  select(f_ccei_1_end, f_ccei_2_end) %>%
  pivot_longer(cols = everything(), values_to = "f_ccei") %>%
  mutate(source = "Endline")

fccei_combined <- bind_rows(fccei_baseline_long, fccei_endline_long) %>%
  mutate(f_ccei = ifelse(f_ccei < 0 & f_ccei > -1e-6, 0, f_ccei))

mean_baseline <- mean(fccei_baseline_long$f_ccei, na.rm = TRUE)
mean_endline  <- mean(fccei_endline_long$f_ccei,  na.rm = TRUE)

F_baseline <- ecdf(fccei_baseline_long$f_ccei)
F_endline  <- ecdf(fccei_endline_long$f_ccei)

y_baseline <- F_baseline(mean_baseline)
y_endline  <- F_endline(mean_endline)

cdf_f_ccei <- ggplot(
  fccei_combined,
  aes(x = f_ccei, color = source)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  geom_segment(
    x = mean_baseline, xend = mean_baseline,
    y = 0, yend = y_baseline,
    color = "blue", linetype = "dashed", size = 0.6
  ) +
  geom_segment(
    x = mean_endline, xend = mean_endline,
    y = 0, yend = y_endline,
    color = "red", linetype = "dashed", size = 0.6
  ) +
  annotate(
    "text", x = mean_baseline, y = y_baseline + 0.05,
    label = paste0("mean = ", format(round(mean_baseline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "blue", size = 4
  ) +
  annotate(
    "text", x = mean_endline, y = y_endline + 0.03,
    label = paste0("mean = ", format(round(mean_endline, 3), nsmall = 3)),
    hjust = 1, vjust = 0, color = "red", size = 4
  ) +
  scale_color_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
  labs(
    x = "Individual F GARP",
    y = "Cumulative Frequency",
    color = NULL
  ) +
  scale_x_continuous(
    limits = c(-0.0022, 1.0022),
    breaks = seq(0.2, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1c.png",
  plot     = cdf_f_ccei,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)


#############################################

# Figure A1 - (d)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("data/finalized_panel_final.RData") 

mpi_baseline_long <- panel_final %>%
  select(max_mpi_1, max_mpi_2) %>%
  pivot_longer(cols = everything(), values_to = "max_mpi") %>%
  mutate(value = 1 - max_mpi,
         source = "Baseline")

mpi_endline_long <- panel_final %>%
  select(max_mpi_1_end, max_mpi_2_end) %>%
  pivot_longer(cols = everything(), values_to = "max_mpi") %>%
  mutate(value = 1 - max_mpi,
         source = "Endline")

mpi_combined <- bind_rows(mpi_baseline_long, mpi_endline_long)

mean_baseline <- mean(mpi_baseline_long$value, na.rm = TRUE)
mean_endline  <- mean(mpi_endline_long$value,  na.rm = TRUE)

F_baseline <- ecdf(mpi_baseline_long$value)
F_endline  <- ecdf(mpi_endline_long$value)

y_baseline <- F_baseline(mean_baseline)
y_endline  <- F_endline(mean_endline)

cdf_mpi <- ggplot(
  mpi_combined,
  aes(x = value, color = source)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  geom_segment(x = mean_baseline, xend = mean_baseline, y = 0, yend = y_baseline,
               color = "blue", linetype = "dashed", size = 0.6) +
  geom_segment(x = mean_endline, xend = mean_endline, y = 0, yend = y_endline,
               color = "red", linetype = "dashed", size = 0.6) +
  annotate("text", x = mean_baseline, y = y_baseline + 0.05,
           label = paste0("mean = ", format(round(mean_baseline, 3), nsmall = 3)),
           hjust = 1, vjust = 0, color = "blue", size = 4) +
  annotate("text", x = mean_endline, y = y_endline + 0.03,
           label = paste0("mean = ", format(round(mean_endline, 3), nsmall = 3)),
           hjust = 1, vjust = 0, color = "red", size = 4) +
  scale_color_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
  labs(
    x = "Individual (1 - Max MPI)",
    y = "Cumulative Frequency",
    color = NULL
  ) +
  scale_x_continuous(
    limits = c(-0.0022, 1.0022),
    breaks = seq(0, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1d.png",
  plot     = cdf_mpi,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

#########################################################

# Figure A1 - (a)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())

load("data/finalized_panel_final.RData") 

# calculate median value of individual FGARP ccei
ccei_long <- panel_final %>%
  select(group_id, f_ccei_1, f_ccei_2) %>%
  pivot_longer(
    cols = starts_with("f_ccei_"),
    names_to = "member",
    values_to = "ccei"
  )

ccei_median <- median(ccei_long$ccei, na.rm = TRUE)

# labeling high median / low median
panel_final <- panel_final %>%
  mutate(
    f_ccei_both_median = as.integer(f_ccei_1 >= ccei_median) +
      as.integer(f_ccei_2 >= ccei_median),
    group_label_base = factor(
      f_ccei_both_median,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

# cdf
cdf_f_baseline_plot <- ggplot(
  panel_final,
  aes(x = f_ccei_g,
      colour = group_label_base,
      linetype = group_label_base)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective FGARP",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(
    limits = c(0.1, 1.0022),
    breaks = seq(0.2, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1a.png",
  plot     = cdf_f_baseline_plot,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

# Figure A1 - (b)

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())

load("data/finalized_panel_final.RData")

# calculate median value of individual FGARP ccei at endline
ccei_long <- panel_final %>%
  select(group_id, f_ccei_1_end, f_ccei_2_end) %>%
  pivot_longer(
    cols = starts_with("f_ccei_"),
    names_to = "member",
    values_to = "ccei"
  )

ccei_median <- median(ccei_long$ccei, na.rm = TRUE)

# labeling high median / low median
panel_final <- panel_final %>%
  mutate(
    f_ccei_both_median = as.integer(f_ccei_1_end >= ccei_median) +
      as.integer(f_ccei_2_end >= ccei_median),
    group_label_end = factor(
      f_ccei_both_median,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

# cdf
cdf_f_endline_plot <- ggplot(
  panel_final,
  aes(x = f_ccei_g_end,
      colour = group_label_end,
      linetype = group_label_end)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective FGARP",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(
    limits = c(0.1, 1.0022),
    breaks = seq(0.2, 1.0, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.0, 1.0),
    breaks = seq(0.0, 1.0, by = 0.2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1b.png",
  plot     = cdf_f_endline_plot,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

############################################

# Figure A1 - (c): MPI baseline

library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())
load("data/finalized_panel_final.RData")

# transform variables: 1 - max_mpi
panel_final <- panel_final %>%
  mutate(
    one_minus_mpi_1    = 1 - max_mpi_1,
    one_minus_mpi_2    = 1 - max_mpi_2,
    one_minus_mpi_g    = 1 - max_mpi_g
  )

# median of individual (1 - max_mpi)
mpi_long <- panel_final %>%
  select(group_id, one_minus_mpi_1, one_minus_mpi_2) %>%
  pivot_longer(
    cols = starts_with("one_minus_mpi_"),
    names_to = "member",
    values_to = "mpi_val"
  )

mpi_median <- median(mpi_long$mpi_val, na.rm = TRUE)

# label groups
panel_final <- panel_final %>%
  mutate(
    mpi_both_median = as.integer(one_minus_mpi_1 >= mpi_median) +
      as.integer(one_minus_mpi_2 >= mpi_median),
    group_label_base = factor(
      mpi_both_median,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

# plot CDF
cdf_mpi_baseline_plot <- ggplot(
  panel_final,
  aes(x = one_minus_mpi_g,
      colour = group_label_base,
      linetype = group_label_base)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective (1 - Max MPI)",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0.0, 1.0022),
                     breaks = seq(0.0, 1.0, by = 0.2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.0, 1.0),
                     breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1c.png",
  plot     = cdf_mpi_baseline_plot,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

# Figure A1 - (d): MPI endline

rm(list = ls())
load("data/finalized_panel_final.RData")

# transform variables: 1 - max_mpi (endline)
panel_final <- panel_final %>%
  mutate(
    one_minus_mpi_1_end = 1 - max_mpi_1_end,
    one_minus_mpi_2_end = 1 - max_mpi_2_end,
    one_minus_mpi_g_end = 1 - max_mpi_g_end
  )

# median of individual (1 - max_mpi_end)
mpi_long <- panel_final %>%
  select(group_id, one_minus_mpi_1_end, one_minus_mpi_2_end) %>%
  pivot_longer(
    cols = starts_with("one_minus_mpi_"),
    names_to = "member",
    values_to = "mpi_val"
  )

mpi_median <- median(mpi_long$mpi_val, na.rm = TRUE)

# label groups
panel_final <- panel_final %>%
  mutate(
    mpi_both_median = as.integer(one_minus_mpi_1_end >= mpi_median) +
      as.integer(one_minus_mpi_2_end >= mpi_median),
    group_label_end = factor(
      mpi_both_median,
      levels = c(0, 1, 2),
      labels = c("(Low, Low)", "(Low, High)", "(High, High)")
    )
  )

# plot CDF
cdf_mpi_endline_plot <- ggplot(
  panel_final,
  aes(x = one_minus_mpi_g_end,
      colour = group_label_end,
      linetype = group_label_end)
) +
  stat_ecdf(geom = "step", size = 0.8) +
  scale_color_manual(values = c("black", "blue", "red")) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) +
  labs(
    x = "Collective (1 - Max MPI)",
    y = "Cumulative Frequency",
    color = NULL,
    linetype = NULL
  ) +
  scale_x_continuous(limits = c(0.0, 1.0022),
                     breaks = seq(0.0, 1.0, by = 0.2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.0, 1.0),
                     breaks = seq(0.0, 1.0, by = 0.2)) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = "white", colour = "black"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(1.5, "lines"),
    legend.text          = element_text(size = 12),
    axis.title           = element_text(size = 14),
    axis.text            = element_text(size = 12),
    plot.background      = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "figure_A1d.png",
  plot     = cdf_mpi_endline_plot,
  path     = "results",
  width    = 7, height = 5, dpi = 300
)

####################################
library(ggplot2)

df <- data.frame(
  Block = c("Time", "CCEI", "Group/Friendship", "Class FE"),
  Shapley = c(0.0215, 0.0718, 0.0258, 0.0763),
  Contribution = c(11.0, 36.7, 13.2, 39.0)
)

df$Label <- sprintf("%.4f (%.1f%%)", df$Shapley, df$Contribution)

df$Block <- factor(df$Block,
                   levels = c("CCEI", "Group/Friendship", "Time", "Class FE"))

ggplot(df, aes(x = "", y = Shapley, fill = Block)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("Time" = "skyblue",
                               "CCEI" = "orange",
                               "Group/Friendship" = "green",
                               "Class FE" = "grey70")) +
  labs(title = expression("Shapley Decomposition of " ~ R^2),
       y = expression(R^2 ~ " Contribution"),
       x = "") +
  theme_minimal(base_size = 14)

ggsave("shapley_v1.png", width = 6, height = 5, dpi = 300)





library(ggplot2)
library(ggpattern)

df <- data.frame(
  Block = c("Time", "CCEI", "Group/Friendship", "Class FE"),
  Shapley = c(0.0215, 0.0718, 0.0258, 0.0763),
  Contribution = c(11.0, 36.7, 13.2, 39.0)
)

df$Label <- sprintf("%.4f (%.1f%%)", df$Shapley, df$Contribution)

df$Block <- factor(df$Block,
                   levels = c("CCEI", "Group/Friendship", "Time", "Class FE"))

ggplot(df, aes(x = "", y = Shapley,
               fill = Block, pattern = Block)) +
  geom_bar_pattern(stat = "identity", width = 0.5,
                   colour = "black",
                   # aesthetic 값 매핑은 scale_*_manual로 빼기
                   pattern_angle = c(-45, -45, 45, 0),
                   pattern_spacing = 0.12,
                   pattern_density = 0.4,
                   pattern_fill = "white",
                   pattern_colour = "white") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(
    name = "Block",
    values = c("Time" = "lightblue",
               "CCEI" = "white",
               "Group/Friendship" = "pink",
               "Class FE" = "grey90")
  ) +
  scale_pattern_manual(
    name = "Block",
    values = c("Time" = "stripe",
               "CCEI" = "stripe",
               "Group/Friendship" = "stripe",
               "Class FE" = "none")
  ) +
  labs(y = expression(R^2 ~ " Contribution"),
       x = "") +
  theme_minimal(base_size = 14)

ggsave("shapley_v2.png", width = 6, height = 5, dpi = 300)

######################################################

library(ggplot2)
library(dplyr)

##############################################
# Group A: Friend, Gender
##############################################
dfA <- data.frame(
  xpos = factor(c("Friend: No","Friend: Mutual",
                  "Gender: Different","Gender: Same"),
                levels=c("Friend: No","Friend: Mutual",
                         "Gender: Different","Gender: Same")),
  coef = c(-0.439, -0.475, -0.423, -0.449),
  ci_low = c(-0.474, -0.541, -0.497, -0.481),
  ci_high = c(-0.403, -0.410, -0.349, -0.417),
  stars = c("**","**","**","**"),
  group = "normal"
)

dfA <- dfA %>%
  mutate(id=row_number(),
         hjust=ifelse(id %% 2 == 1, -0.2, 1.2))

block_labels_A <- data.frame(
  xpos=c(1.5, 3.5),
  ypos=-0.85,
  label=c("Friend","Gender")
)

pA <- ggplot(dfA, aes(x=xpos, y=coef)) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=0.05, color="black") +
  geom_point(shape=16, size=3, color="black") +
  geom_text(aes(label=paste0(sprintf("%.3f",coef),stars),
                hjust=hjust), vjust=-0.5, size=3.5) +
  scale_y_continuous(limits=c(-0.9,0), breaks=seq(-0.8,0,0.2)) +
  scale_x_discrete(labels=c("No","Mutual","Different","Same")) +
  geom_text(data=block_labels_A, aes(x=xpos, y=ypos, label=label),
            inherit.aes=FALSE, fontface="bold", size=5) +
  theme_classic(base_size=14) +
  labs(x="", y="Coefficient of HighCCEI") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=12),
        legend.position="none")

ggsave("results/fig6_A_ver1.png", pA, width=6, height=4, dpi=300)


##############################################
# Group B: Outgoing, Valuetrust, Valuecowork
##############################################
dfB <- data.frame(
  xpos=factor(c("Outgoing: Low","Outgoing: High",
                "Valuetrust: Low","Valuetrust: High",
                "Valuecowork: Low","Valuecowork: High"),
              levels=c("Outgoing: Low","Outgoing: High",
                       "Valuetrust: Low","Valuetrust: High",
                       "Valuecowork: Low","Valuecowork: High")),
  coef=c(-0.438, -0.461, -0.434, -0.585, -0.440, -0.565),
  ci_low=c(-0.470, -0.526, -0.464, -0.693, -0.470, -0.773),
  ci_high=c(-0.405, -0.396, -0.403, -0.478, -0.409, -0.358),
  stars=c("**","**","**","**","**","**"),
  group=c("normal","normal","trust","trust","normal","normal")
)

dfB <- dfB %>%
  mutate(id=row_number(),
         hjust=ifelse(id %% 2 == 1, -0.2, 1.2),
         vjust=ifelse(xpos=="Outgoing: High", 1.5, -0.5))

block_labels_B <- data.frame(
  xpos=c(1.5, 3.5, 5.5),
  ypos=-0.85,
  label=c("Outgoing","Value Trust","Value Cowork")
)

pB <- ggplot(dfB, aes(x=xpos, y=coef)) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high,
                    color=group, linetype=group), width=0.05) +
  geom_point(aes(shape=group, color=group), size=3) +
  geom_text(aes(label=paste0(sprintf("%.3f",coef),stars),
                hjust=hjust, vjust=vjust), size=3.5) +
  annotate("text", x=3.5, y=min(dfB$ci_low[dfB$group=="trust"])-0.02,
           label="p=0.005", size=4, color="red") +
  scale_color_manual(values=c("normal"="black","trust"="red")) +
  scale_shape_manual(values=c("normal"=16,"trust"=18)) +
  scale_linetype_manual(values=c("normal"="solid","trust"="solid")) +
  scale_y_continuous(limits=c(-0.9,0), breaks=seq(-0.8,0,0.2)) +
  scale_x_discrete(labels=c("Low","High","Low","High","Low","High")) +
  geom_text(data=block_labels_B, aes(x=xpos, y=ypos, label=label),
            inherit.aes=FALSE, fontface="bold", size=5) +
  theme_classic(base_size=14) +
  labs(x="", y="Coefficient of HighCCEI") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=12),
        legend.position="none")

ggsave("results/fig6_B_ver1.png", pB, width=6, height=4, dpi=300)


##############################################
# Group C: Pbl share idea, Teacher induce, Class participate
##############################################
dfC <- data.frame(
  xpos=factor(c("Pbleffectsharidea: Low","Pbleffectsharidea: High",
                "Teacherinduce: Low","Teacherinduce: High",
                "Classparticipate: Low","Classparticipate: High"),
              levels=c("Pbleffectsharidea: Low","Pbleffectsharidea: High",
                       "Teacherinduce: Low","Teacherinduce: High",
                       "Classparticipate: Low","Classparticipate: High")),
  coef=c(-0.433,-0.529,-0.438,-0.485,-0.436,-0.508),
  ci_low=c(-0.467,-0.609,-0.472,-0.563,-0.470,-0.597),
  ci_high=c(-0.399,-0.448,-0.405,-0.407,-0.401,-0.420),
  stars=c("**","**","**","**","**","**"),
  group=c("share","share","normal","normal","normal","normal")
)

dfC <- dfC %>%
  mutate(id=row_number(),
         hjust=ifelse(id %% 2 == 1, -0.2, 1.2),
         vjust=ifelse(id %% 2 == 0, 1.5, -0.5))

block_labels_C <- data.frame(
  xpos=c(1.5,3.5,5.5),
  ypos=-0.85,
  label=c("PBL Share Idea","Teacher Induce","Class Participate")
)

pC <- ggplot(dfC, aes(x=xpos, y=coef)) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high,
                    color=group, linetype=group), width=0.05) +
  geom_point(aes(shape=group, color=group), size=3) +
  geom_text(aes(label=paste0(sprintf("%.3f",coef),stars),
                hjust=hjust, vjust=vjust), size=3.5) +
  annotate("text", x=1.5, y=min(dfC$ci_low[dfC$group=="share"])-0.02,
           label="p=0.040", size=4, color="red") +
  scale_color_manual(values=c("normal"="black","share"="red")) +
  scale_shape_manual(values=c("normal"=16,"share"=18)) +
  scale_linetype_manual(values=c("normal"="solid","share"="solid")) +
  scale_y_continuous(limits=c(-0.9,0), breaks=seq(-0.8,0,0.2)) +
  scale_x_discrete(labels=c("Low","High","Low","High","Low","High")) +
  geom_text(data=block_labels_C, aes(x=xpos, y=ypos, label=label),
            inherit.aes=FALSE, fontface="bold", size=5) +
  theme_classic(base_size=14) +
  labs(x="", y="Coefficient of HighCCEI") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(size=12),
        legend.position="none")

ggsave("results/fig6_C_ver1.png", pC, width=6, height=4, dpi=300)

#############################################


library(ggplot2)
library(ggpattern)

df <- data.frame(
  Block = c("Time", "CCEI", "Group and individual Char., Friendship", "Class FE"),
  Shapley = c(0.02153, 0.07173, 0.02562, 0.07649),
  Contribution = c(11.0, 36.7, 13.1, 39.2)
)

df$Label <- sprintf("%.4f (%.1f%%)", df$Shapley, df$Contribution)

df$Block <- factor(df$Block,
                   levels = c("CCEI", "Group and individual Char., Friendship", "Time", "Class FE"))

ggplot(df, aes(x = "", y = Shapley,
               fill = Block, pattern = Block)) +
  geom_bar_pattern(stat = "identity", width = 0.5,
                   colour = "black",
                   pattern_angle = c(-45, -45, 45, 0),
                   pattern_spacing = 0.12,
                   pattern_density = 0.4,
                   pattern_fill = "white",
                   pattern_colour = "white") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(
    name = "Block",
    values = c("Time" = "lightblue",
               "CCEI" = "white",
               "Group and individual Char., Friendship" = "pink",
               "Class FE" = "grey90")
  ) +
  scale_pattern_manual(
    name = "Block",
    values = c("Time" = "stripe",
               "CCEI" = "stripe",
               "Group and individual Char., Friendship" = "stripe",
               "Class FE" = "none")
  ) +
  labs(y = expression(R^2 ~ " Contribution"),
       x = "") +
  theme_minimal(base_size = 14)

ggsave("shapley_1.png", width = 6, height = 5, dpi = 300)

################################

library(ggplot2)
library(ggpattern)

df <- data.frame(
  Block = c("Time", "CCEI", "Group/Friendship", "Mover", "Class FE"),
  Shapley = c(0.10814, 0.26222, 0.02318, 0.00029, 0.00010),
  Contribution = c(27.5, 66.6, 5.9, 0.1, 0.0)
)

df$Label <- sprintf("%.4f (%.1f%%)", df$Shapley, df$Contribution)

df$Block <- factor(df$Block,
                   levels = c("CCEI", "Group/Friendship", "Time", "Mover", "Class FE"))

ggplot(df, aes(x = "", y = Shapley,
               fill = Block, pattern = Block)) +
  geom_bar_pattern(stat = "identity", width = 0.5,
                   colour = "black",
                   pattern_angle = c(-45, -45, 45, 0, 0),
                   pattern_spacing = 0.12,
                   pattern_density = 0.4,
                   pattern_fill = "white",
                   pattern_colour = "white") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(
    name = "Block",
    values = c("Time" = "lightblue",
               "CCEI" = "white",
               "Group/Friendship" = "pink",
               "Mover" = "orange",
               "Class FE" = "grey90")
  ) +
  scale_pattern_manual(
    name = "Block",
    values = c("Time" = "stripe",
               "CCEI" = "stripe",
               "Group/Friendship" = "stripe",
               "Mover" = "stripe",
               "Class FE" = "none")
  ) +
  labs(y = expression(R^2 ~ " Contribution"),
       x = "") +
  theme_minimal(base_size = 14)

ggsave("shapley_2.png", width = 6, height = 5, dpi = 300)

############################################3

library(ggplot2)
library(ggpattern)
library(scales)

df <- data.frame(
  Block = c("Time", "CCEI", "Group/Friendship", "Mover", "Class FE"),
  Shapley = c(0.10814, 0.26222, 0.02318, 0.00029, 0.00010),
  Contribution = c(27.5, 66.6, 5.9, 0.1, 0.0)
)

df$Block <- as.character(df$Block) # 문자열 처리
others <- data.frame(
  Block = "Others",
  Shapley = sum(df$Shapley[df$Block %in% c("Mover", "Class FE")]),
  Contribution = sum(df$Contribution[df$Block %in% c("Mover", "Class FE")])
)

df <- rbind(
  df[!(df$Block %in% c("Mover", "Class FE")), ],
  others
)

df$Label <- sprintf("%.4f (%.1f%%)", df$Shapley, df$Contribution)

df$Block <- factor(df$Block,
                   levels = c("CCEI", "Group/Friendship", "Time", "Others"))

library(scales)

p <- ggplot(df, aes(x = "", y = Shapley,
                    fill = Block, pattern = Block)) +
  geom_bar_pattern(stat = "identity", width = 0.5,
                   colour = "black",
                   pattern_angle = c(-45, -45, 45, 0),
                   pattern_spacing = 0.12,
                   pattern_density = 0.4,
                   pattern_fill = "white",
                   pattern_colour = "white") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(
    name = "Block",
    values = c("Time" = "lightblue",
               "CCEI" = "white",
               "Group/Friendship" = "pink",
               "Others" = "grey90")
  ) +
  scale_pattern_manual(
    name = "Block",
    values = c("Time" = "stripe",
               "CCEI" = "stripe",
               "Group/Friendship" = "stripe",
               "Others" = "none")
  ) +
  scale_y_continuous(
    labels = number_format(accuracy = 0.01)
  ) +
  labs(y = expression(R^2 ~ " Contribution"),
       x = "") +
  theme_minimal(base_size = 14)



ggsave("shapley_2.png", plot = p, width = 6, height = 5, dpi = 300)

#####################################################################
