rm(list = ls())
options(error = NULL)

distance_var <- "Ihat_ig"

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

replication_dir <- normalizePath(code_dir, mustWork = TRUE)
data_dir <- file.path(replication_dir, "data")
result_dir <- file.path(code_dir, "results", "figures")
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
regular_output_path <- function(stem, extension = ".png") {
  file.path(result_dir, paste0(stem, extension))
}

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

# Figure 2

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

make_figure2_pair(distance_var, result_dir)

# Figure 3

plot_definitions <- tibble::tribble(
  ~definition, ~ccei_var, ~file_suffix,
  "bothhigh", "HighCCEI_both_high", "",
  "bothlow", "HighCCEI_both_low", "_bothlow"
)

for (definition_index in seq_len(nrow(plot_definitions))) {
  ccei_var <- plot_definitions$ccei_var[definition_index]
  file_suffix <- plot_definitions$file_suffix[definition_index]
  panel_plot <- panel_individual |>
    mutate(
      HighCCEI_plot = as.numeric(.data[[ccei_var]])
    )

rp_data <- panel_plot |>
  filter(!is.na(.data[[distance_var]]), !is.na(HighCCEI_plot)) |>
  transmute(
    distance = .data[[distance_var]],
    HighCCEI_plot,
    ccei_group = factor(
      if_else(HighCCEI_plot == 1, "Higher CCEI", "Lower CCEI"),
      levels = c("Lower CCEI", "Higher CCEI")
    )
  )

rp_stats <- rp_data |>
  group_by(ccei_group) |>
  summarise(
    mean = mean(distance),
    sd = sd(distance),
    n = n(),
    se = sd / sqrt(n),
    ci = qt(0.975, n - 1) * se,
    .groups = "drop"
  )

rp_test <- t.test(distance ~ ccei_group, data = rp_data)
rp_difference <- diff(rev(rp_stats$mean))
rp_label <- sprintf(
  "Diff. = %.3f%s",
  rp_difference,
  sig_mark(rp_test$p.value)
)
rp_y <- max(rp_stats$mean + rp_stats$ci) + 0.08

distance_symbol <- quote(I[ig])

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
  labs(
    x = NULL,
    y = bquote("Mean revealed preference distance")
  ) +
  paper_theme +
  theme(legend.position = "none")

rp_cdf <- ggplot(rp_data, aes(distance, colour = ccei_group, linetype = ccei_group)) +
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
    x = bquote("Revealed preference distance"),
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
  file.path(result_dir, paste0("bargaining_index_by_ccei_bar", file_suffix, ".png")),
  rp_bar, width = 6, height = 5, dpi = 300
)
ggsave(
  file.path(result_dir, paste0("bargaining_index_by_ccei_cdf", file_suffix, ".png")),
  rp_cdf, width = 6, height = 5, dpi = 300
)

# I distribution

hist_overlay_data_Ihat_ig <- panel_plot |>
  filter(!is.na(HighCCEI_plot), !is.na(Ihat_ig)) |>
  mutate(
    member = factor(
      HighCCEI_plot,
      levels = c(0, 1),
      labels = c("Lower CCEI", "Higher CCEI")
    )
  ) |>
  group_by(member) |>
  mutate(percent_weight = 100 / n()) |>
  ungroup()

hist_higher_Ihat_ig <- hist_overlay_data_Ihat_ig |>
  filter(member == "Higher CCEI")
hist_lower_Ihat_ig <- hist_overlay_data_Ihat_ig |>
  filter(member == "Lower CCEI")

hist_bargaining_index <- ggplot() +
  geom_histogram(
    data = hist_higher_Ihat_ig,
    aes(
      x = Ihat_ig,
      weight = percent_weight,
      fill = member,
      colour = member
    ),
    binwidth = 0.05,
    boundary = 0,
    closed = "left",
    position = "identity",
    alpha = 0.45,
    linewidth = 0.45
  ) +
  geom_histogram(
    data = hist_lower_Ihat_ig,
    aes(
      x = Ihat_ig,
      weight = percent_weight,
      fill = member,
      colour = member
    ),
    binwidth = 0.05,
    boundary = 0,
    closed = "left",
    position = "identity",
    alpha = 0.45,
    linewidth = 0.45
  ) +
  scale_fill_manual(
    breaks = c("Higher CCEI", "Lower CCEI"),
    values = c(
      "Lower CCEI" = "#E39695",
      "Higher CCEI" = "#74A9CF"
    ),
    labels = c(
      "More rational member (higher CCEI)",
      "Less rational member (lower CCEI)"
    )
  ) +
  scale_colour_manual(
    breaks = c("Higher CCEI", "Lower CCEI"),
    values = c(
      "Lower CCEI" = "#E39695",
      "Higher CCEI" = "#2C6DA4"
    ),
    labels = c(
      "More rational member (higher CCEI)",
      "Less rational member (lower CCEI)"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Revealed-preference distance",
    y = "Percent",
    fill = NULL,
    colour = NULL
  ) +
  guides(
    colour = "none",
    fill = guide_legend(
      override.aes = list(
        alpha = c(0.45, 0.45),
        colour = c("#2C6DA4", "#E39695")
      )
    )
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(
      fill = "white", colour = "black", linewidth = 0.3
    ),
    legend.box.background = element_rect(
      fill = "white", colour = "black", linewidth = 0.3
    ),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  file.path(result_dir, paste0("hist_bargaining_index", file_suffix, ".png")),
  hist_bargaining_index,
  width = 7.2,
  height = 4.6,
  dpi = 400,
  bg = "white"
)
}

# Figure 7

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
           linewidth = 0, fill = "white") +
  annotate("segment", x = 2, xend = 3, y = bracket_low, yend = bracket_low) +
  annotate("segment", x = 2, xend = 2, y = bracket_low - 0.020, yend = bracket_low) +
  annotate("segment", x = 3, xend = 3, y = bracket_low - 0.020, yend = bracket_low) +
  annotate("label", x = 2.58, y = bracket_low + 0.012,
           label = diff_mid_high$label, size = 17 / .pt,
           linewidth = 0, fill = "white") +
  annotate("segment", x = 1, xend = 3, y = bracket_high, yend = bracket_high) +
  annotate("segment", x = 1, xend = 1, y = bracket_high - 0.020, yend = bracket_high) +
  annotate("segment", x = 3, xend = 3, y = bracket_high - 0.020, yend = bracket_high) +
  annotate("label", x = 2, y = bracket_high + 0.012,
           label = diff_extreme$label, size = 18 / .pt,
           linewidth = 0, fill = "white") +
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
  regular_output_path("group_ccei_by_member_ccei_median_bar"),
  group_bar, width = 6, height = 5, dpi = 300
)
ggsave(
  regular_output_path("group_ccei_by_member_ccei_median_cdf"),
  group_cdf, width = 6, height = 5, dpi = 300
)

# CEI figures

cei_data <- panel_individual |>
  distinct(
    group_id,
    post,
    ccei_g,
    cei_g,
    cei_g_untempered,
    cei_n_viol,
    cei_n_viol_untempered
  ) |>
  mutate(
    wave = if_else(post == 0, "base", "end"),
    e_coll_tempered = cei_g,
    e_coll_untempered = cei_g_untempered,
    n_viol_tempered = cei_n_viol,
    n_viol_untempered = cei_n_viol_untempered
  )
cei_by_wave <- list(
  Baseline = cei_data[cei_data$post == 0, ],
  Endline = cei_data[cei_data$post == 1, ]
)
cei_colors <- c(Baseline = "#4C78A8", Endline = "#E45756")

draw_cei_distribution <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  values <- lapply(cei_by_wave, function(x) x$e_coll_tempered)
  lower <- floor(min(unlist(values)) * 10) / 10
  breaks <- seq(lower, 1, by = 0.05)
  centers <- head(breaks, -1) + 0.025
  par(mfrow = c(1, 2), mar = c(4.5, 4.8, 3.2, 1.0), oma = c(2.2, 0, 2.3, 0))

  for (label in names(values)) {
    x <- values[[label]]
    h <- hist(x[x < 1 - 1e-9], breaks = breaks, plot = FALSE, right = FALSE)
    pct <- h$counts / length(x) * 100
    mass_one <- mean(x >= 1 - 1e-9) * 100
    plot(
      NA, xlim = c(lower - 0.025, 1.03), ylim = c(0, 82),
      xaxs = "i", yaxs = "i", xlab = "Collective Efficiency Index",
      ylab = if (label == "Baseline") "Percent" else "",
      main = sprintf("%s (N = %d)", label, length(x)), axes = FALSE
    )
    abline(h = seq(0, 80, by = 10), col = "#D9D9D9", lwd = 0.8)
    rect(centers - 0.023, 0, centers + 0.023, pct,
         col = cei_colors[[label]], border = "white", lwd = 0.5)
    rect(1 - 0.0175, 0, 1 + 0.0175, mass_one,
         col = cei_colors[[label]], border = "black", lwd = 0.9)
    text(1, mass_one + 2.2, sprintf("%.1f%%", mass_one), cex = 0.85)
    axis(1, at = seq(max(0.3, lower), 1, by = 0.1), las = 1)
    axis(2, at = seq(0, 80, by = 10),
         labels = paste0(seq(0, 80, by = 10), "%"), las = 1)
    box(bty = "l")
  }
  mtext("Distribution of Collective Efficiency Index", side = 3,
        outer = TRUE, line = 0.7, cex = 1.25)
}

png(regular_output_path("figure_cei_distribution"),
    width = 3000, height = 1400, res = 300)
draw_cei_distribution()
dev.off()
pdf(regular_output_path("figure_cei_distribution", ".pdf"), width = 10, height = 4.7)
draw_cei_distribution()
dev.off()

collapse_cei_points <- function(z) {
  out <- aggregate(
    rep(1, nrow(z)),
    by = list(ccei_g = z$ccei_g, cei = z$e_coll_tempered),
    FUN = sum
  )
  names(out)[3] <- "frequency"
  out
}

draw_ccei_cei_scatter <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 2), mar = c(4.5, 4.8, 3.2, 1.0))

  for (label in names(cei_by_wave)) {
    z <- cei_by_wave[[label]]
    point_data <- collapse_cei_points(z)
    correlation <- cor(z$ccei_g, z$e_coll_tempered)
    n_both_one <- sum(z$ccei_g >= 1 - 1e-9 & z$e_coll_tempered >= 1 - 1e-9)
    plot(
      point_data$ccei_g, point_data$cei,
      xlim = c(0.15, 1.015), ylim = c(0.15, 1.015),
      xaxs = "i", yaxs = "i", xlab = "Group CCEI",
      ylab = if (label == "Baseline") "Collective Efficiency Index" else "",
      main = sprintf("%s (N = %d)", label, nrow(z)), type = "n", axes = FALSE
    )
    grid(nx = NULL, ny = NULL, col = "grey88", lty = 1)
    abline(a = 0, b = 1, lty = 2, col = "grey55", lwd = 1)
    points(
      point_data$ccei_g, point_data$cei, pch = 21,
      bg = adjustcolor("#2C6DA4", alpha.f = 0.68),
      col = "white", cex = 0.72, lwd = 0.35
    )
    axis(1, at = seq(0.2, 1.0, by = 0.2))
    axis(2, at = seq(0.2, 1.0, by = 0.2), las = 1)
    box(bty = "l")
    text(
      0.18, 0.985,
      labels = sprintf(
        "Correlation = %.3f\nN at (1, 1) = %d (%.1f%%)",
        correlation, n_both_one, 100 * n_both_one / nrow(z)
      ),
      adj = c(0, 1), cex = 0.85
    )
  }
}

png(regular_output_path("figure_group_ccei_cei_scatter"),
    width = 3000, height = 1500, res = 300)
draw_ccei_cei_scatter()
dev.off()
pdf(regular_output_path("figure_group_ccei_cei_scatter", ".pdf"), width = 10, height = 5)
draw_ccei_cei_scatter()
dev.off()

draw_ccei_cei_gap_cdf <- function() {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  gap_values <- lapply(cei_by_wave, function(z) z$ccei_g - z$e_coll_tempered)
  raw_x_range <- range(unlist(gap_values))
  x_range <- c(floor(raw_x_range[1] * 5) / 5, ceiling(raw_x_range[2] * 5) / 5)
  x_ticks <- seq(x_range[1], x_range[2], by = 0.2)
  par(mar = c(4.7, 5.0, 1.0, 1.2))
  plot(
    ecdf(gap_values$Baseline), verticals = TRUE, do.points = FALSE,
    col = cei_colors[["Baseline"]], lwd = 2.2, lty = 1,
    xlim = x_range, ylim = c(0, 1),
    xlab = "Group CCEI - Collective Efficiency Index",
    ylab = "Cumulative probability", main = "", axes = FALSE
  )
  plot(ecdf(gap_values$Endline), verticals = TRUE, do.points = FALSE,
       col = cei_colors[["Endline"]], lwd = 2.2, lty = 2, add = TRUE)
  axis(1, at = x_ticks, labels = sprintf("%.1f", x_ticks), cex.axis = 0.82)
  axis(2, at = seq(0, 1, by = 0.1),
       labels = paste0(seq(0, 100, by = 10), "%"), las = 1)
  abline(h = seq(0, 1, by = 0.1), col = "grey85", lty = 3, lwd = 0.8)
  abline(v = x_ticks, col = "grey85", lty = 3, lwd = 0.8)
  plot(ecdf(gap_values$Baseline), verticals = TRUE, do.points = FALSE,
       col = cei_colors[["Baseline"]], lwd = 2.2, lty = 1, add = TRUE)
  plot(ecdf(gap_values$Endline), verticals = TRUE, do.points = FALSE,
       col = cei_colors[["Endline"]], lwd = 2.2, lty = 2, add = TRUE)
  abline(v = 0, col = "black", lty = 2, lwd = 1.3)
  box(bty = "l")
  legend(
    "topleft", legend = c("Baseline", "Endline"),
    col = unname(cei_colors), lty = c(1, 2), lwd = 2.2, bty = "n"
  )
}

png(regular_output_path("figure_group_ccei_minus_cei_cdf"),
    width = 2100, height = 1500, res = 300)
draw_ccei_cei_gap_cdf()
dev.off()
pdf(regular_output_path("figure_group_ccei_minus_cei_cdf", ".pdf"), width = 7, height = 5)
draw_ccei_cei_gap_cdf()
dev.off()
