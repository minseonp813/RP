rm(list = ls())
options(error = NULL)

distance_var <- "Ihat_ig"
suppressPackageStartupMessages({
  library(haven)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ggpattern)
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
table_dir <- file.path(code_dir, "results", "tables")
regular_output_path <- function(stem, extension = ".png") {
  file.path(result_dir, paste0(stem, extension))
}

panel_individual <- read_dta(file.path(data_dir, "panel_individual.dta"))

sig_mark <- function(p) {
  dplyr::case_when(
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.10 ~ "+",
    TRUE ~ ""
  )
}

cdf_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.18, 0.84),
    legend.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

paper_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA)
  )

make_wave_cdf <- function(data, value, x_label, x_limits = c(0, 1)) {
  ggplot(
    data,
    aes(
      x = .data[[value]],
      colour = wave,
      linetype = wave
    )
  ) +
    stat_ecdf(geom = "step", linewidth = 0.8, pad = FALSE) +
    scale_colour_manual(values = c("Baseline" = "blue", "Endline" = "red")) +
    scale_linetype_manual(values = c("Baseline" = "solid", "Endline" = "dashed")) +
    scale_x_continuous(
      limits = x_limits,
      breaks = seq(0, 1, by = 0.2),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      expand = c(0, 0)
    ) +
    labs(
      x = x_label,
      y = "Cumulative Frequency",
      colour = NULL,
      linetype = NULL
    ) +
    cdf_theme
}

# Figure A4

individual_ccei_data <- panel_individual |>
  filter(!is.na(ccei_i), !is.na(post)) |>
  distinct(id, post, ccei_i) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

group_ccei_data <- panel_individual |>
  filter(!is.na(ccei_g), !is.na(post)) |>
  distinct(group_id, post, ccei_g) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

individual_ccei_cdf <- make_wave_cdf(
  individual_ccei_data, "ccei_i", "Individual CCEI"
)
group_ccei_cdf <- make_wave_cdf(
  group_ccei_data, "ccei_g", "Group CCEI", c(0.1, 1)
)

ggsave(
  regular_output_path("figure_individual_ccei_cdf_baseline_endline"),
  individual_ccei_cdf, width = 7, height = 5, dpi = 300
)
ggsave(
  regular_output_path("figure_group_ccei_cdf_baseline_endline"),
  group_ccei_cdf, width = 7, height = 5, dpi = 300
)

alternative_cdf_specs <- tibble::tribble(
  ~measure, ~label, ~individual, ~group,
  "hm_index", "HM Index", "hm_index_i", "hm_index_g",
  "revmaxmpi", "RevMaxMPI", "revmaxmpi_i", "revmaxmpi_g"
)

alternative_cdf_data <- panel_individual |>
  mutate(
    hm_index_i = 1 - hm_i / 18,
    hm_index_g = 1 - hm_g / 18,
    revmaxmpi_i = 1 - maxmpi_i,
    revmaxmpi_g = 1 - maxmpi_g
  )

for (spec_index in seq_len(nrow(alternative_cdf_specs))) {
  spec <- alternative_cdf_specs[spec_index, ]

  individual_data <- alternative_cdf_data |>
    filter(!is.na(.data[[spec$individual]]), !is.na(post)) |>
    distinct(id, post, value = .data[[spec$individual]]) |>
    mutate(
      wave = factor(
        if_else(post == 0, "Baseline", "Endline"),
        levels = c("Baseline", "Endline")
      )
    )

  group_data <- alternative_cdf_data |>
    filter(!is.na(.data[[spec$group]]), !is.na(post)) |>
    distinct(group_id, post, value = .data[[spec$group]]) |>
    mutate(
      wave = factor(
        if_else(post == 0, "Baseline", "Endline"),
        levels = c("Baseline", "Endline")
      )
    )

  individual_plot <- make_wave_cdf(
    individual_data, "value", paste("Individual", spec$label)
  )
  group_plot <- make_wave_cdf(
    group_data, "value", paste("Group", spec$label), c(0.1, 1)
  )

  ggsave(
    regular_output_path(paste0(
      "figure_individual_", spec$measure, "_cdf_baseline_endline"
    )),
    individual_plot, width = 7, height = 5, dpi = 300
  )
  ggsave(
    regular_output_path(paste0(
      "figure_group_", spec$measure, "_cdf_baseline_endline"
    )),
    group_plot, width = 7, height = 5, dpi = 300
  )
}

# Figure A5

individual_ra_data <- panel_individual |>
  filter(!is.na(RA_i), !is.na(post)) |>
  distinct(id, post, RA_i) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

group_ra_data <- panel_individual |>
  filter(!is.na(RA_g), !is.na(post)) |>
  distinct(group_id, post, RA_g) |>
  mutate(wave = factor(if_else(post == 0, "Baseline", "Endline"),
                       levels = c("Baseline", "Endline")))

individual_ra_cdf <- make_wave_cdf(
  individual_ra_data, "RA_i", "Individual Risk Aversion"
)
group_ra_cdf <- make_wave_cdf(
  group_ra_data, "RA_g", "Group Risk Aversion"
)

ggsave(
  regular_output_path("figure_individual_ra_cdf_baseline_endline"),
  individual_ra_cdf, width = 7, height = 5, dpi = 300
)
ggsave(
  regular_output_path("figure_group_ra_cdf_baseline_endline"),
  group_ra_cdf, width = 7, height = 5, dpi = 300
)

# Figure A6

histogram_data <- panel_individual |>
  filter(!is.na(HighCCEI_both_high), !is.na(.data[[distance_var]])) |>
  mutate(
    member = factor(
      HighCCEI_both_high,
      levels = c(0, 1),
      labels = c("Lower CCEI", "Higher CCEI")
    )
  ) |>
  group_by(member) |>
  mutate(percent_weight = 100 / n()) |>
  ungroup()

histogram_higher <- histogram_data |>
  filter(member == "Higher CCEI")
histogram_lower <- histogram_data |>
  filter(member == "Lower CCEI")

hist_bargaining_index <- ggplot() +
  geom_histogram(
    data = histogram_higher,
    aes(
      x = .data[[distance_var]],
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
    data = histogram_lower,
    aes(
      x = .data[[distance_var]],
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
  regular_output_path("hist_bargaining_index"),
  hist_bargaining_index,
  width = 7.2,
  height = 4.6,
  dpi = 400,
  bg = "white"
)

# Figure A8

plot_definitions <- tibble::tribble(
  ~definition, ~ccei_var, ~file_suffix,
  "bothhigh", "HighCCEI_both_high", "",
  "bothlow", "HighCCEI_both_low", "_bothlow"
)

make_absolute_ra_distance_cdf <- function(data, wave_value, ccei_var) {
  plot_data <- data |>
    mutate(
      HighCCEI_plot = as.numeric(.data[[ccei_var]])
    ) |>
    filter(
      post == wave_value,
      !is.na(RA_i),
      !is.na(RA_g),
      !is.na(HighCCEI_plot)
    ) |>
    mutate(
      absolute_RA_distance = abs(RA_i - RA_g),
      rationality_group = factor(
        if_else(HighCCEI_plot == 1, "High Rationality", "Low Rationality"),
        levels = c("Low Rationality", "High Rationality")
      )
    )

  ggplot(
    plot_data,
    aes(
      absolute_RA_distance,
      colour = rationality_group,
      linetype = rationality_group
    )
  ) +
    stat_ecdf(geom = "step", linewidth = 0.9, pad = FALSE) +
    scale_colour_manual(
      values = c("Low Rationality" = "blue", "High Rationality" = "red")
    ) +
    scale_linetype_manual(
      values = c("Low Rationality" = "dashed", "High Rationality" = "solid")
    ) +
    scale_x_continuous(
      limits = c(0, 0.5),
      breaks = seq(0, 0.5, 0.1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      expand = c(0, 0)
    ) +
    labs(
      x = expression("|" * RA[i] - RA[g] * "|"),
      y = "Cumulative Frequency",
      colour = NULL,
      linetype = NULL
    ) +
    cdf_theme
}

for (definition_index in seq_len(nrow(plot_definitions))) {
  ccei_var <- plot_definitions$ccei_var[definition_index]
  file_suffix <- plot_definitions$file_suffix[definition_index]
  absolute_ra_cdf_baseline <- make_absolute_ra_distance_cdf(panel_individual, 0, ccei_var)
  absolute_ra_cdf_endline <- make_absolute_ra_distance_cdf(panel_individual, 1, ccei_var)

  ggsave(
    file.path(result_dir, paste0("risk_preference_distance_cdf_baseline", file_suffix, ".png")),
    absolute_ra_cdf_baseline, width = 7, height = 5, dpi = 300
  )
  ggsave(
    file.path(result_dir, paste0("risk_preference_distance_cdf_endline", file_suffix, ".png")),
    absolute_ra_cdf_endline, width = 7, height = 5, dpi = 300
  )
}

# Figure A9

for (definition_index in seq_len(nrow(plot_definitions))) {
  ccei_var <- plot_definitions$ccei_var[definition_index]
  file_suffix <- plot_definitions$file_suffix[definition_index]

ra_data <- panel_individual |>
  mutate(
    denominator = (RA_i - RA_g)^2 + (RA_j - RA_g)^2,
    RA_I_ig = if_else(
      denominator > 0,
      (RA_i - RA_g)^2 / denominator,
      NA_real_
    ),
    HighCCEI_plot = as.numeric(.data[[ccei_var]]),
    ccei_group = factor(
      if_else(HighCCEI_plot == 1, "Higher CCEI", "Lower CCEI"),
      levels = c("Lower CCEI", "Higher CCEI")
    )
  ) |>
  filter(!is.na(RA_I_ig), !is.na(ccei_group))

ra_stats <- ra_data |>
  group_by(ccei_group) |>
  summarise(
    mean = mean(RA_I_ig),
    sd = sd(RA_I_ig),
    n = n(),
    se = sd / sqrt(n),
    ci = qt(0.975, n - 1) * se,
    .groups = "drop"
  )

ra_test <- t.test(RA_I_ig ~ ccei_group, data = ra_data)
ra_difference <- ra_stats$mean[ra_stats$ccei_group == "Lower CCEI"] -
  ra_stats$mean[ra_stats$ccei_group == "Higher CCEI"]
ra_y <- max(ra_stats$mean + ra_stats$ci) + 0.045

ra_bar <- ggplot(ra_stats, aes(ccei_group, mean, fill = ccei_group)) +
  geom_col(width = 0.62, colour = "black", linewidth = 0.3) +
  geom_errorbar(
    aes(ymin = mean - ci, ymax = mean + ci),
    width = 0.16, linewidth = 0.45
  ) +
  annotate("segment", x = 1, xend = 2, y = ra_y, yend = ra_y) +
  annotate("segment", x = 1, xend = 1, y = ra_y - 0.008, yend = ra_y) +
  annotate("segment", x = 2, xend = 2, y = ra_y - 0.008, yend = ra_y) +
  annotate(
    "text", x = 1.5, y = ra_y + 0.025,
    label = sprintf("Diff. = %.3f%s", ra_difference, sig_mark(ra_test$p.value)),
    size = 5
  ) +
  scale_fill_manual(values = c("Lower CCEI" = "#D99A99", "Higher CCEI" = "#80ADD0")) +
  scale_x_discrete(labels = c("Lower\nCCEI", "Higher\nCCEI")) +
  scale_y_continuous(limits = c(0, ra_y + 0.06), expand = c(0, 0)) +
  labs(
    x = NULL,
    y = expression("Mean risk-aversion distance d(" * RA[i] * "," * RA[g] * ")")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 28),
    plot.background = element_rect(fill = "white", colour = NA)
  )

ra_cdf <- ggplot(
  ra_data,
  aes(RA_I_ig, colour = ccei_group, linetype = ccei_group)
) +
  stat_ecdf(geom = "step", linewidth = 0.8, pad = FALSE) +
  scale_colour_manual(values = c("Lower CCEI" = "red", "Higher CCEI" = "blue")) +
  scale_linetype_manual(values = c("Lower CCEI" = "dashed", "Higher CCEI" = "solid")) +
  scale_x_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
  ) +
  labs(
    x = expression("Risk-aversion distance d(" * RA[i] * "," * RA[g] * ")"),
    y = "Cumulative probability",
    colour = NULL,
    linetype = NULL
  ) +
  cdf_theme

ggsave(
  file.path(result_dir, paste0("ra_distance_by_ccei_bar", file_suffix, ".png")),
  ra_bar, width = 6, height = 5, dpi = 300
)
ggsave(
  file.path(result_dir, paste0("ra_distance_by_ccei_cdf", file_suffix, ".png")),
  ra_cdf, width = 6, height = 5, dpi = 300
)
}

ra_rank_specs <- tibble::tribble(
  ~measure, ~high_both, ~high_low,
  "hm", "HighHM_both_high", "HighHM_both_low",
  "maxmpi", "HighMaxMPI_both_high", "HighMaxMPI_both_low"
)

make_ra_distance_by_rank <- function(data, high_var, stem) {
  ra_data <- data |>
    mutate(
      denominator = (RA_i - RA_g)^2 + (RA_j - RA_g)^2,
      RA_I_ig = if_else(
        denominator > 0,
        (RA_i - RA_g)^2 / denominator,
        NA_real_
      ),
      rank_group = factor(
        if_else(.data[[high_var]] == 1, "Higher rationality", "Lower rationality"),
        levels = c("Lower rationality", "Higher rationality")
      )
    ) |>
    filter(!is.na(RA_I_ig), !is.na(rank_group))

  ra_stats <- ra_data |>
    group_by(rank_group) |>
    summarise(
      mean = mean(RA_I_ig),
      sd = sd(RA_I_ig),
      n = n(),
      se = sd / sqrt(n),
      ci = qt(0.975, n - 1) * se,
      .groups = "drop"
    )

  ra_test <- t.test(RA_I_ig ~ rank_group, data = ra_data)
  ra_difference <- ra_stats$mean[ra_stats$rank_group == "Lower rationality"] -
    ra_stats$mean[ra_stats$rank_group == "Higher rationality"]
  ra_y <- max(ra_stats$mean + ra_stats$ci) + 0.045

  ra_bar <- ggplot(ra_stats, aes(rank_group, mean, fill = rank_group)) +
    geom_col(width = 0.62, colour = "black", linewidth = 0.3) +
    geom_errorbar(
      aes(ymin = mean - ci, ymax = mean + ci),
      width = 0.16, linewidth = 0.45
    ) +
    annotate("segment", x = 1, xend = 2, y = ra_y, yend = ra_y) +
    annotate("segment", x = 1, xend = 1, y = ra_y - 0.008, yend = ra_y) +
    annotate("segment", x = 2, xend = 2, y = ra_y - 0.008, yend = ra_y) +
    annotate(
      "text", x = 1.5, y = ra_y + 0.025,
      label = sprintf("Diff. = %.3f%s", ra_difference, sig_mark(ra_test$p.value)),
      size = 5
    ) +
    scale_fill_manual(values = c(
      "Lower rationality" = "#D99A99", "Higher rationality" = "#80ADD0"
    )) +
    scale_x_discrete(labels = c(
      "Lower rationality" = "Lower\nrationality",
      "Higher rationality" = "Higher\nrationality"
    )) +
    scale_y_continuous(limits = c(0, ra_y + 0.06), expand = c(0, 0)) +
    labs(
      x = NULL,
      y = expression("Mean risk-aversion distance d(" * RA[i] * "," * RA[g] * ")")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 10, 28),
      plot.background = element_rect(fill = "white", colour = NA)
    )

  ra_cdf <- ggplot(
    ra_data,
    aes(RA_I_ig, colour = rank_group, linetype = rank_group)
  ) +
    stat_ecdf(geom = "step", linewidth = 0.8, pad = FALSE) +
    scale_colour_manual(values = c(
      "Lower rationality" = "red", "Higher rationality" = "blue"
    )) +
    scale_linetype_manual(values = c(
      "Lower rationality" = "dashed", "Higher rationality" = "solid"
    )) +
    scale_x_continuous(
      limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
    ) +
    labs(
      x = expression("Risk-aversion distance d(" * RA[i] * "," * RA[g] * ")"),
      y = "Cumulative probability",
      colour = NULL,
      linetype = NULL
    ) +
    cdf_theme

  ggsave(file.path(result_dir, paste0(stem, "_bar.png")),
         ra_bar, width = 6, height = 5, dpi = 300)
  ggsave(file.path(result_dir, paste0(stem, "_cdf.png")),
         ra_cdf, width = 6, height = 5, dpi = 300)
}

for (spec_index in seq_len(nrow(ra_rank_specs))) {
  spec <- ra_rank_specs[spec_index, ]
  make_ra_distance_by_rank(
    panel_individual,
    spec$high_both,
    paste0("ra_distance_by_", spec$measure)
  )
  make_ra_distance_by_rank(
    panel_individual,
    spec$high_low,
    paste0("ra_distance_by_", spec$measure, "_bothlow")
  )
}

# Alternative-index figures

rank_specs <- tibble::tribble(
  ~measure, ~label, ~distance, ~high_both, ~high_low,
  "hm", "HM index", "Ihat_hm_ig", "HighHM_both_high", "HighHM_both_low",
  "maxmpi", "RevMaxMPI", "Ihat_maxmpi_ig", "HighMaxMPI_both_high", "HighMaxMPI_both_low"
)

make_rank_histogram <- function(data, outcome, high_var, label, stem) {
  histogram_data <- data |>
    filter(!is.na(.data[[high_var]]), !is.na(.data[[outcome]])) |>
    mutate(
      member = factor(
        .data[[high_var]],
        levels = c(0, 1),
        labels = c("Less rational", "More rational")
      )
    ) |>
    group_by(member) |>
    mutate(percent_weight = 100 / n()) |>
    ungroup()

  more_rational <- histogram_data |>
    filter(member == "More rational")
  less_rational <- histogram_data |>
    filter(member == "Less rational")

  histogram <- ggplot() +
    geom_histogram(
      data = more_rational,
      aes(
        x = .data[[outcome]],
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
      data = less_rational,
      aes(
        x = .data[[outcome]],
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
      breaks = c("More rational", "Less rational"),
      values = c(
        "Less rational" = "#E39695",
        "More rational" = "#74A9CF"
      ),
      labels = c(
        paste0("More rational member (higher ", label, ")"),
        paste0("Less rational member (lower ", label, ")")
      )
    ) +
    scale_colour_manual(
      breaks = c("More rational", "Less rational"),
      values = c(
        "Less rational" = "#E39695",
        "More rational" = "#2C6DA4"
      ),
      labels = c(
        paste0("More rational member (higher ", label, ")"),
        paste0("Less rational member (lower ", label, ")")
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
    file.path(result_dir, paste0(stem, ".png")),
    histogram,
    width = 7.2,
    height = 4.6,
    dpi = 400,
    bg = "white"
  )
}

for (k in seq_len(nrow(rank_specs))) {
  s <- rank_specs[k, ]
  make_rank_histogram(
    panel_individual, s$distance, s$high_both, s$label,
    paste0("hist_revealed_preference_distance_", s$measure, "_bothhigh")
  )
  make_rank_histogram(
    panel_individual, s$distance, s$high_low, s$label,
    paste0("hist_revealed_preference_distance_", s$measure, "_bothlow")
  )
}

make_rank_distance <- function(data, outcome, high_var, label, stem) {
  plot_data <- data |>
    transmute(
      distance = .data[[outcome]],
      rank = factor(
        if_else(.data[[high_var]] == 1, "More rational", "Less rational"),
        levels = c("Less rational", "More rational")
      )
    ) |>
    filter(!is.na(distance), !is.na(rank))

  stats <- plot_data |>
    group_by(rank) |>
    summarise(
      mean = mean(distance),
      sd = sd(distance),
      n = n(),
      se = sd / sqrt(n),
      ci = qt(0.975, n - 1) * se,
      .groups = "drop"
    )

  test <- t.test(distance ~ rank, data = plot_data)
  difference <- diff(rev(stats$mean))
  difference_label <- sprintf(
    "Diff. = %.3f%s", difference, sig_mark(test$p.value)
  )
  y <- max(stats$mean + stats$ci) + 0.08

  bar <- ggplot(stats, aes(rank, mean, fill = rank)) +
    geom_col(width = 0.62, colour = "black", linewidth = 0.3) +
    geom_errorbar(
      aes(ymin = mean - ci, ymax = mean + ci),
      width = 0.16, linewidth = 0.45
    ) +
    annotate("segment", x = 1, xend = 2, y = y, yend = y) +
    annotate("segment", x = 1, xend = 1, y = y - 0.025, yend = y) +
    annotate("segment", x = 2, xend = 2, y = y - 0.025, yend = y) +
    annotate("text", x = 1.5, y = y + 0.04, label = difference_label, size = 5) +
    scale_fill_manual(values = c(
      "Less rational" = "#D99A99", "More rational" = "#80ADD0"
    )) +
    scale_x_discrete(labels = c(
      "Less rational" = "Less\nrational", "More rational" = "More\nrational"
    )) +
    scale_y_continuous(limits = c(0, y + 0.09), expand = c(0, 0)) +
    labs(x = NULL, y = "Mean revealed preference distance") +
    paper_theme +
    theme(legend.position = "none")

  cdf <- ggplot(plot_data, aes(distance, colour = rank, linetype = rank)) +
    stat_ecdf(geom = "step", linewidth = 0.9, pad = FALSE) +
    scale_colour_manual(values = c(
      "Less rational" = "red", "More rational" = "blue"
    )) +
    scale_linetype_manual(values = c(
      "Less rational" = "dashed", "More rational" = "solid"
    )) +
    scale_x_continuous(
      limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
    ) +
    labs(
      x = "Revealed preference distance",
      y = "Cumulative probability",
      colour = NULL,
      linetype = NULL
    ) +
    paper_theme +
    theme(
      legend.position = c(0.22, 0.85),
      legend.background = element_rect(fill = "white", colour = "black")
    )

  ggsave(file.path(result_dir, paste0(stem, "_bar.png")),
         bar, width = 6, height = 5, dpi = 300)
  ggsave(file.path(result_dir, paste0(stem, "_cdf.png")),
         cdf, width = 6, height = 5, dpi = 300)
}
for (k in seq_len(nrow(rank_specs))) {
  s <- rank_specs[k, ]
  make_rank_distance(panel_individual, s$distance, s$high_both, s$label,
                     paste0("rp_distance_", s$measure, "_bothhigh"))
  make_rank_distance(panel_individual, s$distance, s$high_low, s$label,
                     paste0("rp_distance_", s$measure, "_bothlow"))
}

survey_bar_alt <- function(data, outcome, survey, labels) {
  plot_data <- data |>
    filter(
      !is.na(.data[[outcome]]),
      .data[[survey]] %in% seq_along(labels)
    ) |>
    group_by(response = .data[[survey]]) |>
    summarise(
      mean = mean(.data[[outcome]], na.rm = TRUE),
      sd = sd(.data[[outcome]], na.rm = TRUE),
      n = sum(!is.na(.data[[outcome]])),
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
  if (identical(survey, "risk_whose_i") && "4" %in% names(fill_values)) {
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
    labs(x = NULL, y = "Mean Revealed Preference Distance") +
    paper_theme +
    theme(
      legend.position = "none",
      panel.grid.minor.y = element_line(colour = "grey92", linewidth = 0.3)
    )
}
whose_labels <- c("1" = "Mostly Partner's", "2" = "Both",
                  "3" = "Mostly Mine", "4" = "Neither")
similar_labels <- c("1" = "Very Differently", "2" = "Somewhat Differently",
                    "3" = "Somewhat Similar", "4" = "Mostly Similar")
survey_data <- panel_individual |>
  mutate(RA_difference = abs(RA_i - RA_j))
for (k in seq_len(nrow(rank_specs))) {
  s <- rank_specs[k, ]
  full <- survey_data |> filter(!is.na(.data[[s$distance]]))
  cutoff <- median(full$RA_difference, na.rm = TRUE)
  high_ra <- full |> filter(!is.na(RA_difference), RA_difference >= cutoff)
  p1 <- survey_bar_alt(full, s$distance, "risk_whose_i", whose_labels)
  p2 <- survey_bar_alt(high_ra, s$distance, "risk_similar_i", similar_labels)
  ggsave(file.path(result_dir, paste0("validation_", s$measure, "_whose_suggestion.png")),
         p1, width = 7, height = 5, dpi = 300)
  ggsave(file.path(result_dir, paste0("validation_", s$measure, "_similarity.png")),
         p2, width = 7, height = 5, dpi = 300)
}

panel_alternative <- panel_individual |>
  mutate(
    hm_r_i = 1 - hm_i / 18,
    hm_r_g = 1 - hm_g / 18,
    maxmpi_r_i = 1 - maxmpi_i,
    maxmpi_r_g = 1 - maxmpi_g
  )

make_collective_category <- function(data, individual, collective, label, stem) {
  cutoff <- median(data[[individual]], na.rm = TRUE)

  group_data <- data |>
    filter(
      !is.na(group_id),
      !is.na(post),
      !is.na(.data[[individual]]),
      !is.na(.data[[collective]])
    ) |>
    group_by(group_id, post) |>
    summarise(
      value = first(.data[[collective]]),
      n_members = n(),
      n_high = sum(.data[[individual]] >= cutoff),
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
      mean = mean(value),
      sd = sd(value),
      n = n(),
      se = sd / sqrt(n),
      ci = qt(0.975, n - 1) * se,
      .groups = "drop"
    )

  pairwise_diff_alt <- function(data, lower_group, upper_group) {
    comparison <- data |>
      filter(pair_category %in% c(lower_group, upper_group)) |>
      droplevels()
    difference <- mean(
      comparison$value[comparison$pair_category == upper_group]
    ) - mean(
      comparison$value[comparison$pair_category == lower_group]
    )
    test <- t.test(value ~ pair_category, data = comparison)
    tibble(
      difference = difference,
      label = sprintf("Diff. = %.3f%s", difference, sig_mark(test$p.value))
    )
  }

  diff_extreme <- pairwise_diff_alt(group_data, "Low-Low", "High-High")
  diff_low_mid <- pairwise_diff_alt(group_data, "Low-Low", "Low-High")
  diff_mid_high <- pairwise_diff_alt(group_data, "Low-High", "High-High")

  group_y_min <- max(
    0.75,
    floor((min(group_stats$mean - group_stats$ci, na.rm = TRUE) - 0.02) * 100) / 100
  )
  group_y_top <- max(group_stats$mean + group_stats$ci, na.rm = TRUE)
  bracket_low <- group_y_top + 0.025
  bracket_high <- group_y_top + 0.070

  bar <- ggplot(group_stats, aes(pair_category, mean, fill = pair_category)) +
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
    labs(x = NULL, y = paste("Mean Collective", label)) +
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

  cdf <- ggplot(
    group_data,
    aes(value, colour = pair_category, linetype = pair_category)
  ) +
    stat_ecdf(geom = "step", linewidth = 0.9, pad = FALSE) +
    scale_colour_manual(values = c(
      "Low-Low" = "red", "Low-High" = "grey45", "High-High" = "blue"
    )) +
    scale_linetype_manual(values = c(
      "Low-Low" = "dashed", "Low-High" = "dotdash", "High-High" = "solid"
    )) +
    scale_x_continuous(
      limits = c(0.1, 1), breaks = seq(0.2, 1, 0.2), expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0, 0)
    ) +
    labs(
      x = paste("Collective", label),
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

  ggsave(file.path(result_dir, paste0(stem, "_bar.png")),
         bar, width = 6, height = 5, dpi = 300)
  ggsave(file.path(result_dir, paste0(stem, "_cdf.png")),
         cdf, width = 6, height = 5, dpi = 300)
}
make_collective_category(panel_alternative, "hm_r_i", "hm_r_g", "HM Index",
                         "collective_hm_by_member_category")
make_collective_category(panel_alternative, "maxmpi_r_i", "maxmpi_r_g", "RevMaxMPI",
                         "collective_maxmpi_by_member_category")

# Shapley figures

make_shapley_plot <- function(
    input_file,
    output_file,
    block_order,
    fill_values,
    pattern_values,
    pattern_angles) {
  if (!nzchar(tools::file_ext(output_file))) {
    output_file <- paste0(output_file, ".png")
  }
  if (!file.exists(input_file)) {
    warning(
      "Skipping Shapley figure because ", input_file,
      " does not exist. Allow 99_3_Tables_Appendix.do to finish first."
    )
    return(invisible(FALSE))
  }

  shapley <- read.csv(input_file, stringsAsFactors = FALSE)
  required <- c("block", "shapley_value", "shapley_percent", "total_r2")
  missing <- setdiff(required, names(shapley))
  if (length(missing) > 0) {
    stop("Missing Shapley columns: ", paste(missing, collapse = ", "))
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  shapley$block <- factor(shapley$block, levels = block_order)
  shapley$label <- sprintf(
    "%.3f (%.1f%%)",
    shapley$shapley_value,
    shapley$shapley_percent
  )

  plot <- ggplot(
    shapley,
    aes(
      x = "",
      y = shapley_value,
      fill = block,
      pattern = block,
      pattern_angle = block
    )
  ) +
    geom_col_pattern(
      width = 0.50,
      colour = "black",
      linewidth = 0.45,
      pattern_spacing = 0.045,
      pattern_density = 0.35,
      pattern_fill = "white",
      pattern_colour = "white"
    ) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 4
    ) +
    scale_fill_manual(
      name = "Block",
      values = fill_values,
      breaks = block_order,
      drop = FALSE
    ) +
    scale_pattern_manual(
      name = "Block",
      values = pattern_values,
      breaks = block_order,
      drop = FALSE
    ) +
    scale_pattern_angle_manual(values = pattern_angles, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = expression(R^2 ~ "Contribution")) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "right",
      plot.margin = margin(10, 20, 10, 10)
    )

  ggsave(
    output_file,
    plot,
    width = 6,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  invisible(TRUE)
}

shapley_specs <- tibble::tribble(
  ~input_file, ~output_file, ~primary_block, ~second_block, ~fe_block,
  file.path(table_dir, "shapley_bargaining_index.csv"),
    file.path(result_dir, "shapley_bargaining_index.png"),
    "CCEI", "Individual/Friendship", "Individual FE",
  file.path(table_dir, "shapley_bargaining_index_bothlow.csv"),
    file.path(result_dir, "shapley_bargaining_index_bothlow.png"),
    "CCEI", "Individual/Friendship", "Individual FE",
  file.path(table_dir, "shapley_bargaining_index_cceidiff.csv"),
    file.path(result_dir, "shapley_bargaining_index_cceidiff.png"),
    "CCEI", "Individual/Friendship", "Individual FE",
  file.path(table_dir, "shapley_bargaining_hm.csv"),
    file.path(result_dir, "shapley_bargaining_hm.png"),
    "HM Index", "Individual/Friendship", "Individual FE",
  file.path(table_dir, "shapley_bargaining_hm_bothlow.csv"),
    file.path(result_dir, "shapley_bargaining_hm_bothlow.png"),
    "HM Index", "Individual/Friendship", "Individual FE",
  file.path(table_dir, "shapley_bargaining_hm_hmdiff.csv"),
    file.path(result_dir, "shapley_bargaining_hm_hmdiff.png"),
    "HM Index", "Individual/Friendship", "Individual FE",
  file.path(table_dir, "shapley_bargaining_revmaxmpi.csv"),
    file.path(result_dir, "shapley_bargaining_revmaxmpi.png"),
    "RevMaxMPI", "Individual/Friendship", "Individual FE",
  file.path(table_dir, "shapley_bargaining_revmaxmpi_bothlow.csv"),
    file.path(result_dir, "shapley_bargaining_revmaxmpi_bothlow.png"),
    "RevMaxMPI", "Individual/Friendship", "Individual FE",
  file.path(table_dir, "shapley_bargaining_revmaxmpi_revmaxmpidiff.csv"),
    file.path(result_dir, "shapley_bargaining_revmaxmpi_revmaxmpidiff.png"),
    "RevMaxMPI", "Individual/Friendship", "Individual FE",
  file.path(table_dir, "shapley_collective_ccei.csv"),
    file.path(result_dir, "shapley_collective_ccei.png"),
    "CCEI", "Group/Friendship", "Pair FE",
  file.path(table_dir, "shapley_collective_hm.csv"),
    file.path(result_dir, "shapley_collective_hm.png"),
    "HM Index", "Group/Friendship", "Pair FE",
  file.path(table_dir, "shapley_collective_revmaxmpi.csv"),
    file.path(result_dir, "shapley_collective_revmaxmpi.png"),
    "RevMaxMPI", "Group/Friendship", "Pair FE",
  file.path(table_dir, "c_Ng", "shapley_collective_cNg.csv"),
    file.path(result_dir, "c_Ng", "shapley_collective_cNg.png"),
    "Individual CCEI", "Group/Friendship", "Pair FE",
  file.path(table_dir, "c_Ng", "shapley_collective_cNg_hm.csv"),
    file.path(result_dir, "c_Ng", "shapley_collective_cNg_hm.png"),
    "Individual HM Index", "Group/Friendship", "Pair FE",
  file.path(table_dir, "c_Ng", "shapley_collective_cNg_revmaxmpi.csv"),
    file.path(result_dir, "c_Ng", "shapley_collective_cNg_revmaxmpi.png"),
    "Individual RevMaxMPI", "Group/Friendship", "Pair FE"
)

for (spec_index in seq_len(nrow(shapley_specs))) {
  spec <- shapley_specs[spec_index, ]
  block_order <- c(
    spec$primary_block,
    spec$second_block,
    "Risk Aversion",
    "Corner/Midpoint Shares",
    spec$fe_block
  )
  make_shapley_plot(
    spec$input_file,
    spec$output_file,
    block_order,
    setNames(
      c("white", "#B7D5E1", "#A9E891", "#F2C6CF", "#C8C8C8"),
      block_order
    ),
    setNames(c("none", "stripe", "stripe", "stripe", "none"), block_order),
    setNames(c(0, -45, 45, 0, 0), block_order)
  )
}
