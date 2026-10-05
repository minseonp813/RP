## Reproduce the three bargaining-index figures with I_ig, then rerun with Ihat_ig
## Input:  data/panel_individual.dta
## Output: results/ihat_reanalysis/figures/

rm(list = ls())

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  context_path <- rstudioapi::getSourceEditorContext()$path
  if (!is.null(context_path) && !is.na(context_path) && context_path != "") {
    setwd(dirname(context_path))
  }
}

input_path <- "data/panel_individual.dta"
output_dir <- "results/ihat_reanalysis/figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

panel_individual <- read_dta(input_path)

required_vars <- c(
  "group_id", "id", "post", "HighCCEI",
  "I_ig", "Ihat_ig",
  "risk_whose_i", "risk_similar_i", "RA_dist"
)

missing_vars <- setdiff(required_vars, names(panel_individual))
if (length(missing_vars) > 0) {
  stop(
    "panel_individual.dta is missing required variables: ",
    paste(missing_vars, collapse = ", ")
  )
}

## Survey response labels.
## The raw files contain numeric responses but no embedded value labels.
## These labels follow the ordering implied by the validation question.
## If the questionnaire uses different wording, edit labels here only.
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

theme_paper <- function() {
  theme_classic(base_size = 12) +
    theme(
      axis.title = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      legend.position = "none",
      plot.margin = margin(8, 12, 8, 8)
    )
}

mean_ci <- function(df, group_var, index_var) {
  df %>%
    filter(
      !is.na(.data[[group_var]]),
      !is.na(.data[[index_var]])
    ) %>%
    group_by(group = .data[[group_var]]) %>%
    summarise(
      n = n(),
      mean = mean(.data[[index_var]]),
      sd = sd(.data[[index_var]]),
      se = sd / sqrt(n),
      critical = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
      lower = mean - critical * se,
      upper = mean + critical * se,
      .groups = "drop"
    )
}

significance_symbol <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.10 ~ "+",
    TRUE ~ ""
  )
}

save_plot <- function(plot, filename, width, height) {
  ggsave(
    filename = file.path(output_dir, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 400,
    bg = "white"
  )
}

make_validation_plot <- function(
    df,
    response_var,
    response_labels,
    index_var,
    x_title) {

  stats <- mean_ci(df, response_var, index_var) %>%
    mutate(
      group = as.character(group),
      share = n / sum(n),
      axis_label = paste0(
        unname(response_labels[group]),
        "\n(N=", n, ", ", scales::percent(share, accuracy = 1), ")"
      ),
      response = factor(
        axis_label,
        levels = axis_label[order(match(group, names(response_labels)))]
      )
    )

  bar_colors <- if (response_var == "risk_whose_i") {
    c("1" = "lightblue", "2" = "lightblue", "3" = "lightblue", "4" = "grey90")
  } else {
    c("1" = "lightblue", "2" = "lightblue", "3" = "lightblue", "4" = "lightblue")
  }

  plot <- ggplot(stats, aes(x = response, y = mean, fill = group)) +
    geom_col(width = 0.66, color = "black", linewidth = 0.35) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      width = 0.15,
      linewidth = 0.45
    ) +
    scale_fill_manual(values = bar_colors) +
    scale_y_continuous(
      limits = c(0, 0.8),
      breaks = seq(0, 0.8, by = 0.2),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = x_title,
      y = "Mean revealed-preference distance"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )

  list(plot = plot, stats = stats)
}

make_ccei_bar <- function(df, index_var) {
  stats <- mean_ci(df, "HighCCEI", index_var) %>%
    mutate(
      member = factor(
        group,
        levels = c(0, 1),
        labels = c("Lower CCEI", "Higher CCEI")
      )
    )

  test_data <- df %>%
    filter(!is.na(HighCCEI), !is.na(.data[[index_var]])) %>%
    select(group_id, post, HighCCEI, all_of(index_var)) %>%
    mutate(member = if_else(HighCCEI == 0, "lower", "higher")) %>%
    select(-HighCCEI) %>%
    pivot_wider(names_from = member, values_from = all_of(index_var)) %>%
    filter(!is.na(lower), !is.na(higher))

  test_result <- tryCatch(
    t.test(test_data$lower, test_data$higher, paired = TRUE),
    error = function(e) NULL
  )

  p_value <- if (is.null(test_result)) NA_real_ else test_result$p.value
  low_mean <- stats$mean[stats$group == 0]
  high_mean <- stats$mean[stats$group == 1]
  difference <- low_mean - high_mean
  difference_label <- paste0(
    "Diff. = ",
    sprintf("%.3f", difference),
    significance_symbol(p_value)
  )

  bracket_y <- min(0.94, max(stats$upper, na.rm = TRUE) + 0.09)

  plot <- ggplot(stats, aes(x = member, y = mean)) +
    geom_col(
      aes(fill = member),
      width = 0.62,
      color = "black",
      linewidth = 0.35
    ) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      width = 0.14,
      linewidth = 0.45
    ) +
    geom_segment(
      aes(x = 1, xend = 2, y = bracket_y, yend = bracket_y),
      inherit.aes = FALSE,
      linewidth = 0.45
    ) +
    geom_segment(
      aes(x = 1, xend = 1, y = bracket_y, yend = bracket_y - 0.025),
      inherit.aes = FALSE,
      linewidth = 0.45
    ) +
    geom_segment(
      aes(x = 2, xend = 2, y = bracket_y, yend = bracket_y - 0.025),
      inherit.aes = FALSE,
      linewidth = 0.45
    ) +
    annotate(
      "text",
      x = 1.5,
      y = bracket_y + 0.035,
      label = difference_label,
      size = 3.6
    ) +
    scale_fill_manual(values = c(
      "Lower CCEI" = "#E39695",
      "Higher CCEI" = "#74A9CF"
    )) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = NULL,
      y = "Mean revealed-preference distance"
    ) +
    theme_classic(base_size = 13) +
    theme(
      legend.position = "none",
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )

  list(
    plot = plot,
    stats = stats %>%
      mutate(
        difference_lower_minus_higher = difference,
        p_value = p_value
      )
  )
}

make_ccei_cdf <- function(df, index_var) {
  plot_data <- df %>%
    filter(!is.na(HighCCEI), !is.na(.data[[index_var]])) %>%
    mutate(
      member = factor(
        HighCCEI,
        levels = c(0, 1),
        labels = c("Lower CCEI", "Higher CCEI")
      )
    )

  ggplot(
    plot_data,
    aes(
      x = .data[[index_var]],
      color = member,
      linetype = member
    )
  ) +
    stat_ecdf(linewidth = 0.9, geom = "step") +
    scale_color_manual(
      breaks = c("Higher CCEI", "Lower CCEI"),
      values = c("Lower CCEI" = "red", "Higher CCEI" = "blue"),
      labels = c(
        "More rational member (higher CCEI)",
        "Less rational member (lower CCEI)"
      )
    ) +
    scale_linetype_manual(
      breaks = c("Higher CCEI", "Lower CCEI"),
      values = c("Lower CCEI" = "dashed", "Higher CCEI" = "solid"),
      labels = c(
        "More rational member (higher CCEI)",
        "Less rational member (lower CCEI)"
      )
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2)
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2)
    ) +
    labs(
      x = "Revealed-preference distance",
      y = "Empirical CDF",
      color = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      legend.background = element_rect(
        fill = "white", color = "black", linewidth = 0.3
      ),
      legend.box.background = element_rect(
        fill = "white", color = "black", linewidth = 0.3
      ),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
}

make_histogram <- function(df, index_var) {
  ggplot(
    df %>% filter(!is.na(.data[[index_var]])),
    aes(x = .data[[index_var]])
  ) +
    geom_histogram(
      binwidth = 0.05,
      boundary = 0,
      closed = "left",
      fill = "#4C78A8",
      color = "white",
      linewidth = 0.25
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      limits = c(0, 400),
      breaks = seq(0, 400, by = 100),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = "Revealed bargaining index",
      y = "Frequency"
    ) +
    theme_classic(base_size = 13) +
    theme(
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
}

## Alternative distribution figure:
## overlay the two CCEI-member distributions and express each bin as a
## percentage of that member type's observations. The original CDF and pooled
## frequency histogram above are retained unchanged.
make_ccei_percent_histogram <- function(df, index_var) {
  plot_data <- df %>%
    filter(!is.na(HighCCEI), !is.na(.data[[index_var]])) %>%
    mutate(
      member = factor(
        HighCCEI,
        levels = c(0, 1),
        labels = c("Lower CCEI", "Higher CCEI")
      )
    ) %>%
    group_by(member) %>%
    mutate(percent_weight = 100 / n()) %>%
    ungroup()

  higher_data <- plot_data %>% filter(member == "Higher CCEI")
  lower_data <- plot_data %>% filter(member == "Lower CCEI")

  ggplot() +
    ## Both distributions are translucent, so overlap remains visible.
    geom_histogram(
      data = higher_data,
      aes(
        x = .data[[index_var]],
        weight = percent_weight,
        fill = member,
        color = member
      ),
      binwidth = 0.05,
      boundary = 0,
      closed = "left",
      position = "identity",
      alpha = 0.45,
      linewidth = 0.45
    ) +
    ## The lower-CCEI distribution is shown in translucent red.
    geom_histogram(
      data = lower_data,
      aes(
        x = .data[[index_var]],
        weight = percent_weight,
        fill = member,
        color = member
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
    scale_color_manual(
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
      color = NULL
    ) +
    guides(
      color = "none",
      fill = guide_legend(
        override.aes = list(
          alpha = c(0.45, 0.45),
          color = c("#2C6DA4", "#E39695")
        )
      )
    ) +
    theme_classic(base_size = 13) +
    theme(
      legend.position = "bottom",
      legend.background = element_rect(
        fill = "white", color = "black", linewidth = 0.3
      ),
      legend.box.background = element_rect(
        fill = "white", color = "black", linewidth = 0.3
      ),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
}

## A second alternative: one pooled histogram, with each bin divided into
## lower- and higher-CCEI shares. The total height of a bin is its percentage
## of the full estimation sample.
make_ccei_stacked_percent_histogram <- function(df, index_var) {
  plot_data <- df %>%
    filter(!is.na(HighCCEI), !is.na(.data[[index_var]])) %>%
    mutate(
      member = factor(
        HighCCEI,
        levels = c(0, 1),
        labels = c("Lower CCEI", "Higher CCEI")
      ),
      percent_weight = 100 / n()
    )

  ggplot(
    plot_data,
    aes(
      x = .data[[index_var]],
      weight = percent_weight,
      fill = member
    )
  ) +
    geom_histogram(
      binwidth = 0.05,
      boundary = 0,
      closed = "left",
      position = "stack",
      color = "white",
      linewidth = 0.25
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
      y = "Percent of pooled sample",
      fill = NULL
    ) +
    theme_classic(base_size = 13) +
    theme(
      legend.position = "bottom",
      legend.background = element_rect(
        fill = "white", color = "black", linewidth = 0.3
      ),
      legend.box.background = element_rect(
        fill = "white", color = "black", linewidth = 0.3
      ),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black")
    )
}

## Define the high-disagreement sample once so I_ig and Ihat_ig use the same
## RA threshold. RA_dist is repeated for the two members of each pair-wave.
RA_pair_wave <- panel_individual %>%
  distinct(group_id, post, RA_dist) %>%
  filter(!is.na(RA_dist))

RA_median <- median(RA_pair_wave$RA_dist)

high_RA_disagreement <- panel_individual %>%
  filter(!is.na(RA_dist), RA_dist >= RA_median)

index_runs <- tibble(
  index_var = c("I_ig", "Ihat_ig"),
  file_tag = c("I_ig", "Ihat_ig")
)

for (run in seq_len(nrow(index_runs))) {
  index_var <- index_runs$index_var[run]
  file_tag <- index_runs$file_tag[run]

  cat("\nCreating figures for", index_var, "\n")

  validation_whose <- make_validation_plot(
    df = panel_individual,
    response_var = "risk_whose_i",
    response_labels = whose_labels,
    index_var = index_var,
    x_title = "Whose suggestions were reflected?"
  )

  validation_similar <- make_validation_plot(
    df = high_RA_disagreement,
    response_var = "risk_similar_i",
    response_labels = similar_labels,
    index_var = index_var,
    x_title = "Similarity of hypothetical own choices"
  )

  ccei_bar <- make_ccei_bar(panel_individual, index_var)
  ccei_cdf <- make_ccei_cdf(panel_individual, index_var)
  histogram <- make_histogram(panel_individual, index_var)
  ccei_percent_histogram <- make_ccei_percent_histogram(
    panel_individual,
    index_var
  )
  ccei_stacked_percent_histogram <- make_ccei_stacked_percent_histogram(
    panel_individual,
    index_var
  )

  save_plot(
    validation_whose$plot,
    paste0("ccei_bargaining_whose_suggestion_", file_tag, ".png"),
    width = 5.6,
    height = 4.2
  )
  save_plot(
    validation_similar$plot,
    paste0("ccei_bargaining_had_individual_high_", file_tag, ".png"),
    width = 5.6,
    height = 4.2
  )
  save_plot(
    ccei_bar$plot,
    paste0("bargaining_index_by_ccei_bar_", file_tag, ".png"),
    width = 5.6,
    height = 4.2
  )
  save_plot(
    ccei_cdf,
    paste0("bargaining_index_by_ccei_cdf_", file_tag, ".png"),
    width = 5.6,
    height = 4.2
  )
  save_plot(
    histogram,
    paste0("hist_bargaining_index_", file_tag, ".png"),
    width = 7.2,
    height = 4.6
  )
  save_plot(
    ccei_percent_histogram,
    paste0(
      "hist_bargaining_index_by_ccei_percent_",
      file_tag,
      ".png"
    ),
    width = 7.2,
    height = 4.6
  )
  save_plot(
    ccei_stacked_percent_histogram,
    paste0(
      "hist_bargaining_index_by_ccei_stacked_percent_",
      file_tag,
      ".png"
    ),
    width = 7.2,
    height = 4.6
  )

  write_csv(
    validation_whose$stats,
    file.path(
      output_dir,
      paste0("ccei_bargaining_whose_suggestion_", file_tag, "_stats.csv")
    )
  )
  write_csv(
    validation_similar$stats,
    file.path(
      output_dir,
      paste0("ccei_bargaining_had_individual_high_", file_tag, "_stats.csv")
    )
  )
  write_csv(
    ccei_bar$stats,
    file.path(
      output_dir,
      paste0("bargaining_index_by_ccei_bar_", file_tag, "_stats.csv")
    )
  )
}

cat("\nHigh-RA-disagreement cutoff (median RA_dist):", RA_median, "\n")
cat("Figures saved to:", output_dir, "\n")
