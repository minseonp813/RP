rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(haven)
  library(stringr)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) == 1) {
  project_dir <- dirname(normalizePath(sub("^--file=", "", file_arg)))
} else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  project_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  project_dir <- getwd()
}
setwd(project_dir)

table_path <- "ML/Tables/Table5.txt"
panel_path <- "data/panel_individual.dta"

panel_outputs <- tibble::tribble(
  ~panel, ~output_path, ~height,
  "A. Demographics and skills", "ML/Figures/Figure_clan_selected_boosting_demographics.pdf", 3.2,
  "B. Friendship network", "ML/Figures/Figure_clan_selected_boosting_network.pdf", 2.9,
  "C. Classroom environment", "ML/Figures/Figure_clan_selected_boosting_classroom.pdf", 4.3
)

selection <- tibble::tribble(
  ~variable, ~panel, ~label, ~order,
  "mathscore_diff", "A. Demographics and skills", "Math-score difference", 1,
  "outgoing_diff", "A. Demographics and skills", "Outgoingness difference", 2,
  "height_diff", "A. Demographics and skills", "Height difference", 3,
  "malepair_01", "A. Demographics and skills", "Female with male partner", 4,
  "malepair_10", "A. Demographics and skills", "Male with female partner", 5,
  "malepair_11", "A. Demographics and skills", "Male-male pair", 6,
  "inclass_n_friends_i", "B. Friendship network", "Number of in-class friends", 1,
  "inclass_n_diff", "B. Friendship network", "In-class friends difference", 2,
  "inclass_popularity_i", "B. Friendship network", "In-class popularity", 3,
  "inclass_popularity_diff", "B. Friendship network", "In-class popularity difference", 4,
  "friendship_mutual", "B. Friendship network", "Mutual friendship", 5,
  "post", "C. Classroom environment", "Endline", 1,
  "pblclass_horizontal_i", "C. Classroom environment", "Horizontal PBL", 2,
  "pblclass_horizontal_diff", "C. Classroom environment", "Horizontal PBL difference", 3,
  "teacher_induce_i", "C. Classroom environment", "Teacher induces participation", 4,
  "teacher_induce_diff", "C. Classroom environment", "Teacher induction difference", 5,
  "peer_fair_i", "C. Classroom environment", "Peer-rated fairness", 6,
  "peer_fair_diff", "C. Classroom environment", "Peer-rated fairness difference", 7,
  "class_outcast_i", "C. Classroom environment", "Class outcast", 8,
  "class_outcast_diff", "C. Classroom environment", "Class outcast difference", 9
)

split_tex_row <- function(line) {
  line |>
    str_remove("\\\\\\\\\\s*$") |>
    str_split("&", simplify = FALSE) |>
    unlist() |>
    str_trim() |>
    str_replace_all("\\\\_", "_")
}

parse_ci <- function(value) {
  as.numeric(str_split(str_remove_all(value, "[()]"), ",", simplify = TRUE))
}

lines <- readLines(table_path, warn = FALSE)
rows <- vector("list", nrow(selection))

for (i in seq_len(length(lines) - 2)) {
  fields <- split_tex_row(lines[i])
  if (length(fields) != 7 || !fields[1] %in% selection$variable) {
    next
  }

  ci_fields <- split_tex_row(lines[i + 1])
  p_fields <- split_tex_row(lines[i + 2])
  ci <- parse_ci(ci_fields[4])

  rows[[match(fields[1], selection$variable)]] <- tibble::tibble(
    variable = fields[1],
    difference_table = as.numeric(fields[4]),
    ci_low_table = ci[1],
    ci_high_table = ci[2],
    p_adjusted = as.numeric(str_remove_all(p_fields[4], "[\\[\\]]"))
  )
}

if (any(vapply(rows, is.null, logical(1)))) {
  missing <- selection$variable[vapply(rows, is.null, logical(1))]
  stop("Selected variables missing from Table5.txt: ", paste(missing, collapse = ", "))
}

estimates <- bind_rows(rows)

panel_data <- read_dta(panel_path) |>
  mutate(
    malepair_01 = as.numeric(male_i == 0 & male_j == 1),
    malepair_10 = as.numeric(male_i == 1 & male_j == 0),
    malepair_11 = as.numeric(male_i == 1 & male_j == 1),
    friendship_mutual = as.numeric(friendship == 2)
  )

full_sample_sd <- vapply(
  selection$variable,
  function(v) sd(as.numeric(panel_data[[v]]), na.rm = TRUE),
  numeric(1)
)

plot_data <- estimates |>
  left_join(selection, by = "variable") |>
  mutate(
    full_sample_sd = full_sample_sd[match(variable, selection$variable)],
    # Table 5 reports top-score minus bottom-score. Since lower effects on I_ig
    # imply a larger bargaining-power premium, reverse the comparison here.
    estimate = -difference_table / full_sample_sd,
    conf_low = -ci_high_table / full_sample_sd,
    conf_high = -ci_low_table / full_sample_sd,
    p_text = if_else(
      p_adjusted < 0.001,
      "p < 0.001",
      paste0("p = ", sprintf("%.3f", p_adjusted))
    ),
    plot_label = paste0(label, "  (", p_text, ")"),
    panel = factor(panel, levels = unique(selection$panel))
  ) |>
  arrange(panel, order)

plot_data$plot_label <- factor(
  plot_data$plot_label,
  levels = rev(plot_data$plot_label)
)

common_limits <- c(
  floor(min(plot_data$conf_low) * 4) / 4,
  ceiling(max(plot_data$conf_high) * 4) / 4
)
common_breaks <- seq(
  ceiling(common_limits[1] * 2) / 2,
  floor(common_limits[2] * 2) / 2,
  by = 0.5
)

make_panel_plot <- function(panel_name) {
  panel_plot_data <- plot_data |>
    filter(as.character(panel) == panel_name) |>
    droplevels()

  ggplot(panel_plot_data, aes(x = estimate, y = plot_label)) +
    geom_vline(xintercept = 0, linewidth = 0.35, colour = "grey55") +
    geom_errorbar(
      aes(xmin = conf_low, xmax = conf_high),
      width = 0.18,
      linewidth = 0.55,
      colour = "grey35"
    ) +
    geom_point(
      aes(fill = p_adjusted < 0.05),
      shape = 21,
      size = 2.6,
      stroke = 0.6,
      colour = "black"
    ) +
    scale_x_continuous(breaks = common_breaks, limits = common_limits) +
    scale_fill_manual(values = c(`TRUE` = "#2166AC", `FALSE` = "white"), guide = "none") +
    labs(
      x = "Standardized difference: larger minus smaller rationality premium",
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(colour = "black"),
      axis.title.x = element_text(margin = margin(t = 8)),
      plot.margin = margin(8, 12, 6, 6)
    )
}

dir.create("ML/Figures", recursive = TRUE, showWarnings = FALSE)

for (i in seq_len(nrow(panel_outputs))) {
  panel_plot <- make_panel_plot(panel_outputs$panel[i])
  ggsave(
    panel_outputs$output_path[i],
    panel_plot,
    width = 7.2,
    height = panel_outputs$height[i],
    device = cairo_pdf
  )
  cat("Saved:", panel_outputs$output_path[i], "\n")
}

print(plot_data |>
  select(panel, label, estimate, conf_low, conf_high, p_adjusted))
