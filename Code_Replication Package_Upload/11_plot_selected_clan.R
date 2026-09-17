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
  "B. Friendship network", "ML/Figures/Figure_clan_selected_boosting_network.pdf", 3.2,
  "C. Classroom environment", "ML/Figures/Figure_clan_selected_boosting_classroom.pdf", 3.2
)

selection <- tibble::tribble(
  ~variable, ~panel, ~label, ~order,
  "mathscore_i", "A. Demographics and skills", "Math~score[i]", 1,
  "height_i", "A. Demographics and skills", "Height[i]", 2,
  "malepair_01", "A. Demographics and skills", "Female[i]~\",\"~Male[j]", 3,
  "malepair_11", "A. Demographics and skills", "Male[i]~\",\"~Male[j]", 4,
  "outgoing_i", "A. Demographics and skills", "Outgoing[i]", 5,
  "inclass_popularity_i", "B. Friendship network", "\"In-degree\"[i]", 1,
  "inclass_popularity_diff", "B. Friendship network", "\"In-degree\"[diff]", 2,
  "inclass_n_friends_i", "B. Friendship network", "\"Out-degree\"[i]", 3,
  "inclass_n_diff", "B. Friendship network", "\"Out-degree\"[diff]", 4,
  "friendship_mutual", "B. Friendship network", "Mutual~friend[ij]", 5,
  "post", "C. Classroom environment", "Second~wave", 1,
  "pblclass_horizontal_i", "C. Classroom environment", "Horizontal~pedagogy[i]", 2,
  "teacher_induce_i", "C. Classroom environment", "Participation~encouraged[i]", 3,
  "peer_reciprocal_i", "C. Classroom environment", "Reciprocal~classmates[i]", 4,
  "class_outcast_i", "C. Classroom environment", "Excluded~classmates[i]", 5
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
    estimate = difference_table / full_sample_sd,
    conf_low = ci_low_table / full_sample_sd,
    conf_high = ci_high_table / full_sample_sd,
    plot_label = label,
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
      aes(
        fill = p_adjusted < 0.05,
        shape = p_adjusted < 0.05
      ),
      size = 4,
      stroke = 0.6,
      colour = "black"
    ) +
    scale_x_continuous(
      breaks = if (panel_name %in% c("A. Demographics and skills", "B. Friendship network")) {
        seq(-1.0, 0.5, by = 0.5)
      } else {
        common_breaks
      },
      limits = if (panel_name %in% c("A. Demographics and skills", "B. Friendship network")) {
        c(-1.0, 0.5)
      } else {
        common_limits
      }
    ) +
    scale_fill_manual(values = c(`TRUE` = "#2166AC", `FALSE` = "white"), guide = "none") +
    scale_shape_manual(values = c(`TRUE` = 24, `FALSE` = 21), guide = "none") +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 18) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(colour = "black"),
      axis.title.x = element_text(margin = margin(t = 8)),
      plot.margin = margin(0, 0, 0, 0)
    ) +
    scale_y_discrete(labels = function(labels) parse(text = labels))
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
