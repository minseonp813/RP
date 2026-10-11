# Run from Code after 99_8_within_wave_median.do.
# Optional argument: class_wave, matching the estimation argument.
# Reuse Figure 7's plot code and inference; write sensitivity outputs separately.
suppressPackageStartupMessages({
  library(haven)
  library(ggplot2)
  library(dplyr)
  library(grid)
})
code_dir <- getwd()
source(file.path(code_dir, "programs/class_fe_category_summary.R"))
args <- commandArgs(trailingOnly = TRUE)
median_scope <- if (length(args)) args[1] else "wave"
stopifnot(median_scope %in% c("wave", "class_wave"))
result_dir <- paste0("results/collective_quality/within_", median_scope, "_median")
analysis_data <- read_dta(file.path(result_dir, "analysis_sample.dta"))
strata <- if (median_scope == "class_wave") c("class", "post") else "post"
cutoff <- paste0(median_scope, "_median")
members <- bind_rows(
  analysis_data |> select(all_of(strata), individual_ccei = ccei_min),
  analysis_data |> select(all_of(strata), individual_ccei = ccei_max))
cutoff_checks <- members |> group_by(across(all_of(strata))) |>
  summarise(check_median = median(individual_ccei),
            check_ties = sum(individual_ccei == check_median), .groups = "drop")
checked <- left_join(analysis_data, cutoff_checks, by = strata)
stopifnot(nrow(analysis_data) == 1304, !anyDuplicated(analysis_data[c("group_id", "post")]),
          all(checked[[cutoff]] == checked$check_median),
          all(checked$median_ties == checked$check_ties),
          all(analysis_data$pair_category ==
              (analysis_data$ccei_min > analysis_data[[cutoff]]) +
              (analysis_data$ccei_max > analysis_data[[cutoff]])),
          all(analysis_data$ties_high_pair_category ==
              (analysis_data$ccei_min >= analysis_data[[cutoff]]) +
              (analysis_data$ccei_max >= analysis_data[[cutoff]])))
# The original individual panel must give exactly the same pair classifications.
individual <- read_dta("data/panel_individual.dta") |>
  group_by(across(all_of(strata))) |>
  mutate(high = ccei_i > median(ccei_i), ties_high = ccei_i >= median(ccei_i)) |>
  group_by(group_id, post) |>
  summarise(individual_category = sum(high), individual_ties_high = sum(ties_high), .groups = "drop")
checked <- left_join(analysis_data, individual, by = c("group_id", "post"))
stopifnot(!anyNA(checked$individual_category),
          all(checked$pair_category == checked$individual_category),
          all(checked$ties_high_pair_category == checked$individual_ties_high))
counts <- analysis_data |>
  group_by(post, pair_category) |>
  summarise(n = n(), .groups = "drop")
write.csv(counts, file.path(result_dir, "wave_category_counts.csv"), row.names = FALSE)
transitions <- analysis_data |>
  count(post, pooled_pair_category, pair_category)
write.csv(transitions, file.path(result_dir, "category_transitions.csv"), row.names = FALSE)

main_file <- readLines("07_Figures_Main.R")
main_expressions <- parse(text = main_file)
for (expression in main_expressions) {
  if (is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      is.symbol(expression[[2]]) &&
      as.character(expression[[2]]) %in% c("sig_mark", "paper_theme")) {
    eval(expression)
  }
}
first <- which(grepl("category_data$pair_category <- factor", main_file, fixed = TRUE))
last <- which(grepl('cat("Pooled individual CCEI median:', main_file, fixed = TRUE)) - 1L
stopifnot(length(first) == 1, length(last) == 1, last > first)
figure_code <- paste(main_file[first:last], collapse = "\n")
figure_code <- sub('draft_figure_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")',
                   'draft_figure_dir <- collective_result_dir', figure_code, fixed = TRUE)
figure_code <- sub('High: individual CCEI above pooled median %.7f across both waves.',
                   'High: %s.',
                   figure_code, fixed = TRUE)
figure_code <- sub('category_pooled_median, paste(category_stats$n, collapse = " / ")',
                   'classification_label, paste(category_stats$n, collapse = " / ")',
                   figure_code, fixed = TRUE)
figure_code <- sub('stopifnot(file.copy(panel_files, file.path(draft_figure_dir, basename(panel_files)), overwrite = TRUE))',
                   'figure7_plots[[outcome]] <- list(bar = bar, cdf = cdf)', figure_code, fixed = TRUE)
draw_figure <- function(data, output_dir, classification_label, figure_title, filename) {
  category_data <- data
  collective_result_dir <- collective_figure_dir <- output_dir
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  figure7_plots <- list()
  eval(parse(text = figure_code))
  category_counts <- table(factor(data$pair_category, levels = 0:2))
  png(file.path(output_dir, filename),
      width = 3000, height = 2850, res = 300)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(4, 2, heights = unit(c(.06, .43, .43, .08), "npc"))))
  grid.text(figure_title, gp = gpar(fontsize = 17, fontface = "bold"),
            vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  for (row in 1:2) {
    outcome <- c("ccei", "ceiv")[row]
    for (column in 1:2) {
      print(figure7_plots[[outcome]][[column]], newpage = FALSE,
            vp = viewport(layout.pos.row = row + 1, layout.pos.col = column))
    }
  }
  grid.text(sprintf("High: %s.\n1,304 pair-waves; LL / LH / HH: %s. Class FE; class-clustered contrasts.\n95%% normal mean intervals; ordinary KS tests. + p < .10, * p < .05, ** p < .01.",
                    classification_label, paste(category_counts, collapse = " / ")), gp = gpar(fontsize = 10),
            vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2))
  popViewport()
  dev.off()
}

if (median_scope == "wave") {
  wave_medians <- cutoff_checks$check_median[order(cutoff_checks$post)]
  label <- sprintf("CCEI above own-wave median (baseline %.7f; endline %.7f)", wave_medians[1], wave_medians[2])
  title <- "Figure 7: classification by the within-wave median"
} else {
  label <- "CCEI strictly above own class-wave median; median ties assigned Low"
  title <- "Figure 7: medians within each class and wave"
}
draw_figure(analysis_data, result_dir, label, title,
            paste0("figure7_within_", median_scope, "_median.png"))
if (median_scope == "class_wave") {
  ties_data <- analysis_data
  ties_data$pair_category <- ties_data$ties_high_pair_category
  draw_figure(ties_data, file.path(result_dir, "ties_high"),
              "CCEI at or above own class-wave median; median ties assigned High",
              "Figure 7: class-wave medians, ties assigned High",
              "figure7_class_wave_median_ties_high.png")
}

# Reproducing the existing pooled fits verifies sample, controls, and inference.
coefficients <- read.csv(file.path(result_dir, "table_coefficients.csv"))
reference <- read.csv("results/collective_quality/table5_coefficients.csv") |>
  filter(column <= 3)
baseline <- coefficients |> filter(definition == "pooled") |>
  left_join(reference, by = c("outcome", "column", "term"), suffix = c("", "_reference"))
stopifnot(nrow(baseline) == 12, !anyNA(baseline),
          max(abs(baseline$estimate - baseline$estimate_reference)) < 1e-12,
          max(abs(baseline$se - baseline$se_reference)) < 1e-12,
          max(abs(baseline$p - baseline$p_reference)) < 1e-12)
diagnostics <- read.csv(file.path(result_dir, "table_diagnostics.csv"))
stars <- function(p) ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .10, "+", "")))
export_table <- function(selected_definition, output_path) {
  wave <- coefficients |> filter(definition == .env$selected_definition)
  table_lines <- c("\\begin{tabular}{lccc}\\toprule",
                   "& (1) & (2) & (3)\\\\\\midrule")
  for (outcome in c("ccei", "ceiv")) {
    table_lines <- c(table_lines, sprintf("\\multicolumn{4}{l}{\\textit{Group %s}}\\\\[3pt]", toupper(outcome)))
    for (term in c("low_low", "high_high")) {
      cells <- wave |> filter(.data$outcome == .env$outcome, .data$term == .env$term) |> arrange(column)
      label <- if (term == "low_low") "Low--Low pair" else "High--High pair"
      table_lines <- c(table_lines,
        paste0(label, " & ", paste(sprintf("%.3f\\textsuperscript{%s}", cells$estimate, stars(cells$p)), collapse = " & "), "\\\\"),
        paste0(" & ", paste(sprintf("(%.3f)", cells$se), collapse = " & "), "\\\\"))
    }
    fits <- diagnostics |> filter(definition == .env$selected_definition, .data$outcome == .env$outcome) |> arrange(column)
    table_lines <- c(table_lines, "Observations & 1,304 & 1,304 & 1,304\\\\",
      paste0("R-squared & ", paste(sprintf("%.3f", fits$r2), collapse = " & "), "\\\\\\midrule"))
  }
  table_lines <- c(table_lines, "Fixed effects & Class & Class & Pair\\\\",
                   "Full controls & & $\\checkmark$ & $\\checkmark$\\\\",
                   "\\bottomrule\\end{tabular}")
  writeLines(table_lines, output_path)
  print(wave)
}
export_table(median_scope, file.path(result_dir, paste0("tableA7_within_", median_scope, "_median.tex")))
if (median_scope == "class_wave") {
  export_table("class_ties", file.path(result_dir, "ties_high/tableA7_class_wave_median_ties_high.tex"))
}
cat("SUCCESS: baseline replicated; median sensitivity figures and tables saved separately.\n")
