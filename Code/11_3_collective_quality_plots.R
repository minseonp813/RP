# Run from Code after 11_collective_quality.do, or source from 07_Figures_Main.R.
if (!exists("code_dir")) code_dir <- getwd()
.libPaths(c(file.path(code_dir, ".R-library"), .libPaths()))
library(ggplot2)
source(file.path(code_dir, "programs/plot_collective_ame.R"))
sample <- haven::read_dta(file.path(code_dir, "data/collective_analysis_sample.dta"))
member_sd <- c(maximum = sd(sample$ccei_max), minimum = sd(sample$ccei_min))
collective_result_dir <- file.path(code_dir, "results/collective_quality")
collective_figure_dir <- file.path(code_dir, "results/figures/collective_rationality")
dir.create(collective_figure_dir, recursive = TRUE, showWarnings = FALSE)
ame_files <- c(
  plot_cei_ame(file.path(collective_result_dir, "joint_outcome_linear_ame.csv"),
               collective_figure_dir, classification = "a", portrait = TRUE, prefix = "figure8_lpm",
               member_sd = member_sd),
  plot_cei_ame(file.path(collective_result_dir, "joint_outcome_ame.csv"),
               collective_figure_dir, classification = "a", portrait = TRUE, member_sd = member_sd)
)
draft_figure_dir <- file.path(code_dir, "..", "Overleaf/figures_2025/collective_rationality")
dir.create(draft_figure_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(ame_files, file.path(draft_figure_dir, basename(ame_files)), overwrite = TRUE))
