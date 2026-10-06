# Latest update: 2026-10-06
# Purpose: review launcher for the RA benchmark, now implemented by the shared
# builder used in 01_calculate_indices.R.
# Inputs: the balanced wide panel_final.dta with choice-derived RA estimates.
# Outputs: outputs/benchmarks/ra/placebo_*; old outputs/data/ra_* stay available
# for comparing the old panel-input version against the new choice-input version.
# Sections: 1 locate/load inputs; 2 build the full RA donor benchmark.
rm(list = ls())

# 1. Locate the shared builder and load choice-derived risk-aversion estimates.
args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
review_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else normalizePath(getwd())
code_dir <- dirname(review_dir)
source(file.path(code_dir, "programs", "build_placebo_donor_matrices.R"))
pairs <- haven::read_dta(file.path(code_dir, "data", "panel_final.dta"))
benchmark_dir <- Sys.getenv("PLACEBO_REVIEW_BENCHMARK_DIR", file.path(review_dir, "outputs", "benchmarks"))

# 2. All non-own pairs in the same wave; no survey/imputation input from 05.
build_placebo_donor_matrix(
  "ra", code_dir, pairs, base = NULL, end = NULL,
  output_dir = file.path(benchmark_dir, "ra"),
  chunk_size = 8L, cores = as.integer(Sys.getenv("PLACEBO_CORES", "1")),
  max_targets = 0L, max_donors = 0L, cost_timeout = 0
)
