# Latest update: 2026-10-06
# Purpose: review launcher for full HM/MaxMPI benchmarks; production uses 01.
# Inputs: the balanced wide panel_final.dta and base/end choice data.
# Outputs: outputs/benchmarks/{hm,maxmpi}. Existing unverified caches are rejected;
# set PLACEBO_REVIEW_BENCHMARK_DIR to a new folder to retain old runs for comparison.
# Sections: 1 locate/load inputs; 2 run full uncapped benchmarks.
rm(list = ls())

# 1. Locate the shared builder and load the upstream choice/index data.
args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
review_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  normalizePath(getwd())
}
code_dir <- dirname(review_dir)
setwd(code_dir)

source(file.path(code_dir, "programs", "build_placebo_donor_matrices.R"))
source(file.path(code_dir, "programs", "calculate_rp_indices.R"))
pairs <- haven::read_dta(file.path(code_dir, "data", "panel_final.dta"))
base <- rp_load_wave(file.path(code_dir, "data", "base_raw.dta"))
end <- rp_load_wave(file.path(code_dir, "data", "end_raw.dta"))
benchmark_dir <- Sys.getenv("PLACEBO_REVIEW_BENCHMARK_DIR", file.path(review_dir, "outputs", "benchmarks"))

# 2. Calculate all non-own donor comparisons, explicitly without a time cap.
for (measure in c("hm", "maxmpi")) {
  build_placebo_donor_matrix(
    measure, code_dir, pairs, base, end,
    output_dir = file.path(benchmark_dir, measure),
    chunk_size = 8L, cores = as.integer(Sys.getenv("PLACEBO_CORES", "8")),
    max_targets = 0L, max_donors = 0L, cost_timeout = 0
  )
}
