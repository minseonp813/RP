rm(list = ls())

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
review_dir <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[1])))
} else {
  normalizePath(getwd())
}
code_dir <- dirname(review_dir)
setwd(code_dir)
source(file.path(review_dir, "build_placebo_donor_matrices_parallel.R"))

# HM and some exact MaxMPI cross-cost problems are computationally intensive.
# For the review stage, estimate both donor means from 50 reproducibly sampled
# non-own pairs per target. Exact full builds remain available through
# 04_build_full_rp_benchmarks.R if the adjusted outcome is adopted.
specs <- list(
  list(measure = "hm", folder = "hm_sample50", max_donors = "50"),
  list(measure = "maxmpi", folder = "maxmpi_sample20", max_donors = "20")
)

for (spec in specs) {
  output_dir <- file.path(review_dir, "outputs", "benchmarks", spec$folder)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(
    PLACEBO_OUTPUT_DIR = output_dir,
    PLACEBO_TARGET_CHUNK_SIZE = "8",
    PLACEBO_CORES = Sys.getenv("PLACEBO_CORES", "8"),
    PLACEBO_MAX_TARGETS = "0",
    PLACEBO_MAX_DONORS = spec$max_donors,
    PLACEBO_COST_TIMEOUT = if (spec$measure == "maxmpi") "2" else "0"
  )
  build_placebo_donor_matrix(spec$measure, code_dir)
}
