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

# CCEI is already complete. HM and MaxMPI are written inside this review
# package so pilot or manuscript outputs are never overwritten.
for (measure in c("hm", "maxmpi")) {
  output_dir <- file.path(review_dir, "outputs", "benchmarks", measure)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(
    PLACEBO_OUTPUT_DIR = output_dir,
    PLACEBO_TARGET_CHUNK_SIZE = "8",
    PLACEBO_CORES = Sys.getenv("PLACEBO_CORES", "8"),
    PLACEBO_MAX_TARGETS = "0",
    PLACEBO_MAX_DONORS = "0"
  )
  build_placebo_donor_matrix(measure, code_dir)
}
