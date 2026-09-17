rm(list = ls())

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", grep("^--file=", script_args, value = TRUE))
if (length(script_file) == 1L) setwd(dirname(normalizePath(script_file)))
if (length(script_file) == 0L && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  source_file <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(source_file)) setwd(dirname(normalizePath(source_file)))
}

package_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source("programs/build_placebo_donor_matrices.R")

measures <- trimws(strsplit(Sys.getenv("PLACEBO_MEASURES", "ccei,hm,maxmpi"), ",")[[1]])
if (!all(measures %in% c("ccei", "hm", "maxmpi"))) stop("Unknown PLACEBO_MEASURES value.")

for (measure in measures) {
  cat("\n========== ", toupper(measure), " ==========\n", sep = "")
  build_placebo_donor_matrix(measure, package_dir)
}

cat("\nAll placebo donor matrices completed.\n")
