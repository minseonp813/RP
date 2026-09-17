rm(list = ls())

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", grep("^--file=", script_args, value = TRUE))
if (length(script_file) == 1L) setwd(dirname(normalizePath(script_file)))
if (length(script_file) == 0L && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  source_file <- rstudioapi::getSourceEditorContext()$path
  if (nzchar(source_file)) setwd(dirname(normalizePath(source_file)))
}

source("programs/build_placebo_donor_matrix_alternative.R")
build_placebo_donor_matrix("hm", normalizePath(getwd(), winslash = "/"))
