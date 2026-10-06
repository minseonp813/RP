# Latest update: 2026-10-06
# Purpose: reproduce sampled review benchmarks with the shared production builder.
# Inputs: the balanced wide panel_final.dta and base/end choice data.
# Outputs: outputs/benchmarks/{hm_sample50,maxmpi_sample20}. These remain pilot
# estimates, distinct from full M in 01. Set PLACEBO_REVIEW_BENCHMARK_DIR to a new
# folder when retaining old caches, which have no input/config identity.
# Sections: 1 locate/load inputs; 2 specify and build sampled review benchmarks.
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

# 2. Preserve the historical 50-donor HM and 20-donor MaxMPI specifications.
specs <- list(
  list(measure = "hm", folder = "hm_sample50", max_donors = 50L),
  list(measure = "maxmpi", folder = "maxmpi_sample20", max_donors = 20L)
)

for (spec in specs) {
  build_placebo_donor_matrix(
    spec$measure, code_dir, pairs, base, end,
    output_dir = file.path(benchmark_dir, spec$folder),
    chunk_size = 8L, cores = as.integer(Sys.getenv("PLACEBO_CORES", "8")),
    max_targets = 0L, max_donors = spec$max_donors,
    cost_timeout = if (spec$measure == "maxmpi") 2 else 0
  )
}
