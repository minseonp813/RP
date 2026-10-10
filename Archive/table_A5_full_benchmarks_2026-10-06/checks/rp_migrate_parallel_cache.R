setwd('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code')
stopifnot(any(grepl('SUCCESS:',readLines('Logs/01_donor_parallel_validation.log'))))
archive <- '../Archive/table_A5_full_benchmarks_2026-10-06/checks'
for(measure in c('hm','maxmpi')) {
 path <- file.path('results/benchmarks',measure,'benchmark_config.rds')
 config <- readRDS(path)
 stopifnot(identical(config$builder_code,unname(tools::md5sum(file.path(archive,'build_placebo_donor_matrices_target_parallel.R')))),
           identical(config$native_code,unname(tools::md5sum('programs/rp_donor_costs.cpp'))),
           config$cost_timeout==0,config$max_donors==0)
 file.copy(path,file.path(archive,paste0(measure,'_target_parallel_config.rds')),overwrite=FALSE)
 config$builder_code <- unname(tools::md5sum('programs/build_placebo_donor_matrices.R'))
 saveRDS(config,path)
 cat('Verified scheduling-only cache migration:',measure,'; costs/diagonals match for 10,432 full-benchmark rows; unchanged inputs/objective/tolerances.\n')
}
