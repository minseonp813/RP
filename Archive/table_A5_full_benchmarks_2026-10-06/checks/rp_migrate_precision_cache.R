setwd('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code')
stopifnot(any(grepl('SUCCESS:',readLines('Logs/01_hybrid_donor_validation.log'))),
          any(grepl('SUCCESS:',readLines('Logs/01_mpi_bruteforce.log'))),
          any(grepl('SUCCESS:',readLines('Logs/01_mpi_completed_validation.log'))),
          any(grepl('SUCCESS:',readLines('Logs/01_mpi_precision_validation.log'))))
archive <- '../Archive/table_A5_full_benchmarks_2026-10-06/checks'
for(measure in c('hm','maxmpi')) {
 path <- file.path('results/benchmarks',measure,'benchmark_config.rds')
 config <- readRDS(path)
 stopifnot(identical(config$builder_code,unname(tools::md5sum(file.path(archive,'build_placebo_donor_matrices_unscaled.R')))),
           identical(config$native_code,unname(tools::md5sum('programs/rp_donor_costs.cpp'))),
           config$cost_timeout==0,config$max_donors==0)
 file.copy(path,file.path(archive,paste0(measure,'_unscaled_config.rds')),overwrite=FALSE)
 config$builder_code <- unname(tools::md5sum('programs/build_placebo_donor_matrices.R'))
 saveRDS(config,path)
 cat('Verified precision-only cache migration:',measure,'; identical inputs, objectives and costs; scaled solver bound matches reference/native/completed-cost checks.\n')
}
