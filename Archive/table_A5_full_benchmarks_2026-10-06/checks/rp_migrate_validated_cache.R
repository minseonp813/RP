setwd('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code')
stopifnot(any(grepl('SUCCESS:',readLines('Logs/01_hybrid_donor_validation.log'))),
          any(grepl('SUCCESS:',readLines('Logs/01_mpi_bruteforce.log'))),
          any(grepl('SUCCESS:',readLines('Logs/01_mpi_completed_validation.log'))))
archive <- '../Archive/table_A5_full_benchmarks_2026-10-06/checks'
for(measure in c('hm','maxmpi')) {
 path <- file.path('results/benchmarks',measure,'benchmark_config.rds')
 config <- readRDS(path)
 stopifnot(config$max_donors==0L,config$cost_timeout==0,
           identical(config$numerical_code,unname(tools::md5sum('programs/calculate_rp_indices.R'))),
           identical(config$native_code,unname(tools::md5sum(file.path(archive,'rp_donor_costs_native_only.cpp')))),
           identical(config$builder_code,unname(tools::md5sum(file.path(archive,'build_placebo_donor_matrices_native_only.R')))))
 file.copy(path,file.path(archive,paste0(measure,'_native_only_config.rds')),overwrite=FALSE)
 config$native_code <- unname(tools::md5sum('programs/rp_donor_costs.cpp'))
 config$builder_code <- unname(tools::md5sum('programs/build_placebo_donor_matrices.R'))
 # Preserve manifest field ordering for the unchanged roster/raw inputs/settings.
 config <- append(config,list(highs_version=if(measure=='maxmpi')as.character(packageVersion('highs')) else NULL),after=which(names(config)=='native_code'))
 saveRDS(config,path)
 cat('Verified cache migration:',measure,'; same exact objective, inputs and settings; updated native/HiGHS implementation hashes only.\n')
}
