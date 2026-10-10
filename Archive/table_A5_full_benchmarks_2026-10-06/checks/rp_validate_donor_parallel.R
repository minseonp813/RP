setwd('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code')
source('programs/calculate_rp_indices.R')
source('programs/build_placebo_donor_matrices.R')
pairs <- haven::read_dta('data/panel_final.dta')
base <- rp_load_wave('data/base_raw.dta');end <- rp_load_wave('data/end_raw.dta')
for(chunk in c(0L,9L)) {
 # Select eight target rows for this scheduling check; keep all 652 same-wave donors.
 original <- build_placebo_donor_matrix
 body_text <- paste(readLines('programs/build_placebo_donor_matrices.R'),collapse='\n')
 old <- 'roster <- if (max_targets > 0L) head(roster_full, max_targets) else roster_full'
 replacement <- sprintf('roster <- roster_full[%s:%s, , drop = FALSE]',chunk*8L+1L,chunk*8L+8L)
 stopifnot(grepl(old,body_text,fixed=TRUE))
 prototype <- new.env(parent=.GlobalEnv)
 eval(parse(text=sub(old,replacement,body_text,fixed=TRUE)),prototype)
 original <- prototype$build_placebo_donor_matrix
 directory <- paste0('/private/tmp/rp_donor_parallel_',chunk)
 start <- Sys.time()
 original('maxmpi',normalizePath('.'),pairs,base,end,output_dir=directory,cores=8L)
 new <- readr::read_csv(file.path(directory,'placebo_donor_matrix.csv'),show_col_types=FALSE)
 old <- readr::read_csv(sprintf('results/benchmarks/maxmpi/donor_matrix_chunks/donor_matrix_chunk_%04d.csv',chunk),show_col_types=FALSE)
 stopifnot(identical(names(old),names(new)),nrow(old)==5216L,nrow(new)==5216L)
 for(column in names(old)) {
  if(is.numeric(old[[column]]))stopifnot(isTRUE(all.equal(old[[column]],new[[column]],tolerance=1e-10)))
  else stopifnot(identical(old[[column]],new[[column]]))
 }
 cat('PASS: donor-parallel chunk',chunk,'; all 5,216 rows and costs match; elapsed seconds',as.numeric(difftime(Sys.time(),start,units='secs')),'\n')
}
cat('SUCCESS: donor scheduling preserves completed full-benchmark costs and summaries.\n')
