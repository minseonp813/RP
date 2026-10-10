.libPaths(c('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code/.R-library',.libPaths()))
setwd('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code')
Rcpp::sourceCpp('programs/rp_donor_costs.cpp')
source('programs/calculate_rp_indices.R')
pairs <- haven::read_dta('data/panel_final.dta')
raw <- do.call(rbind,lapply(c('base','end'),function(wave) {
  d <- rp_load_wave(paste0('data/',wave,'_raw.dta'));d$post <- as.integer(wave=='end');d
}))
raw <- raw[raw$group_id %in% as.character(pairs$group_id),]
individual <- split(raw[raw$round_number<=18,],paste(raw$group_id[raw$round_number<=18],raw$post[raw$round_number<=18],raw$id[raw$round_number<=18],sep='|'))
group <- raw[raw$round_number>=19 & raw$mover==1,]
group <- split(group,paste(group$group_id,group$post,sep='|'))
source('programs/build_placebo_donor_matrices.R')
paths <- list.files('results/benchmarks/maxmpi/donor_matrix_chunks',pattern='[.]csv$',full.names=TRUE)
cache <- do.call(rbind,lapply(paths,read.csv,colClasses=c(target_group_id='character',member1_id='character',member2_id='character',donor_group_id='character')))
set.seed(20261006)
rows <- sample(seq_len(nrow(cache)),100)
err <- parallel::mclapply(rows,function(r) {
 d <- cache[r,]
 i1 <- individual[[paste(d$target_group_id,d$post,d$member1_id,sep='|')]]
 i2 <- individual[[paste(d$target_group_id,d$post,d$member2_id,sep='|')]]
 g <- group[[paste(d$donor_group_id,d$post,sep='|')]]
 blocks <- list(i1,i2,rbind(i1,i2))
 errors <- numeric(3)
 for(k in 1:3) {
  E <- rp_expenditure(rbind(blocks[[k]],g))$E
  side <- c(rep('I',nrow(blocks[[k]])),rep('G',nrow(g)))
  bound <- rp_donor_mpi(E,side,branch_limit=128L)$value
  value <- rp_donor_mpi_exact(E,side,lower=bound)
  errors[k] <- abs(value-d[[c('cost1','cost2','cost12')[k]]])
 }
 stopifnot(all(errors<1e-10))
 max(errors)
},mc.cores=8,mc.preschedule=FALSE)
stopifnot(all(vapply(err,is.numeric,logical(1))))
cat('SUCCESS: 300 completed full-benchmark costs independently match the uncapped mixed-integer solver; maximum error',max(unlist(err)),'\n')
