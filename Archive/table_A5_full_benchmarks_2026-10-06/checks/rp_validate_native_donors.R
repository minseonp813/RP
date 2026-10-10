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
for(measure in c('hm','maxmpi')) {
  paths <- list.files(paste0('IminusM_review/outputs/benchmarks/',if(measure=='hm')'hm_sample50' else 'maxmpi_sample20','/donor_matrix_chunks'),pattern='[.]csv$',full.names=TRUE)
  cache <- do.call(rbind,lapply(paths,read.csv,colClasses=c(target_group_id='character',member1_id='character',member2_id='character',donor_group_id='character')))
  stopifnot(nrow(cache)==1304*(if(measure=='hm')51 else 21))
  started <- Sys.time()
  groups <- split(seq_len(nrow(cache)),ceiling(seq_len(nrow(cache))/500))
  results <- parallel::mclapply(groups,function(rows) {
    max_error <- 0;checked <- 0;own_checked <- 0
    for(r in rows) {
      d <- cache[r,]
      i1 <- individual[[paste(d$target_group_id,d$post,d$member1_id,sep='|')]]
      i2 <- individual[[paste(d$target_group_id,d$post,d$member2_id,sep='|')]]
      g <- group[[paste(d$donor_group_id,d$post,sep='|')]]
      stopifnot(nrow(i1)==18,nrow(i2)==18,nrow(g)==18)
      blocks <- list(i1,i2,rbind(i1,i2))
      costs <- numeric(3)
      for(k in 1:3) {
        reference <- d[[c('cost1','cost2','cost12')[k]]]
        if(!is.finite(reference) && d$is_own!=1) {costs[k]<-NA_real_;next}
        EX <- rp_expenditure(rbind(blocks[[k]],g))
        side <- c(rep('I',nrow(blocks[[k]])),rep('G',nrow(g)))
        z <- if(measure=='hm') rp_donor_hm(EX$E,side) else rp_donor_mpi(EX$E,side)
        if(measure=='maxmpi'){if(!z$exhausted)stop(sprintf('MaxMPI branch limit: row %s, cost %s, target %s donor %s',r,k,d$target_group_id,d$donor_group_id));z <- z$value}
        costs[k] <- z
        reference <- d[[c('cost1','cost2','cost12')[k]]]
        if(is.finite(reference)) {
          error <- abs(z-reference)
          if(error>1e-10)stop(sprintf('%s cached cost mismatch: row %s cost %s, native %.17g reference %.17g',measure,r,k,z,reference))
          max_error <- max(max_error,error);checked <- checked+1
        }
      }
      if(d$is_own==1) {
        row <- match(d$target_group_id,as.character(pairs$group_id));wave <- if(d$post==0)'base' else 'end'
        mover_first <- as.character(pairs[[paste0('id_mover_',wave)]][row])==d$member1_id
        actual <- pairs[[paste0('Ihat_',measure,'_',if(mover_first)'1g_' else '2g_',wave)]][row]
        index <- rp_index(costs[1],costs[2],costs[3])
        stopifnot(is.na(index)==is.na(actual))
        if(is.finite(actual))stopifnot(abs(index-actual)<1e-10)
        own_checked <- own_checked+1
      }
    }
    c(error=max_error,costs=checked,own=own_checked)
  },mc.cores=8,mc.preschedule=FALSE)
  saveRDS(results, '/private/tmp/rp_native_validation_results.rds')
  failed <- !vapply(results,is.numeric,logical(1))
  if(any(failed))stop(paste(unlist(results[failed]),collapse='\n'))
  results <- do.call(rbind,results)
  cat('PASS:',measure,';',nrow(cache),'saved donor rows;',sum(results[,'costs']),'finite saved costs;',sum(results[,'own']),'own-pair indices; maximum cost error',max(results[,'error']),'; seconds',as.numeric(difftime(Sys.time(),started,units='secs')),'\n')
  flush.console()
}
cat('SUCCESS: native kernels match all saved pilot costs and all own-pair indices.\n')
