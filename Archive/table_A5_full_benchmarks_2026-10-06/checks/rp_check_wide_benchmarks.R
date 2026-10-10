setwd('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code')
old <- haven::read_dta('../Archive/table_A5_full_benchmarks_2026-10-06/Code/data/panel_final.dta')
new <- haven::read_dta('data/panel_final.dta')
stopifnot(nrow(new)==652L,identical(old$group_id,new$group_id),all(names(old)%in%names(new)))
unchanged <- setdiff(names(old),grep('^(M|n|nvalid|degfrac)_(hm|maxmpi)',names(old),value=TRUE))
for(column in unchanged)stopifnot(isTRUE(all.equal(old[[column]],new[[column]],check.attributes=FALSE,tolerance=0)))
for(measure in c('hm','maxmpi')) {
 a <- haven::read_dta(file.path('results/benchmarks',measure,'placebo_normalized_member_wave.dta'))
 for(wave in c('base','end'))for(member in 1:2) {
  role <- if(member==1L)'mover' else 'nonmover'
  rows <- match(paste(new$group_id,as.integer(wave=='end'),new[[paste0('id_',role,'_',wave)]],sep='|'),paste(a$group_id,a$post,a$id,sep='|'))
  stopifnot(!anyNA(rows),all(new[[paste0('n_',measure,'_donors_',member,'_',wave)]]==651L),
            max(abs(new[[paste0('M_',measure,'_',member,'_',wave)]]-a$M_all_imp[rows]))<1e-12)
 }
}
cat('PASS: wide panel keeps all 652 pairs and every original value outside the HM/MaxMPI benchmark fields; full M maps by member ID and wave.\n')
