.libPaths(c('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code/.R-library',.libPaths()))
setwd('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code')
measures <- commandArgs(trailingOnly=TRUE)
for(measure in measures) {
  directory <- file.path('results/benchmarks',measure)
  config <- readRDS(file.path(directory,'benchmark_config.rds'))
  stopifnot(config$max_targets==0,config$max_donors==0,config$cost_timeout==0)
  d <- readr::read_csv(file.path(directory,'placebo_donor_matrix.csv'),show_col_types=FALSE,progress=FALSE,
    col_select=c(target_group_id,post,donor_group_id,is_own,cost1,cost2,cost12,Ihat1_donor,Ihat2_donor),
    col_types=readr::cols(.default=readr::col_double(),target_group_id=readr::col_character(),donor_group_id=readr::col_character()))
  a <- haven::read_dta(file.path(directory,'placebo_normalized_member_wave.dta'))
  stopifnot(nrow(d)==850208,nrow(a)==2608,all(a$n_all==651),all(is.finite(a$M_all_imp)),
    all(a$M_all_imp>=0 & a$M_all_imp<=1),sum(d$is_own)==1304,
    !anyDuplicated(d[c('target_group_id','post','donor_group_id')]),
    all(d$cost1>=0 & d$cost2>=0 & d$cost12>=0),
    all(d$cost1<=d$cost12+1e-10),all(d$cost2<=d$cost12+1e-10),
    all(is.na(d$Ihat1_donor)==is.na(d$Ihat2_donor)))
  valid <- !is.na(d$Ihat1_donor)
  stopifnot(all(d$Ihat1_donor[valid]>= -1e-10 & d$Ihat1_donor[valid]<=1+1e-10),
    max(abs(d$Ihat1_donor[valid]+d$Ihat2_donor[valid]-1))<1e-10)
  key <- paste(a$group_id,a$post,a$id,sep='|')
  partner <- match(paste(a$group_id,a$post,a$partner_id,sep='|'),key)
  stopifnot(!anyNA(partner),max(abs(a$M_all_imp+a$M_all_imp[partner]-1))<1e-10)
  cat('PASS:',measure,'850,208 donor rows; 2,608 member-wave benchmarks; all 651 donors; no target/donor/time cap; monotonic costs and adding-up identities pass.\n')
}
