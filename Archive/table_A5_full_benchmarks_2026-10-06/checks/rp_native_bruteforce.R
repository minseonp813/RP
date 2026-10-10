.libPaths(c('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code/.R-library',.libPaths()))
setwd('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code')
Rcpp::sourceCpp('programs/rp_donor_costs.cpp')
source('programs/calculate_rp_indices.R')
set.seed(20261006)
for(case in 1:102) {
  n <- 6L; side <- c(rep('I',3),rep('G',3))
  e <- matrix(sample(500:1600,n*n,replace=TRUE),n,n);diag(e) <- 1000
  if(case==101)e[,] <- 1000
  if(case==102){e[,] <- 1500;diag(e)<-1000;e[1,2]<-900;e[2,1]<-900;e[1,4]<-1000;e[4,1]<-1000}
  EX <- list(E=e,n=n);REL <- rp_relations(EX)
  hm <- n
  for(k in 0:n) {
    removed <- if(k==0)list(integer(0)) else combn(n,k,simplify=FALSE)
    if(any(vapply(removed,function(r)!rp_has_cross(REL,side,setdiff(1:n,r)),logical(1)))){hm<-k;break}
  }
  best <- 0
  visit <- function(path) {
    u <- tail(path,1);start <- path[1]
    for(v in which(REL$W[u,])) {
      if(v==start && length(path)>1 && length(unique(side[path]))==2)best<<-max(best,rp_mpi(EX,path))
      else if(v>start && !v%in%path)visit(c(path,v))
    }
  }
  for(start in 1:n)visit(start)
  native <- rp_donor_mpi(e,side)
  stopifnot(rp_donor_hm(e,side)==hm,native$exhausted,abs(native$value-best)<1e-10)
}
cat('SUCCESS: HM deletion counts and MaxMPI cross-cycle values match exhaustive enumeration for 102 small graphs, including exact ties and strict internal cycles.\n')
