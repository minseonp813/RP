setwd('/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code')
lines <- readLines('1_1_calculate_indices.R')
start <- grep('^# 7[.] Non-own donor benchmarks M$',lines)
stopifnot(length(start)==1L)
expressions <- parse(text=lines[start:length(lines)])
for(expr in expressions) {
 eval(expr,.GlobalEnv)
 if(is.call(expr) && identical(expr[[1L]],as.name('source')) &&
    identical(as.character(expr[[2L]]),'programs/build_placebo_donor_matrices.R')) {
  original_exact <- rp_donor_mpi_exact
  rp_donor_mpi_exact <- function(...) withCallingHandlers(original_exact(...),error=function(e) {
   frames <- sys.frames()
   candidates <- which(vapply(frames,function(f)exists('fit',f,inherits=FALSE) && exists('E',f,inherits=FALSE),logical(1)))
   if(length(candidates)) {
    f <- frames[[tail(candidates,1)]]
    saveRDS(list(E=f$E,side=f$side,lower=f$lower,best=f$best,value=f$value,
                 fit=f$fit,edges=f$edges,ns=f$ns),paste0('/private/tmp/rp_mpi_error_',Sys.getpid(),'.rds'))
   }
  })
  message('Numerical diagnostic handler installed.')
 }
}
message('SUCCESS: full selected benchmarks and wide-panel mapping complete.')
