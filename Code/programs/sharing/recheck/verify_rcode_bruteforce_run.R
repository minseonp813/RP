## Randomised differential test of the R library, part 2: run the package's own routines on the
## generated cases and compare with the brute-force reference computed in Python.
## Run from rep_code/recheck:  Rscript verify_rcode_bruteforce_run.R
SP <- getwd(); setwd("..")
if (dir.exists(".R-library")) .libPaths(c(".R-library", .libPaths()))
suppressPackageStartupMessages(library(jsonlite))
source("programs/rp_cross_costs.R")

cases <- fromJSON(file.path(SP, Sys.getenv("CASES", "difftest_cases.json")),
                  simplifyDataFrame = FALSE)

bad <- 0; nz <- 0; notexh <- 0; branched <- 0; worst <- c(ccei = 0, hm = 0, mpi = 0)
for (cs in cases) {
  d <- data.frame(coord_x = unlist(cs$cx), coord_y = unlist(cs$cy),
                  intercept_x = unlist(cs$ix), intercept_y = unlist(cs$iy))
  side <- unlist(cs$side)
  EX <- rp_expenditure(d)
  got_ccei <- rp_cost_ccei(EX, side)
  got_hm   <- rp_cost_hm(EX, side)
  rmp      <- rp_cost_mpi(EX, side)
  if (!isTRUE(rmp$exhausted)) notexh <- notexh + 1L
  if (!isTRUE(rmp$certified)) branched <- branched + 1L
  want <- c(ccei = as.numeric(cs$ccei_float), hm = as.numeric(cs$hm),
            mpi = as.numeric(cs$mpi_float))
  got  <- c(ccei = got_ccei, hm = got_hm, mpi = rmp$value)
  dif  <- abs(got - want)
  worst <- pmax(worst, dif)
  if (want["hm"] > 0) nz <- nz + 1L
  if (any(dif > 1e-9)) {
    bad <- bad + 1L
    cat(sprintf("MISMATCH case %d (n=%d, %s, sides=%s)\n", cs$id, cs$n, cs$mode,
                paste(side, collapse = "")))
    cat(sprintf("   R    ccei=%.12f hm=%d mpi=%.12f\n", got["ccei"], as.integer(got["hm"]), got["mpi"]))
    cat(sprintf("   ref  ccei=%.12f hm=%d mpi=%.12f\n", want["ccei"], as.integer(want["hm"]), want["mpi"]))
    cat(sprintf("   ix=%s\n   iy=%s\n   cx=%s\n   cy=%s\n",
                paste(unlist(cs$ix), collapse=","), paste(unlist(cs$iy), collapse=","),
                paste(unlist(cs$cx), collapse=","), paste(unlist(cs$cy), collapse=",")))
  }
}
cat(sprintf("\n%d cases (%d with a cross violation); mismatches = %d; non-exhaustive = %d\n",
            length(cases), nz, bad, notexh))
cat(sprintf("branch-and-bound actually invoked (certificate failed) in %d cases\n", branched))
cat(sprintf("max |R - bruteforce|:  ccei %.3g   hm %.3g   mpi %.3g\n",
            worst["ccei"], worst["hm"], worst["mpi"]))
