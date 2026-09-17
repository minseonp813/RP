## =====================================================================================
##  21_table1_figure2_maxmpi.R
##
##  Table 1 and Figure 2 with the MAXIMUM MONEY PUMP INDEX in place of the CCEI, i.e. the
##  robustness exercise of Appendix C.  Run 20_table1_figure2_ccei.R first if you want the
##  baseline numbers to compare against; the two scripts are independent otherwise.
##
##  ---------------------------------------------------------------------------------
##  WHAT CHANGES RELATIVE TO 20_table1_figure2_ccei.R
##  ---------------------------------------------------------------------------------
##  Only the cost attached to the cross violations of the merged dataset.  Following
##  Echenique, Lee and Shum (2011), a violation V = (k_1, ..., k_L) is priced by
##
##      MPI(V) = sum_a ( p^{k_a}.x^{k_a} - p^{k_a}.x^{k_{a+1}} ) / sum_a p^{k_a}.x^{k_a} ,
##
##  the share of the expenditure along the cycle that an arbitrageur can pump out, and the
##  cost of D^{Sg} is the LARGEST such value over the cross violations with pairwise distinct
##  observations (appendix, def:app_mp):
##
##      c^MP_Sg = max { MPI(V) : V a cross violation of D^{Sg}, observations distinct } .
##
##  The index I_ig, the Shapley weights, and everything downstream are unchanged.
##
##  THREE THINGS TO KNOW, all of which the appendix discusses.
##
##  (a) THE MAXIMUM IS ESSENTIAL.  The minimum, the mean and the median money pump over the
##      cross violations do NOT give a monotone cost game: adding a member's choices can add a
##      cross violation with a smaller money pump and pull the average down, at which point a
##      member's Shapley share turns negative and the index leaves [0,1].  In the paper's own
##      Appendix C example the minimum gives I_ig = -4/13 and the mean gives -2/17.
##
##  (b) THE INCOME NORMALIZATION IS PART OF THE DEFINITION.  MPI(V) is an expenditure-weighted
##      average of the per-step pump shares.  The shares do not depend on how each observation's
##      prices are scaled, but the weights do, so - unlike the CCEI and the Houtman-Maks count -
##      the money pump is defined only once incomes are normalized (appendix,
##      prop:app_invariance).  The paper sets p.x = 1, under which MPI is the plain average of
##      the per-step shares, and rp_mpi() computes exactly that.  Note that "p = 1/intercept"
##      would NOT do: the recorded choices are integer KRW, so with those prices p.x is exactly 1
##      for only about 30 per cent of observations (the rest within 0.2 per cent), the weights
##      would be unequal, and MPI would differ from the paper's definition in the fourth decimal.
##      The script prints that share after loading the data so the claim can be checked.
##
##  (c) VIOLATIONS MUST NOT REPEAT AN OBSERVATION.  Read literally, the paper's def:eviol allows
##      a violation to revisit an observation; the supremum of MPI over such violations then
##      equals the value of a cycle lying entirely on one side and is not attained by any cross
##      violation (appendix example: 17/96 against the 7/48 of def:app_mp), which is exactly what
##      the cross restriction is meant to exclude.  rp_cost_mpi() uses cycles with distinct
##      observations only.
##
##  HOW IT IS COMPUTED (programs/rp_cross_costs.R, section 4).  A cycle's money pump is a ratio
##  of sums, so the LARGEST money pump over all cycles of a component is a maximum-ratio-cycle
##  problem, solved in polynomial time by Dinkelbach iteration with Bellman-Ford.  Inside each
##  strongly connected component that meets both sides we take that unconstrained maximum first.
##  If the maximizing cycle happens to be cross, it is optimal and we are done; this certificate
##  fires for about a quarter of the pair-waves when all three merged datasets are counted.
##
##  Otherwise the maximum has to be taken over cross cycles only, which is not a polynomial
##  problem, and we branch.  The unconstrained maximum bounds every cycle of a subproblem from
##  above, so a subproblem already below the incumbent is dropped.  If its maximizing cycle C is
##  not cross, every cross cycle there omits one of C's edges (a simple cycle containing all
##  edges of another equals it), and we split on the first omitted edge in Lawler's fashion:
##  child i bans e_i and forces e_1, ..., e_{i-1}.  Every cross cycle survives in exactly the
##  child indexed by its first omitted edge, so the split is exhaustive; the forcing is what keeps
##  the tree small.  The search is exhaustive, so the reported value is exact; the columns
##  `certified` and `exhausted` in the saved csv record this per pair-wave: `certified` is TRUE
##  only when ALL THREE merged datasets D^{ig}, D^{jg}, D^{Ng} were settled without branching
##  (26.6 per cent of pair-waves), `exhausted` must be TRUE everywhere (the script stops if not).
##
##  ---------------------------------------------------------------------------------
##  INPUT  data/base_raw.dta, data/end_raw.dta
##  OUTPUT out_measures/maxmpi_pairwave.csv, maxmpi_table1.csv/.tex,
##         maxmpi_figure2_bar.png, maxmpi_figure2_cdf.png, maxmpi_figure2_stats.csv
##
##  RUNTIME about 1.5 minutes for the 1,304 pair-waves on an Apple-silicon laptop (R 4.6); the
##  elapsed time is printed as it runs.  Run from the package folder (see 20_...R).
## =====================================================================================

rm(list = ls())

## ---- 0. Setup ----------------------------------------------------------------------
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  ctx <- rstudioapi::getSourceEditorContext()$path
  if (!is.null(ctx) && nzchar(ctx)) setwd(dirname(ctx))
}
if (dir.exists(".R-library")) .libPaths(c(".R-library", .libPaths()))   # after setwd, see 20_
suppressPackageStartupMessages({ library(haven); library(ggplot2) })
source("programs/rp_cross_costs.R")

MEASURE <- "maxmpi"
LABEL   <- "MaxMPI"
## FALSE: a SMALLER maximum money pump means a more rational subject, the opposite of the CCEI.
## Used only by rp_higher() for the Lower/Higher split and high_mover; costs, index and Table 1
## do not depend on it.  (The paper's earlier appendix tables report 1 - MaxMPI, "RevMaxMPI";
## the ranking, hence high_mover, c_Ng, I_ig and Figure 2, is identical either way.  The
## Individual/Group MaxMPI rows of Table 1 are reported here as MaxMPI itself.)
HIGHER_IS_BETTER <- FALSE

OUTDIR <- "out_measures"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

## ---- 1. Data -----------------------------------------------------------------------
base  <- rp_load_wave("data/base_raw.dta")
end   <- rp_load_wave("data/end_raw.dta")
pairs <- rp_pair_table(base, end)
message(sprintf("pairs in both waves after dropping the %d excluded groups: %d",
                length(RP_DROP_GROUPS), nrow(pairs)))
## Documents point (b) above: with p = 1/intercept, how often would p.x be exactly 1?
px <- with(rbind(base, end), as.numeric(coord_x) / as.numeric(intercept_x) +
                              as.numeric(coord_y) / as.numeric(intercept_y))
message(sprintf("share of observations with coord/intercept expenditure exactly 1: %.3f  (max |dev| %.4f)",
                mean(px == 1), max(abs(px - 1))))

## ---- 2. Costs, index, and the more-rational member ---------------------------------
## Columns of df are documented in 20_table1_figure2_ccei.R; here certified/exhausted are live.
df  <- rp_compute_measure(MEASURE, base, end, pairs)
df  <- rp_add_index(df, higher_is_better = HIGHER_IS_BETTER)
stu <- rp_student_long(df)
write.csv(df, file.path(OUTDIR, paste0(MEASURE, "_pairwave.csv")), row.names = FALSE)

## Ties again.  Members whose own 18 choices satisfy GARP all have a maximum money pump of
## exactly zero, so ties are as common here as they are for the CCEI and are broken the same way.
tie <- df$score_1 == df$score_2
strict_only <- stu[!is.na(stu$I) &
                   !(paste(stu$group_id, stu$wave) %in% paste(df$group_id[tie], df$wave[tie])), ]
message(sprintf("individual-score ties: %d of %d pair-waves (%.1f%%)",
                sum(tie), nrow(df), 100 * mean(tie)))
message(sprintf("Figure 2 gap: %.4f overall, %.4f on strictly ranked pairs only",
                mean(stu$I[stu$higher == 0], na.rm = TRUE) - mean(stu$I[stu$higher == 1], na.rm = TRUE),
                mean(strict_only$I[strict_only$higher == 0]) - mean(strict_only$I[strict_only$higher == 1])))

## Sanity checks that the appendix's prop:app_general requires and that should never fail.
stopifnot(all(df$c_mover <= df$c_N + 1e-9), all(df$c_nonmover <= df$c_N + 1e-9))   # monotone
stopifnot(all(is.na(df$I_mover) | (df$I_mover >= -1e-9 & df$I_mover <= 1 + 1e-9))) # index in [0,1]
## The branch and bound must have CLOSED on every pair-wave.  If this fails the reported maxima
## are only lower bounds: raise the `budget` argument of rp_max_cross_cycle and re-run.
stopifnot(all(df$exhausted))

## ---- 3. Table 1 --------------------------------------------------------------------
t1 <- rp_table1(df, stu, LABEL)
print(t1, digits = 3)
write.csv(t1, file.path(OUTDIR, paste0(MEASURE, "_table1.csv")), row.names = FALSE)

tex <- c("& (1) & (2) & (3) & (4) & (5) & (6) \\\\",
         "& Mean & SD & p10 & p50 & p90 & N \\\\ \\midrule")
for (w in c("base", "end")) {
  tex <- c(tex, sprintf("\\multicolumn{7}{l}{\\emph{Panel %s}: %s} \\\\",
                        if (w == "base") "A" else "B", if (w == "base") "Baseline" else "Endline"))
  b <- t1[t1$wave == w, ]
  tex <- c(tex, sprintf("\\hspace{1em}%s & %.3f & %.3f & %.3f & %.3f & %.3f & %s \\\\",
                        b$measure, b$mean, b$sd, b$p10, b$p50, b$p90,
                        formatC(b$N, big.mark = ",", format = "d")))
}
writeLines(tex, file.path(OUTDIR, paste0(MEASURE, "_table1.tex")))

## ---- 4. Figure 2 -------------------------------------------------------------------
fig <- rp_figure2(stu, LABEL,
                  file.path(OUTDIR, paste0(MEASURE, "_figure2_bar.png")),
                  file.path(OUTDIR, paste0(MEASURE, "_figure2_cdf.png")))
print(fig, digits = 4)
write.csv(fig, file.path(OUTDIR, paste0(MEASURE, "_figure2_stats.csv")), row.names = FALSE)

message("done: ", normalizePath(OUTDIR))
