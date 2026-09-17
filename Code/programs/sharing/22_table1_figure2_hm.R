## =====================================================================================
##  22_table1_figure2_hm.R
##
##  Table 1 and Figure 2 with the HOUTMAN-MAKS index in place of the CCEI, i.e. the second
##  robustness exercise of Appendix C.  Independent of the other two scripts.
##
##  ---------------------------------------------------------------------------------
##  WHAT CHANGES RELATIVE TO 20_table1_figure2_ccei.R
##  ---------------------------------------------------------------------------------
##  Only the cost.  In the spirit of Houtman and Maks (1985), the merged dataset is priced by
##  the smallest number of observations whose removal leaves no cross violation (appendix, def:app_hm):
##
##      c^HM_Sg = min { |R| : R subset of D^{Sg},  D^{Sg} \ R has no cross violation } .
##
##  The index I_ig and everything downstream are unchanged.
##
##  THREE THINGS TO KNOW, all of which the appendix discusses.
##
##  (a) NO NORMALIZATION IS NEEDED, AND NONE SHOULD BE APPLIED.  The cost depends on the data
##      only through the revealed preference relations, which do not change when each
##      observation's prices are rescaled; unlike the money pump, it is a function of the
##      observed budget sets and chosen bundles alone.  But it must be left as a COUNT.  The
##      customary Houtman-Maks convention of reporting the FRACTION of discarded observations
##      would divide by |D^{Sg}|, a denominator that is 36 for one member and 54 for two, and
##      that destroys the monotonicity the Shapley construction needs: a member none of whose
##      observations lies in the support of any cross violation of D^{Ng}, while the other
##      member's do, would then receive a negative share (appendix, prop:app_failures(ii); its
##      footnote shows that c^HM_ig = 0 alone is not enough).  Do not divide.
##
##  (b) THE INDEX IS COARSE.  Because the three costs are integers, the Shapley share is a
##      half-integer and I_ig lives on the grid { k / (2 c^HM_Ng) }.  In pairs with c^HM_Ng = 1
##      the only possible values are 0, 1/2 and 1.  This is why the CDF in panel (b) of
##      Figure 2 is visibly stepped and why the standard deviation reported in Table 1 is
##      smaller than for the other two measures.  It is a property of the measure, not a bug.
##
##  (c) IT IS EXACT BUT COMBINATORIAL.  Deleting an observation destroys exactly the violations
##      whose support contains it, so c^HM_Sg is a minimum hitting set over the supports of the
##      cross violations.  rp_cost_hm() solves it by constraint generation: seed with every
##      two-observation CROSS violation, solve the hitting-set problem by branch and bound, look for a
##      surviving cross violation, add its support as a new constraint, and repeat.  This
##      terminates at the exact optimum; no external solver is required.
##
##  ---------------------------------------------------------------------------------
##  INPUT  data/base_raw.dta, data/end_raw.dta
##  OUTPUT out_measures/hm_pairwave.csv, hm_table1.csv/.tex,
##         hm_figure2_bar.png, hm_figure2_cdf.png, hm_figure2_stats.csv
##
##  RUNTIME the slowest of the three scripts: about 3-4 minutes for the 1,304 pair-waves on an
##  Apple-silicon laptop (R 4.6), dominated by the pairs with the largest costs; the elapsed
##  time is printed as it runs.  Run from the package folder (see 20_...R).
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

MEASURE <- "hm"
LABEL   <- "Houtman-Maks"
## FALSE: a SMALLER Houtman-Maks count (fewer choices to discard) means a more rational subject.
## Used only by rp_higher() for the Lower/Higher split and high_mover; costs, index and Table 1
## do not depend on it.
HIGHER_IS_BETTER <- FALSE

OUTDIR <- "out_measures"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

## ---- 1. Data -----------------------------------------------------------------------
base  <- rp_load_wave("data/base_raw.dta")
end   <- rp_load_wave("data/end_raw.dta")
pairs <- rp_pair_table(base, end)
message(sprintf("pairs in both waves after dropping the %d excluded groups: %d",
                length(RP_DROP_GROUPS), nrow(pairs)))

## ---- 2. Costs, index, and the more-rational member ---------------------------------
## Columns of df are documented in 20_table1_figure2_ccei.R (certified/exhausted are NA here).
df  <- rp_compute_measure(MEASURE, base, end, pairs, progress = 25L)
df  <- rp_add_index(df, higher_is_better = HIGHER_IS_BETTER)
stu <- rp_student_long(df)
write.csv(df, file.path(OUTDIR, paste0(MEASURE, "_pairwave.csv")), row.names = FALSE)

## Ties.  A member whose own choices satisfy GARP has a Houtman-Maks count of zero, so ties are
## common (343 of 1,304 pair-waves, 26 per cent, against 199 for the CCEI; 55 of them at equal
## positive counts); they are broken exactly as in 20_table1_figure2_ccei.R.  Both keys are
## integers, so more residual ties fall to the mover here than under the other measures.
tie <- df$score_1 == df$score_2
strict_only <- stu[!is.na(stu$I) &
                   !(paste(stu$group_id, stu$wave) %in% paste(df$group_id[tie], df$wave[tie])), ]
message(sprintf("individual-score ties: %d of %d pair-waves (%.1f%%)",
                sum(tie), nrow(df), 100 * mean(tie)))
message(sprintf("Figure 2 gap: %.4f overall, %.4f on strictly ranked pairs only",
                mean(stu$I[stu$higher == 0], na.rm = TRUE) - mean(stu$I[stu$higher == 1], na.rm = TRUE),
                mean(strict_only$I[strict_only$higher == 0]) - mean(strict_only$I[strict_only$higher == 1])))

## Checks required by the appendix's prop:app_general: monotone costs, index in [0,1].
stopifnot(all(df$c_mover <= df$c_N), all(df$c_nonmover <= df$c_N))
stopifnot(all(is.na(df$I_mover) | (df$I_mover >= 0 & df$I_mover <= 1)))
## And the grid property of point (b) above: 2 * c_Ng * I_ig is a whole number.
stopifnot(all(is.na(df$I_mover) | abs(2 * df$c_N * df$I_mover - round(2 * df$c_N * df$I_mover)) < 1e-9))

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
  ## The count rows (individual, group, cross cost) are printed with two decimals; the distance
  ## row is a fraction on the grid k/(2 c_Ng) and keeps three, as in the other two scripts.
  dec <- ifelse(grepl("^(Individual|Group) Houtman|^Cross cost", b$measure), 2L, 3L)
  fmt <- function(x) mapply(function(v, d) formatC(v, format = "f", digits = d), x, dec)
  tex <- c(tex, sprintf("\\hspace{1em}%s & %s & %s & %s & %s & %s & %s \\\\",
                        b$measure, fmt(b$mean), fmt(b$sd), fmt(b$p10), fmt(b$p50), fmt(b$p90),
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
