## =====================================================================================
##  20_table1_figure2_ccei.R
##
##  Table 1 and Figure 2 of the paper, with the CCEI as the inconsistency measure.
##  This is the BASELINE specification: it reproduces the published numbers, and the two
##  companion scripts repeat the exercise with the other two measures of Appendix C.
##
##      20_table1_figure2_ccei.R     CCEI                (this file)
##      21_table1_figure2_maxmpi.R   maximum money pump  (appendix, def:app_mp)
##      22_table1_figure2_hm.R       Houtman-Maks count  (appendix, def:app_hm)
##
##  Expected output (so a checker knows what "reproduces" means): Table 1 of the paper row by
##  row; Figure 2 with lower 0.668, higher 0.332, gap 0.335, n = 2,560 student-waves.
##
##  ---------------------------------------------------------------------------------
##  WHAT IT COMPUTES
##  ---------------------------------------------------------------------------------
##  For every pair g and wave, and for S = {mover}, {non-mover}, N = {both}, the merged
##  dataset D^{Sg} joins the individual choices of the members in S (rounds 1-18) with the
##  pair's collective choices (rounds 19-36).  The cross-consistency cost is
##
##      c_Sg = 1 - e^x_Sg ,   e^x_Sg = sup{ e : D^{Sg} admits no cross e-violation }
##
##  (paper, def:cost), and the revealed-preference distance of the mover is
##
##      I_ig = [ (1/2) c_ig + (1/2) ( c_Ng - c_jg ) ] / c_Ng                (paper, def:index),
##
##  with I_jg = 1 - I_ig.  The index is undefined when c_Ng = 0, i.e. when the pair's
##  choices reveal no disagreement with either member; those pair-waves drop out of the
##  distance rows of Table 1 and out of Figure 2, exactly as in the paper.
##
##  e^x is obtained EXACTLY: the revealed-preference relations change only at the finitely
##  many expenditure ratios, so a binary search over that grid returns the supremum with no
##  bisection tolerance.  All comparisons are integer comparisons (see programs/rp_cross_costs.R).
##
##  ---------------------------------------------------------------------------------
##  INPUT  data/base_raw.dta, data/end_raw.dta   (round-level choices; see 01_calculate_ccei.R)
##  OUTPUT out_measures/ccei_pairwave.csv        one row per pair-wave: costs, index, scores
##         out_measures/ccei_table1.csv/.tex     Table 1 rows
##         out_measures/ccei_figure2_bar.png     Figure 2, panel (a)
##         out_measures/ccei_figure2_cdf.png     Figure 2, panel (b)
##         out_measures/ccei_figure2_stats.csv   the numbers quoted in the notes to Figure 2
##
##  RUNTIME about one minute for the 1,304 pair-waves.
## =====================================================================================

rm(list = ls())

## ---- 0. Setup ----------------------------------------------------------------------
## The bundled library is used first so that the script runs with the package's own
## package versions; RStudio users get the working directory set automatically.
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  ctx <- rstudioapi::getSourceEditorContext()$path
  if (!is.null(ctx) && nzchar(ctx)) setwd(dirname(ctx))
}
## The bundled library (macOS arm64, R 4.6) is put first AFTER the working directory is set;
## on other platforms delete .R-library and install haven, ggplot2, igraph yourself.
if (dir.exists(".R-library")) .libPaths(c(".R-library", .libPaths()))
suppressPackageStartupMessages({ library(haven); library(ggplot2) })
source("programs/rp_cross_costs.R")

MEASURE <- "ccei"
LABEL   <- "CCEI"
## TRUE because a LARGER CCEI means a more rational subject.  In the two companion scripts
## this is FALSE: a larger money pump, or a larger Houtman-Maks count, means less rational.
## The flag is used only by rp_higher(): it decides which member is called more rational (the
## Figure 2 split and the high_mover column); costs, the index and Table 1 do not depend on it.
HIGHER_IS_BETTER <- TRUE

OUTDIR <- "out_measures"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

## ---- 1. Data -----------------------------------------------------------------------
base  <- rp_load_wave("data/base_raw.dta")
end   <- rp_load_wave("data/end_raw.dta")
pairs <- rp_pair_table(base, end)
message(sprintf("pairs in both waves after dropping the %d excluded groups: %d",
                length(RP_DROP_GROUPS), nrow(pairs)))

## ---- 2. Costs, index, and the more-rational member ---------------------------------
## rp_compute_measure() returns the three cross costs of every pair-wave together with the
## rationality score of each member, of the group, and of each member's individual-plus-group
## dataset (the last is used only to break ties in the ranking below).
df  <- rp_compute_measure(MEASURE, base, end, pairs)
df  <- rp_add_index(df, higher_is_better = HIGHER_IS_BETTER)
stu <- rp_student_long(df)            # one row per student-wave: the unit of Table 1 / Figure 2
## Columns of df: group_id, wave, id_mover, id_nonmover; c_mover, c_nonmover, c_N (cross costs of
## D^{ig}, D^{jg}, D^{Ng}); certified, exhausted (money-pump search flags, NA for this measure);
## score_1, score_2, score_g, score_1g, score_2g (measure of each member, the group, and each
## member merged with the group -- the last two only break ties); ra_1, ra_2, ra_g (risk
## aversion); I_mover (the index of the mover; the non-mover's is 1 - I_mover); high_mover.
## I_mover is NA in the 8 baseline and 16 endline pair-waves with c_N = 0 (no cross violation),
## which is why the distance rows of Table 1 have N = 2 x 644 = 1,288 and 2 x 636 = 1,272.
write.csv(df, file.path(OUTDIR, paste0(MEASURE, "_pairwave.csv")), row.names = FALSE)

## A note on ties.  Individual CCEIs are equal in about 15 per cent of the pair-waves, all of
## them at CCEI = 1.  rp_higher() follows 01_calculate_ccei.R: the mover is called the more
## rational member unless the non-mover is strictly more rational, ties are broken by the score
## of the individual-plus-group dataset, and any remaining tie goes to the mover.  The line
## below reports how much this matters, because the convention is not innocuous.
tie  <- df$score_1 == df$score_2
strict_only <- stu[!is.na(stu$I) &
                   !(paste(stu$group_id, stu$wave) %in% paste(df$group_id[tie], df$wave[tie])), ]
message(sprintf("individual-score ties: %d of %d pair-waves (%.1f%%)",
                sum(tie), nrow(df), 100 * mean(tie)))
message(sprintf("Figure 2 gap: %.4f overall, %.4f on strictly ranked pairs only",
                mean(stu$I[stu$higher == 0], na.rm = TRUE) - mean(stu$I[stu$higher == 1], na.rm = TRUE),
                mean(strict_only$I[strict_only$higher == 0]) - mean(strict_only$I[strict_only$higher == 1])))

## ---- 3. Table 1 --------------------------------------------------------------------
t1 <- rp_table1(df, stu, LABEL)
print(t1, digits = 3)
write.csv(t1, file.path(OUTDIR, paste0(MEASURE, "_table1.csv")), row.names = FALSE)

## LaTeX body, ready to \input into the Table 1 tabular of the paper.
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
## Panel (a): mean distance of the less- and the more-rational member, waves pooled, with
## 95 per cent confidence intervals.  Panel (b): the two empirical CDFs.
fig <- rp_figure2(stu, LABEL,
                  file.path(OUTDIR, paste0(MEASURE, "_figure2_bar.png")),
                  file.path(OUTDIR, paste0(MEASURE, "_figure2_cdf.png")))
print(fig, digits = 4)
write.csv(fig, file.path(OUTDIR, paste0(MEASURE, "_figure2_stats.csv")), row.names = FALSE)

message("done: ", normalizePath(OUTDIR))
