Section 6 collective-quality pipeline

Run the relevant sections from Code. Estimation is in 06/08; plotting is in
07/09. The former standalone 11/12 scripts have been removed.

1. Main collective estimates: 06_Tables_Main.do, section headed
   "Collective outcomes: Figure 8, multinomial A10 estimates, covariate text and A12."

   programs/build_collective_analysis_sample.do imports
   ../TalkFile_ceiv_fresh_3dp.csv, preserves its endpoint-attainment flags, and
   builds data/panel_group_new_indices.dta and data/collective_analysis_sample.dta.
   The group CEIV panel also supplies Table 1. The same helper is called before
   Table 1 in 06 and by both collective sections in 08.

   Outputs here: joint_outcome_linear_ame.csv for Figure 8 and
   joint_outcome_ame.csv for A10. Extensions outputs: covariate_ame.csv,
   joint_outcome_shapley_r2.csv, joint_outcome_shapley_models.csv and
   joint_outcome_shapley_coalitions.csv. Covariate effects use the full-control
   multinomial fit directly while its estimation sample is in memory.
   Log: Logs/06_collective_outcomes.log.

   Figure 8 uses Class FE, Class FE + controls and Pair FE + controls, with
   class-clustered t(63) intervals. A10 uses the first two specifications in
   multinomial logit, with class-clustered normal intervals. Figure A12 decomposes
   the overall R-squared of eight controlled LPMs: four Class FE models and four
   Pair FE models. Its five blocks include both individual CCEIs, student
   characteristics, friendship, corner/midpoint shares and fixed effects. All
   32 coalitions use the same 1,304 observations; exact contributions sum to
   each full model's overall R-squared and percentage shares sum to 100.

2. Main figures: 07_Figures_Main.R, sections for Figures 6--8.

   These read the collective sample and AME exports produced by 06. Figure 8
   uses figure8_lpm_ame_*.png. Figures are written to
   results/figures/collective_rationality and copied to the existing Overleaf
   figure paths. Counts, category estimates and mean contrasts are written here.
   To run Figure 7 alone, load haven/ggplot2/dplyr/grid, define paper_theme and
   sig_mark as above, and set code_dir to Code. Its class-FE helper is sourced
   by the section. Figure 8 defines its own paths and sources the AME plot helper.

3. Appendix estimation: 08_Tables_Appendix.do.

   The "Collective-outcomes appendix table" section prepares the sample,
   estimates its six categorical regressions, exports table5_coefficients.csv
   and table5_diagnostics.csv, and writes
   results/tables/collective_ccei_ceiv_categories.tex and its Overleaf copy.
   Low--High is omitted; Class FE, Class FE + controls and Pair FE + controls
   correspond to Columns (1)--(3). Log: Logs/08_collective_categories.log.

   The "Figure A11 estimates" section prepares its own sample and estimates
   the same three LPM specifications with HM and RevMaxMPI. It exports
   extensions/alternative_linear_ame.csv.
   Log: Logs/08_collective_alternatives.log.
   Both sections are independently runnable from Code and use the shared
   collective sample/control and LPM helpers.

4. Appendix plots: 09_Figures_Appendix.R, independently runnable A10--A12 sections.

   A10 reads joint_outcome_ame.csv from 06 and writes figure6_a_ame_*.png.
   A11 reads alternative_linear_ame.csv from 08, computes the member SDs used
   in the draft notes, exports extensions/alternative_sd_units.csv, and writes
   alternative_{HM,MaxMPI}_lpm_ame_{maximum,minimum}.png.
   A12 reads the decomposition/full-model exports from 06 and writes
   joint_outcome_shapley.pdf and its PNG preview. Its checks compare the full
   models' CCEI coefficients and standard errors with Figure 8.
   All panels go to results/figures/collective_rationality and their existing
   Overleaf paths. Shared styling remains in programs/plot_collective_ame.R.
   Figure 8/A10/A11 effects are percentage points per one-SD increase; stored
   effects remain per 0.1 units. CCEI SDs are 0.058234780484 (higher) and
   0.151319396465 (lower), computed from the pooled 1,304 pair-wave sample.

Historical table5_*.ster files, joint_outcome_model*.ster, alternative_ame.csv,
HM/MaxMPI.ster and the old likelihood-based shapley.csv are retained results;
the integrated generators do not use or refresh them. Former covariate review
plots are not generated.

The 2026-10-11 draft-only cleanup was checked in isolated runs. All eight
Figure 8/A10/A11 PNG panels and the categorical appendix table reproduced
byte-for-byte. Retained table, LPM, Shapley and SD exports match; multinomial
CCEI AME exports differ by at most 8.3e-11. All 120 covariate point estimates
and scales match exactly. Fresh pre-cleanup and cleaned runs show slight
variation in covariate standard errors, with identical model coefficients
and unchanged draft signs/significance claims. Inference code was unchanged.

The 2026-10-11 integration into 06--09 was checked against the current standalone
scripts in isolated copies. All nine Figure 8/A10--A12 PNGs and both the Table 1
and categorical appendix LaTeX tables reproduced byte-for-byte. The A12 PDF
differs only in creation/modification dates. All categorical, LPM, multinomial
CCEI AME, R2-decomposition and SD exports match; all 120 covariate point estimates
and scales match. Covariate standard errors retain numerical variation in
the multinomial fit: one unused outcome-2 openness effect crosses the 5% cutoff
(p=0.0551 versus 0.0499), with no change to the draft's claims. The 158 analysis
columns, alternative-index extrema and complete group CEIV panel match. Each
A10--A12 plotting section also passed in a fresh R session. Validation wrote
only to temporary project copies, including temporary Overleaf output folders.

Current sample: 1,304 pair-waves, 652 pairs, 64 classroom clusters.
CEIV endpoint status uses ceiv_at1 from the supplied attainment flag, since two
nonattained observations round to 1.000. Individual CCEI categories use the
pooled median across both waves (0.9806594): 348 Low-Low, 608 Low-High, 348 High-High.
Figures 3 and 7 include class FE without additional controls. Predicted means
use the same sample-weighted class composition for every category, and equal
the category averages of the class-adjusted outcomes used in the CDFs. Both
figures use normal mean +/- 1.96 SD/sqrt(n) intervals. Contrasts use
class-clustered CR1 and t(63). CDFs remove estimated class effects and restore
their weighted average. KS p-values use ordinary asymptotic two-sample tests,
treating adjusted values as observed. No bootstrapping is used in either figure.
Adjusted Figure 7 values may exceed one. Draft figure notes await revision.
ceiv_refresh_comparison.csv is a retained diagnostic from the CEIV refresh.

The former results/new_indices folder was retired on 2026-10-11. Historical
review packets, exploratory results, and their original documentation are in
../../../Archive/new_indices_cleanup_2026-10-11/Code/results/new_indices.
The archive's migration_manifest.tsv records every original file's destination
and SHA-256 before path updates. Overleaf figure/table filenames were preserved.
