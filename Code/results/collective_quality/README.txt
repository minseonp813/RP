Section 6 collective-quality pipeline

Run the analysis scripts from Code.

1. Refresh the main estimates:
   /Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 11_collective_quality.do

   Input: ../TalkFile_ceiv_fresh_3dp.csv and the existing panel data.
   Analysis sample: data/collective_analysis_sample.dta.
   Outputs here: table5_coefficients.csv and table5_diagnostics.csv from the
   six categorical appendix-table regressions, joint_outcome_model.ster
   (full controls, used for the draft text), and joint_outcome_ame.csv
   for the multinomial-logit appendix figure. The script also runs
   11_2_collective_quality_linear.do, producing joint_outcome_linear_ame.csv
   for main Figure 8. Its three specifications use Class FE, Class FE + controls,
   and Pair FE + controls, in that order from top to bottom. Triangles, hollow
   circles, and squares distinguish them. All use the same joint CCEI regressors,
   sample, and class-clustered inference; LPM intervals use t(63). The appendix
   multinomial-logit figure retains the first two specifications and uses normal
   intervals.
   Logs: Logs/11_collective_quality.log and Logs/11_2_collective_quality_linear.log.

2. Refresh Figures 6-8 using the final three sections of 07_Figures_Main.R.
   For those sections alone, set code_dir to Code, load haven/ggplot2/dplyr/grid,
   and load the paper_theme and sig_mark definitions above. The Figure 7 section
   loads programs/class_fe_category_summary.R itself.
   Figures go to results/figures/collective_rationality and are copied to
   ../Overleaf/figures_2025/collective_rationality. Counts, category estimates,
   and pairwise mean contrasts are written here.
   The Figure 8 section calls 11_3_collective_quality_plots.R, which can also
   run independently from Code. The shared programs/plot_collective_ame.R
   matches the existing draft style. Main images use figure8_lpm_ame_*; the
   multinomial-logit appendix images retain figure6_a_ame_*. Both are copied
   to the Overleaf figure folder. Figure 8 and Appendix Figure A10 use one-SD
   increases: 0.058234780484 for the higher CCEI and 0.151319396465 for the lower
   CCEI. SDs are computed from the pooled analysis sample at plotting time;
   the stored effects and intervals remain per 0.1 units.

3. Export the collective-outcomes appendix table using its section in
   08_Tables_Appendix.do (headed "Collective-outcomes appendix table").
   It can run independently from Code after step 1, using the coefficient and
   diagnostic CSVs. It writes results/tables/collective_ccei_ceiv_categories.tex
   and copies it to ../Overleaf/tables_2025.

4. Refresh the Section 6 covariate/Shapley text and alternative-index appendix figure:
   /Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 12_1_collective_quality_extensions.do
   Rscript 12_2_collective_quality_plots.R

   Estimates and CSVs: extensions/ (covariate AMEs, Shapley shares,
   alternative_linear_ame.csv, alternative_sd_units.csv).
   Log: Logs/12_1_collective_quality_extensions.log.
   Figures: results/figures/collective_rationality. The alternative-index figure
   uses four alternative_{HM,MaxMPI}_lpm_ame_{maximum,minimum}.png panels,
   copied to the Overleaf figure folder. Covariate effects and Shapley shares
   support the Section 6 discussion; their former review plots are not generated.
   Appendix Figure A11 matches Figure 8's three LPM specifications and style,
   using each member's pooled-sample SD of HM or RevMaxMPI. Both LPM analyses
   share programs/collective_linear_ame.ado and t(63) intervals. The previous
   alternative_ame.csv and HM/MaxMPI.ster files are retained multinomial-logit
   results and are not used for A11. Historical table5_*.ster files and
   joint_outcome_model_class_fe.ster are also no longer generated or required.
   Covariate effects and Shapley shares retain the fully controlled
   multinomial-logit appendix model.
   Covariate effects scale derivatives by one SD (continuous) or one unit
   (binary). Shapley shares allocate fit gains beyond class fixed effects.

The 2026-10-11 draft-only cleanup was checked in isolated runs. All eight
Figure 8/A10/A11 PNG panels and the categorical appendix table reproduced
byte-for-byte. Retained table, LPM, Shapley and SD exports match; multinomial
CCEI AME exports differ by at most 8.3e-11. All 120 covariate point estimates
and scales match exactly. Fresh pre-cleanup and cleaned runs show slight
variation in covariate standard errors, with identical model coefficients
and unchanged draft signs/significance claims. Inference code was unchanged.

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
