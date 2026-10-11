Section 6 collective-quality pipeline

Run the analysis scripts from Code.

1. Refresh the main estimates:
   /Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 11_collective_quality.do

   Input: ../TalkFile_ceiv_fresh_3dp.csv and the existing panel data.
   Analysis sample: data/collective_analysis_sample.dta.
   Outputs here: twelve table5_*.ster models, table5_coefficients.csv,
   table5_diagnostics.csv, joint_outcome_model.ster, and joint_outcome_ame.csv.
   Log: Logs/11_collective_quality.log.

2. Refresh Figures 6-8 using the final three sections of 07_Figures_Main.R.
   For those sections alone, set code_dir to Code, load haven/ggplot2/dplyr/grid,
   and load the paper_theme, sig_mark, and clustered_ks_test definitions above.
   Figures go to results/figures/collective_rationality and are copied to
   ../Overleaf/figures_2025/collective_rationality. Counts, category estimates,
   pairwise tests, and bootstrap draws are written here.
   Figure 8 image filenames retain the legacy figure6_a_ame_* prefix used by TeX.

3. Export the collective-outcomes appendix table using its section in
   08_Tables_Appendix.do (headed "Collective-outcomes appendix table").
   It can run independently from Code after step 1, using the coefficient and
   diagnostic CSVs. It writes results/tables/collective_ccei_ceiv_categories.tex
   and copies it to ../Overleaf/tables_2025.

4. Refresh the Section 6 extensions and alternative-index appendix figure:
   /Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 12_1_collective_quality_extensions.do
   Rscript 12_2_collective_quality_plots.R

   Estimates and CSVs: extensions/ (covariate AMEs, Shapley shares, HM/RevMaxMPI).
   Log: Logs/12_1_collective_quality_extensions.log.
   Figures: results/figures/collective_rationality. The alternative-index figure
   is copied to the Overleaf figure folder; covariate figures are review outputs.
   Covariate effects scale derivatives by one SD (continuous) or one unit
   (binary). Shapley shares allocate fit gains beyond class fixed effects.

Current sample: 1,304 pair-waves, 652 pairs, 64 classroom clusters.
CEIV endpoint status uses ceiv_at1 from the supplied attainment flag, since two
nonattained observations round to 1.000. Individual CCEI categories use the
pooled median across both waves (0.9806594): 348 Low-Low, 608 Low-High, 348 High-High.
Figure 7 means/contrasts use class-clustered CR1 inference and t(63); its KS tests
use Figure 3's centered whole-class bootstrap with 9,999 draws.
ceiv_refresh_comparison.csv is a retained diagnostic from the CEIV refresh.

The former results/new_indices folder was retired on 2026-10-11. Historical
review packets, exploratory results, and their original documentation are in
../../../Archive/new_indices_cleanup_2026-10-11/Code/results/new_indices.
The archive's migration_manifest.tsv records every original file's destination
and SHA-256 before path updates. Overleaf figure/table filenames were preserved.
