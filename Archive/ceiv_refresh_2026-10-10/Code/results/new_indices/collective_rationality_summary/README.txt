Section 6.3 pipeline: three figures and a three-column appendix table.

Run from Code to refresh the underlying estimates when needed:
  /Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 11_collective_quality.do

This dofile imports and validates CEIV from ../CEI_CEIV_CEIC_statistics.xlsx
and rebuilds data/panel_group_new_indices.dta before estimating the models.
Its Stata dependencies are programs/prepare_collective_sample.do and
programs/collective_multinomial_ame.ado. Exploratory dofiles 99_23 through
99_34 and their exploration-only .do helpers and logs have been removed.
The analysis log is saved in Code/Logs/11_collective_quality.log.
The dofile writes analysis_sample.dta, twelve table5_*.ster models,
table5_coefficients.csv, table5_diagnostics.csv, figure6.ster, and figure6_ame.csv
in this result directory. The figure6 filenames are legacy names for the current
draft's Figure 8 AMEs. Figures, TeX tables, and PDFs are built downstream.

Figures 6-8 are the last three sections of 07_Figures_Main.R.
Run those sections first with Code as code_dir and haven/ggplot2/dplyr/grid loaded.
The Figure 6 section reads the saved analysis sample and figure6_ame.csv, writes joint_outcome_quadrants.pdf
and joint_outcome_counts.csv here, and copies the PDF to the Overleaf figure folder.
The PDF retains its legacy filename but plots continuous group CCEI and CEIV values.
The Figure 7 section reads the same analysis sample and generates both CCEI and CEIV
mean bars and CDFs, category means/differences CSVs, and combined PNGs here.
It copies the four portrait PDF panels to the Overleaf figure folder.
The final Figure 8 section reads figure6_ame.csv and writes the maximum/minimum
CCEI AME panels as figure6_a_ame_maximum.png and figure6_a_ame_minimum.png.
It copies both PNGs to the Overleaf figure folder. Its plot_cei_ame() function
also supports the saved CEI/CEIC classifications and subgroup review plots.
Running the entire 07 script
still requires updating its earlier replication-folder path and legacy I_ig/HighCCEI inputs.

Run 08_Tables_Appendix.do from Code to export the three-column appendix table:
  /Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 08_Tables_Appendix.do

Its final Section 6.3 section can also be run independently from Code. It reads
table5_coefficients.csv and table5_diagnostics.csv from this directory, validates
the six categorical models, and writes
Code/results/tables/collective_ccei_ceiv_categories.tex. It copies that table to
Overleaf/tables_2025/collective_ccei_ceiv_categories.tex for the draft. Run
11_collective_quality.do first whenever the underlying estimates need refreshing.
The table export uses saved estimates and does not re-estimate these models.

The separate Python review-packet builder has been retired. The saved
collective_rationality_summary.tex/PDF and the table copy in this directory remain
historical review outputs; the active pipeline does not refresh them.
The saved PDF uses five portrait US-letter pages:
  1. Revised Section 6.3 outline and appendix placement.
  2. Figure 1: mean bars and CDFs for group CCEI and CEIV.
  3. Figure 2: four quadrants of joint group CCEI/CEIV status, with counts and shares.
  4. Figure 3: maximum/minimum individual CCEI average marginal effects.
  5. Appendix Table A1: only columns (1)-(3) of the supplied Table 5 expansion.
Figure and table numbers are local to this review packet.
The historical delivery copy is output/pdf/collective_rationality_summary.pdf.

CEIV is used consistently. All displayed analyses use 1,304 pair-waves from
652 pairs in 64 classes. Each pair contributes observations from two waves.
The quadrant counts classify pair-waves, rather than unique pairs:
  CCEI < 1, CEIV < 1: 415 (31.8%).
  CCEI = 1, CEIV < 1: 117 (9.0%).
  CCEI < 1, CEIV = 1: 300 (23.0%).
  CCEI = 1, CEIV = 1: 472 (36.2%).
The plot recomputes these counts from analysis_sample.dta, checks endpoint
classification against both CEIV bounds, and matches the AME sample shares.
joint_outcome_counts.csv records the counts and unrounded shares.
Grey circles show both indices below one, red circles show CCEI = 1 only,
blue diamonds show CEIV = 1 only, and a black square shows both equal to one.
Observed coordinates are not jittered; identical points overlap. The diagonal
marks CEIV = CCEI, dashed lines mark one, and the inset legend reports counts/shares.

Categorical regressions omit Low-High; High means individual CCEI strictly above
the pooled median across both waves (0.9806594), and Low otherwise.
Each sampled student is counted once per wave; no individual CCEI equals the median.
Low-Low, Low-High, and High-High contain 348, 608, and 348 pair-waves.
Columns (1), (2), and (3) use basic class effects, full controls
with class effects, and full controls with pair effects, respectively.
Full controls include exact corner/midpoint shares; RA controls and wave
effects are excluded. Low-Low and High-High coefficients now compare directly
with Low-High, so the appendix no longer displays post-estimation contrasts.
The AME model includes both continuous individual CCEIs with full controls
and class fixed effects, matching the control set of appendix column (2).

Raw estimates for all twelve original table models remain available; only
the six categorical models appear in the appendix. The estimation dofile retains
merge, sample, and endpoint checks and verifies all eight multinomial AMEs against
finite differences of predicted probabilities. The plotting sections check category
assignments and joint-outcome shares against the saved estimates.
The saved review packet's five pages were rendered and visually checked when it
was produced. The current figures, categorical appendix table, and definitions
remain consistent with the draft. The original Stata export matched the prior Python table. The 2026-10-09
update changes only the categorical baseline and removes contrast rows.

2026-10-09 appendix robustness figure:
  joint_outcomes_excluding_simple_choices.png contains exact and buffered
  exclusions using all 18 collective choices in each pair-wave. A choice is
  simple when its x share is at a corner or midpoint; mixtures count as simple.
  The exact panel removes 76 pair-waves (retains 1,228). The buffered panel
  uses an inclusive 2.5-percentage-point neighborhood and removes 286 (retains
  1,018). The original indices are unchanged. Flags and cell counts are saved
  in joint_outcome_exclusion_flags.csv and joint_outcome_exclusion_counts.csv.
  The matching Figure 6 robustness section of 07_Figures_Main.R creates it
  and copies it to the manuscript appendix.
  Both panels use Figure 6's scatterplot format with retained-sample legend counts.

Figure 8's math/friendship review uses the existing multinomial model.
  results/new_indices/section6_xxx/plots.R creates figure8_math_friendship_ame.png
  from covariate_ame.csv. Continuous derivatives are scaled by one sample SD;
  the friendship indicator uses one unit. This is a review plot, not yet a
  manuscript replacement.
