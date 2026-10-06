Section 6.3 review packet: three figures and a three-column appendix table.

Run from Code to refresh the underlying estimates when needed:
  /Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 99_35_collective_rationality_summary.do

This dofile imports and validates CEIV from ../CEI_CEIV_CEIC_statistics.xlsx
and rebuilds data/panel_group_new_indices.dta before estimating the models.
Its Stata dependencies are programs/prepare_collective_sample.do and
programs/collective_multinomial_ame.ado. Exploratory dofiles 99_23 through
99_34 and their exploration-only .do helpers and logs have been removed.

Build the review packet from the saved analysis sample and results:
  Rscript programs/plot_group_ceiv_by_member_category.R results/new_indices/collective_rationality_summary ccei portrait
  Rscript programs/plot_group_ceiv_by_member_category.R results/new_indices/collective_rationality_summary ceiv portrait
  Rscript programs/plot_collective_joint_outcomes.R results/new_indices/collective_rationality_summary
  Rscript programs/plot_cei_ame.R results/new_indices/collective_rationality_summary/figure6_ame.csv results/new_indices/collective_rationality_summary a portrait
  python3 programs/validate_collective_rationality_summary.py
  python3 programs/build_collective_rationality_summary.py

Compile collective_rationality_summary.tex from this result directory with pdflatex.
The final PDF uses five portrait US-letter pages:
  1. Revised Section 6.3 outline and appendix placement.
  2. Figure 1: mean bars and CDFs for group CCEI and CEIV.
  3. Figure 2: four quadrants of joint group CCEI/CEIV status, with counts and shares.
  4. Figure 3: maximum/minimum individual CCEI average marginal effects.
  5. Appendix Table A1: only columns (1)-(3) of the supplied Table 5 expansion.
Figure and table numbers are local to this review packet.
The checked final PDF is copied to output/pdf/collective_rationality_summary.pdf.

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

Categorical regressions omit Low-Low; High means individual CCEI above the
pooled median. Columns (1), (2), and (3) use basic class effects, full controls
with class effects, and full controls with pair effects, respectively.
Full controls include exact corner/midpoint shares; RA controls and wave
effects are excluded. The post-estimation comparison reports beta_HH-beta_LH,
its class-clustered SE, and the two-sided p-value for H0 beta_HH=beta_LH.
The AME model includes both continuous individual CCEIs with full controls
and class fixed effects, matching the control set of appendix column (2).

Raw estimates for all twelve original table models remain available; only
the six categorical models appear in the appendix. Independent validation
reproduces all twelve sets of focal coefficients, clustered SEs, p-values,
equality-test p-values, and R-squared. Stata's estimation script verifies all
eight multinomial AMEs against finite differences of predicted probabilities.
All five final pages were rendered and visually checked. No draft manuscript
or Overleaf assets changed.
