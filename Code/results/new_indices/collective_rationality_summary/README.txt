Collective rationality summary: category figures, expanded Table 5, Figure 6.

Run from Code:
  /Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 99_35_collective_rationality_summary.do
  Rscript programs/plot_group_ceiv_by_member_category.R results/new_indices/collective_rationality_summary ccei portrait
  Rscript programs/plot_group_ceiv_by_member_category.R results/new_indices/collective_rationality_summary ceiv portrait
  Rscript programs/plot_cei_ame.R results/new_indices/collective_rationality_summary/figure6_ame.csv results/new_indices/collective_rationality_summary a portrait
  python3 programs/validate_collective_rationality_summary.py
  python3 programs/build_collective_rationality_summary.py

Compile collective_rationality_summary.tex from this result directory with pdflatex.
The final PDF uses four portrait US-letter pages: a Section 6 outline followed by the category figures, Table 5, and stacked Figure 6 panels.
The checked final PDF is copied to output/pdf/collective_rationality_summary.pdf.

All models use 1,304 pair-waves, 652 pairs, and 64 class clusters.
Categorical models omit Low-Low; High means individual CCEI above the pooled median.
Their comparison reports beta_HH-beta_LH, its class-clustered SE, and the
two-sided p-value for H0 beta_HH=beta_LH.
Continuous models use maximum CCEI and the within-pair CCEI gap. The equality
test of member-specific slopes is theta_max+2*theta_dist=0, equivalent to
beta_max=beta_min in the original max/min parameterization.
Each three-column block uses basic
class fixed effects, full controls with class fixed effects, and full controls
with pair fixed effects. Full controls include exact corner/midpoint shares;
RA controls and wave effects are excluded.

Independent validation reproduces all 12 sets of focal coefficients, clustered
SEs, p-values, equality-test p-values, and R-squared using within-OLS and the
class-cluster covariance. Stata also verifies all 8 multinomial AMEs against
finite differences of predicted probabilities. The original CCEI figure means
and bracket labels reproduce the attached figure. All 4 final pages were
rendered and visually checked. No draft manuscript or Overleaf assets changed.
