Evidence for the XXX passages in Section 6 of Overleaf/main_v3.tex.

Run from Code:
  /Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do results/new_indices/section6_xxx/analysis.do
  Rscript results/new_indices/section6_xxx/plots.R

Input: the 1,304-row analysis_sample.dta and saved figure6.ster from
results/new_indices/collective_rationality_summary. The main model refit
reproduces the saved log likelihood within 1e-5. Covariate AMEs use the same
model, with class-clustered inference. The review plot scales continuous
derivatives by one sample SD and binary derivatives by one unit; neither
is an exact finite change. Missing indicators remain in the estimation but
are omitted from the display; collinear omitted indicators have undefined SEs.

Shapley shares allocate the gain in log likelihood beyond class fixed
effects across rationality, student, friendship, and choice-pattern blocks.
All 16 subset models converge on the same sample. Class effects enter first
and are not an allocated block. This is a descriptive allocation of fit.

HM and RevMaxMPI models use each measure's higher and lower individual
rationality scores. Group endpoint classifications coincide with CCEI;
CEIV and its CCEI-based individual calibrations remain fixed. Controls are
centered and scaled to stabilize the Hessian without changing the model.
Both models converge on 1,304 observations, 652 pairs, and 64 class clusters.
All alternative AMEs and intervals are finite; effects sum to zero across
outcomes for each measure and member.

The draft includes only the alternative-index Figure 8 variant.
covariate_ame.png is a separate review plot. alternative_joint_ame.png is
copied into Overleaf/figures_2025/collective_rationality by plots.R.

2026-10-10 refresh: rerun after 11_collective_quality.do imported the fresh
three-decimal CEIV CSV. Alternative categories use ceiv_at1 (the supplied
attainment flag), since two nonattained values round to 1.000. Covariate AMEs,
Shapley shares, and all alternative-index AMEs match the pre-refresh results.
