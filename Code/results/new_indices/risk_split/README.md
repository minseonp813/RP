# Group CCEI and CEIV by risk-preference similarity

> Historical outputs. The estimation dofile and report builder have been removed.
> See [the current Section 6 pipeline](../collective_rationality_summary/README.txt) for reproduction.

Reproduces Table 5 column (3) and the CCEI/CEIV version of Figure 6 separately
below and above the median risk-preference gap within each wave. The manuscript
and Overleaf outputs are not edited.

## Sample and split

- Risk preference is the existing `RA_i` / `RA_j` measure: the mean share allocated
  to the more expensive security over the 18 individual choices in that wave.
  Both measures were independently checked against the raw individual-choice
  datasets, with maximum discrepancy below 2e-16.
- Gap: `abs(RA_i - RA_j)`, using one observation per pair-wave.
- Wave medians: 0.109252 at baseline and 0.145095 at endline (unrounded medians
  are used for classification).
- No missing gaps or median ties. Each subgroup has 326 observations per wave,
  totaling 652 pair-waves from 462 distinct pairs in 64 classes.
- Membership is determined anew in each wave; 272 of the 652 pairs switch groups.
- Controls and sample preparation reuse `programs/prepare_collective_sample.do`.
  All models include both individual CCEIs, student/friendship and choice-pattern
  controls, and class fixed effects. Risk-aversion controls remain excluded as
  in column (3). Standard errors are clustered by class.

## Outputs and checks

- `review.pdf` / `review.tex`: OLS table and coefficient-difference tests on page 1;
  Figure 6 below and above the wave median on pages 2 and 3.
- `table5_split.tex`: pooled reference and the two subgroup OLS estimates.
- `table5_coefficients.csv`: focal coefficients, SEs, p-values and sample sizes.
- `slope_differences.csv`: above-minus-below slope differences and class-clustered
  Wald tests from a pooled regression fully interacting all controls and class
  effects with subgroup membership.
- `split_summary.csv`: actual wave-specific thresholds and subgroup counts.
- `figure6_ame.csv`: four-category multinomial-logit average marginal effects
  for both individual CCEIs in both subgroups. The legacy `median_ra` column is
  a pooled diagnostic from the shared helper; it is not the split threshold.
- `table5_*.ster`, `figure6_split_*.ster`: saved models for follow-up analysis.
- `analysis.log`: sample checks, estimates, convergence and final SUCCESS message.

All six OLS models and both multinomial models use their complete intended
samples. Both multinomial fits converge; the focal AMEs, SEs and confidence
intervals are finite, and category AMEs sum to zero for each member/subgroup.
CEIV endpoint classifications agree at both supplied numerical bounds.

Sparse class-by-category cells require numerical care in the multinomial fits.
The below-median model uses base outcome 1 and alternating BFGS/Newton-Raphson;
the above-median model uses base outcome 4 and Newton-Raphson. These change the
numerical optimization/normalization, not the model or covariates. The below-median
focal AMEs agree within 1e-5 probability units with the stabilized likelihood
values under alternative outcome normalizations, which did not all converge or
produce valid covariance matrices. Only converged fits with finite focal SEs
are used in the review. Both subgroup figures share an axis expanded to [-20,40]
percentage points so all intervals remain visible.

The pooled OLS coefficients reproduce the previous column (3) estimates. Existing
pooled CCEI/CEIV and CEIV/CEIC figures were regenerated in a temporary directory
to check compatibility of the shared plotting changes. All three review pages
were rendered and visually inspected.

## Main findings

The minimum individual CCEI predicts group consistency below (0.252, p=0.000105)
and above (0.221, p=0.000383) the median gap. The coefficient difference is not
significant (p=0.727). Thus this association is not confined to pairs with similar
measured risk preferences. Maximum CCEI predicts CEIV more strongly above the
median in point estimates (0.296 versus 0.064), but the cross-subgroup difference
is not significant (p=0.179).

This split describes heterogeneity; it does not establish mediation, identify
stable bargaining weights, or eliminate residual risk-preference differences
within each bin. The RA measure is a summary of risk attitudes, not a complete
representation of preferences.

## Redrawing saved AME panels

To redraw the saved AME panels, load the `plot_cei_ame()` function from the
Figure 8 section of `07_Figures_Main.R`, then run in R from `Code`:

```r
library(ggplot2)
plot_cei_ame("results/new_indices/risk_split/figure6_ame.csv", "results/new_indices/risk_split", "a")
```

The enriched `data/panel_group_new_indices.dta` is the previously validated
input produced by `99_23_new_indices.do`. Its CEIV values and bounds are merged
one-to-one onto the original 1,304-observation pair-wave analysis sample.
A delivery copy of the review is `output/pdf/new_indices_risk_split_review.pdf`.
