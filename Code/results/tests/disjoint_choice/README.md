# Leave-one-choice-out validation

Updated 2026-10-09. This replaces the former Table A7 disjoint-choice workflow.
Removing the preceding summary table changes its appendix number; the manuscript
label remains `tab:disjoint_choice`.

Run from `Code`:

1. `Rscript programs/calculate_indices_disjoint.R` calculates all 18 folds.
2. Run the final **Leave-one-choice-out validation** section of
   `08_Tables_Appendix.do` to estimate and export the six Table 3 specifications.
   The section is self-contained and does not require earlier appendix sections.
3. The final hold-out review section of `09_Figures_Appendix.R` writes the
   coefficient-stability comparison and review plot after estimation.

`LOCO_CORES` defaults to 4. `LOCO_FOLDS=1` selects a validation run; restore
`LOCO_FOLDS=18` to finish. `LOCO_OUTPUT_DIR` selects another cache directory
and must be identical in R, Stata, and plotting. Complete folds and donor chunks
resume. Cache identities include the design, seed, roster, raw choices, and
numerical/calculation source hashes. Changed inputs or code require a fresh
output directory. Only a complete 18-fold, 652-pair run exports to the manuscript.

## Design and estimation

Seed 20260812 generates a separate random permutation for each student-wave.
Every choice is held out exactly once across the 18 folds. Individual CCEI and
exact corner/midpoint shares use the remaining 17 choices. Distance uses only
one held-out choice from each member and all 18 collective choices. M uses
those same two held-out choices against all 651 non-own pairs in the same wave,
across all classes. Undefined donor distances receive 0.5. The shared donor
builder retains compact member summaries. Its compiled single-choice CCEI
kernel uses the same exact expenditure comparisons and critical ratios as the
shared reference R routines; calculations have no approximation or time cap.

Each fold starts from Table 3's balanced 2,512-observation sample and requires
defined held-out distance in both waves. All six specifications share the
resulting fold sample. Columns 1-3 use higher CCEI (both members high in ties);
columns 4-6 use the signed CCEI difference. Every column includes recomputed M.
The three specifications use baseline class effects, full controls with class
effects, and full controls with individual effects. Controls, missing indicators,
and class-clustered SEs match Table 3; risk-aversion controls are excluded.

## Saved results

- `fold_assignments.csv`: all 46,944 held-out assignments.
- `indices/fold_01.dta` through `fold_18.dta`: training rationality/shares,
  held-out distance, M, held-out round, and donor counts for all 2,608 member-waves.
- `loco_config.rds`, `run_config.dta`: input identity and completed-run metadata.
- `fold_estimates.dta`: every fitted coefficient, clustered SE, p-value, N,
  R-squared, cluster count, residual degrees of freedom, and omission flag.
- `focal_fit_inference.csv`: focal coefficient and inference for each of 108 fits.
- `fold_summary.dta`: median and descriptive 2.5th/97.5th fold percentiles for
  focal and M coefficients. These ranges are not confidence intervals.
- `diagnostics_by_fold_wave.csv`: CCEI changes, higher-member agreement, ties,
  undefined own-group distance, regression counts, and undefined donor fractions.
- `sample_selection.csv`: retained/excluded students' full-data rationality.
- `distance_distributions.csv`: full and regression-sample I/M distributions,
  including mass at zero, one half, and one.
- `coefficient_stability.csv`, `fold_coefficient_stability.png`: comparison with
  Table 3; plot bars are confidence intervals for separate fits, not fold ranges.
- `Code/results/tables/table_leave_one_choice_out.tex`: six-column
  production export, copied to `Overleaf/tables_2025` by Stata.

Folds overlap and are not independent samples. Undefined single-choice distance
can substantially change the sample. The outcome uses much less individual
information than full-data distance and is not assumed to be an unbiased noisy
version of it. Consult the sample-selection and boundary diagnostics before
interpreting coefficient magnitudes or individual-fit significance.

## Validation

The compiled kernel was compared with reference R on 220 actual/random small
expenditure matrices, followed by a three-pair complete-donor fixture. A full
first fold was then checked for all 651 donors, own-diagonal agreement, and
within-pair adding-up. All inputs matched the slower reference implementation
exactly. Independent reference-R recomputation of all 651 donors for two target
pair-waves reproduced M within 4.5e-16. Logs are in `Code/Logs/loco_*` and
`08_leave_one_choice_out.log`. The former workflow and pre-update outputs remain
in `Archive/Archive/disjoint_choice_before_migration_2026-10-06` and
`Archive/todo_updates_2026-10-09`; they are not current inputs.

## Completed run (2026-10-09)

All 18 folds and 108 six-specification fits completed. Coefficients and focal
clustered SEs were independently reproduced in R to 2.1e-15 and 3.7e-16.
The native CCEI, HM, MaxMPI, and RA default donor callers were also checked
against their original/default results on complete three-pair fixtures.

Paired adding-up identities make some cluster score directions exactly zero.
Standard reghdfe covariance multiplication suffered numerical cancellation in
one fold. The final 08 section retains its estimates and degree-of-freedom
correction but forms the same cluster sandwich as a Gram matrix of cluster
influences. This preserves the specification and valid first-fold inference;
all saved coefficients and SEs are finite. Validated with reghdfe 6.12.5.

Across fold-wave observations, training CCEI increases by 0.00470 on average;
6.97% change by more than 1e-7. Higher-member classifications agree in 97.17%
of comparisons. Mean training tie rates are 10.65% at baseline and 23.65% at
endline, compared with full-data rates of 9.20% and 21.32%. Undefined own-group
distance averages 47.99% at baseline and 55.37% at endline (fold-wave range
45.86%-57.52%). Undefined donor distances average 42.24% and receive 0.5.

The common regression samples contain 584-768 student-waves (median 662),
with 56-62 class clusters (median 59). Averaging fold-specific means, retained
full-data CCEI is 0.8848 versus 0.9244 among excluded observations; corresponding
full-rationality shares are 29.61% versus 38.64%. This is substantial selection.

Regression-sample held-out distance has mean 0.5, average SD 0.4473, and average
shares of 33.66% at each boundary and 1.68% at one half. M has mean 0.5, average
SD 0.1891, and no observations at zero, one half, or one. Full distributions
and fold-specific extrema/percentiles are in distance_distributions.csv.

Focal medians in columns 1-6 are -0.0204, -0.0178, +0.0181, -0.0711, -0.1178,
and -0.0207; their full-choice Table 3 counterparts are -0.0920, -0.0773,
-0.0905, -0.2491, -0.2283, and -0.2143. Negative estimates occur in
14, 14, 8, 11, 14, and 10 folds; separate-fit 5% rejections occur in
3, 3, 1, 1, 1, and 1 folds. Every descriptive fold percentile range includes
zero. This does not uniformly confirm the full-choice result, especially
with individual effects. Counts of rejections across overlapping folds are
descriptive and are not aggregate hypothesis tests.
