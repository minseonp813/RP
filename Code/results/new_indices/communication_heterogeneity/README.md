# Communication-proxy heterogeneity

> Historical exploration. The estimation dofile and split-OLS helper have been removed.
> See [the current Section 6 pipeline](../collective_rationality_summary/README.txt) for reproduction.

Exploratory subgroup regressions for group CCEI and CEIV, matching Table 5 column (3). No manuscript changes or ML estimation.

Run from `Code`:

```sh
/Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 99_28_communication_heterogeneity.do
Rscript programs/plot_communication_heterogeneity.R
python3 programs/build_communication_heterogeneity.py
```

Compile `review.tex` in this directory with `pdflatex`; final deliverable is `output/pdf/communication_heterogeneity_review.pdf` at the repo root.

Preparation reused `programs/prepare_collective_sample.do`. The former `collective_split_ols.ado` fitted each split, estimated fully interacted slope differences, and exported tables and diagnostics.

Four proposed moderators, with an additional binary friendship check:

- **Wave:** baseline versus endline, 652 pair-waves each.
- **Dyadic friendship:** 0=no directed nomination, 1=one-sided, 2=mutual; eligible counts 912/225/167. Friendship changes for 167 pairs across waves. The binary check uses the existing Table 5 indicator (`friendship>=1`): 912/392.
- **Pair-average math:** mean of two observed scores, excluding 13 pair-waves with at least one missing score. Within-wave medians 2.5 at baseline and 3 at endline. Below versus at/above median yields 530/761 eligible observations. Baseline/endline median ties: 111/123, assigned high.
- **Less rational member's math:** member with strictly lower individual CCEI in that wave, excluding 199 CCEI ties (`1e-9` tolerance) and 11 selected-member math missing values. Within-wave median 3 in both waves. Below versus at/above yields 522/572 eligible observations (1,094 total). Baseline/endline median ties: 116/104, assigned high. Identity can change across waves.

Missing math is recovered from the respondent's `mathscore_i_missing` flag before the original controls' imputation; missing scores are not assigned to the low bin. Other controls retain Table 5's existing cleaning and missing-indicator treatment.

All models include maximum and minimum individual CCEI jointly, the original student/personality, friendship/network, and corner/midpoint-share controls, with class fixed effects and class clustering. RA controls are excluded. Pooled friend/math splits also have wave-fixed-effects robustness fits. The wave-control term and all other covariates and class effects are fully interacted in cross-subgroup difference tests.

For each split, outputs are:

- `*_coefficients.csv`: pooled eligible reference and subgroup slopes, SEs, 95% t intervals, p-values, fitted N, class count, and eligible N.
- `*_tests.csv`: pairwise differences and omnibus moderation tests for both individual CCEIs, including models of `ccei_g-ceiv_g` to test whether moderation differs across outcomes. This difference outcome is a test device, not a new rationality measure.
- `*_diagnostics.csv`: eligible counts, mean individual/group indices, minimum-CCEI SD, and endpoint shares.
- `_wavefe` files: the same fits with wave controls added.
- `split_summary.csv` and `exclusions.csv`: group/wave counts, math cutoffs/ties, and role/missing exclusions.

Separate `reghdfe` fits drop class singletons; the fully interacted pooled model retains all eligible observations and reproduces the separate subgroup slopes (asserted to `1e-7`). Singleton observations contribute no within-cell identifying variation. Tests account for shared classes and repeated pairs across bins/waves. Subgroup fits can have fewer classes than the pooled tests; counts are reported explicitly.

All comparisons are observational and exploratory, without multiple-testing adjustment. The proposed variables are moderators/proxies, not identified mediators. Smaller high-proxy slopes do not disprove a communication channel: differences in support, index ceilings, contemporary friendship, and ability selection can matter. The richer three-category friendship model permits distinct controls and slopes for one-sided/mutual ties, whereas the binary model pools them. No model identifies stable weights or turn-taking.
