# CEIV and CEIC review results

Overleaf was fast-forwarded from `13f8abd` to `532f7a1fd04dc5adbd5b103f5a7effd8f303a294` before analysis.
The Overleaf working tree is clean. No draft tables, figures, or prose were edited.

## Review outputs

- `review.pdf`: original Table 5 panels A/B plus new CEIV/CEIC panels C/D, the fractional-response companion table, then the two Figure 6 variants.
- `table5_new_panels.tex`: new panels only; `table5_four_panels.tex`: combined review table.
- `table5_*.ster`: eight saved Stata OLS models; `figure6_*.ster`: two multinomial models.
- `figure6_[a,b]_ame.csv`: estimates and 95% confidence intervals in probability units; plotted in percentage points.
- `figure6_[a,b]_ame_[maximum,minimum].png`: original Figure 6 styling and dimensions.
- `analysis.log`: full merge checks and model output; `analysis_sample.dta`: exact 1,304-row analysis dataset.
- `precision_audit.csv`: 57 distinct group-waves comprising the 56 endpoint-sensitive CEIC classifications and one separate unresolved midpoint.

A delivery copy of the PDF is in `output/pdf/new_indices_review.pdf`.

## Merge validation

Source: `CEI_CEIV_CEIC_statistics.xlsx`, sheet `Group results`.
SHA-256: `02bcd79f428052a34bbf1af65ae4f5c633078a91a45460190fde2811aef28cab`.

The workbook has 1,304 unique `(group_id, post)` keys. IDs are imported as strings.
All 2,608 student-wave rows match by group and wave, with both student IDs checked
in either orientation. Workbook individual CCEIs, group CCEI, and existing CEI agree
with the original panel within 1e-7. Each pair-wave has exactly two student records.
All 1,304 group-panel rows match, with mover and nonmover IDs checked exactly.
All 652 wide main-panel rows match; both waves' mover and nonmover IDs agree exactly.
No unmatched observations, missing new indices, or row multiplication.

Enriched versions preserve the original panels and are saved as:
- `Code/data/panel_individual_new_indices.dta`: `ceiv_g`, `ceic_g` and workbook audit fields.
- `Code/data/panel_group_new_indices.dta`: the same new group outcomes and audit fields.
- `Code/data/panel_final_new_indices.dta`: `ceiv_g_base`, `ceiv_g_end`, `ceic_g_base`, `ceic_g_end`.

## Specifications and checks

The sample/control preparation was extracted unchanged from `99_1_Tables_Main.do`
into `programs/prepare_collective_sample.do`, used by both the original and new analysis.
Original Table 5 panels are copied directly from the synced draft, without re-estimation.
New OLS panels jointly include maximum and minimum individual CCEI, class FE in (1)-(3),
pair FE in (4), student/friendship controls in (2)-(4), and choice shares in (3)-(4).
No RA controls. Every estimate has 1,304 pair-waves and 64 class clusters.

Both multinomial models use the original Figure 6 specification: both individual CCEIs,
student/friendship/choice-share controls, class FE, and class-clustered SEs.
CCEI regressors are multiplied by 10, so derivatives represent a 0.1 CCEI increase.
All models converge. All AMEs/CIs are finite, sum to zero across categories for each
member (within 1e-8), and fit the original [-20,30] percentage-point axis.

Classification A numeric outcomes: 1=(CCEI<1, CEIV<1), 2=(CCEI=1, CEIV<1),
3=(CCEI<1, CEIV=1), 4=(CCEI=1, CEIV=1).
Classification B: 1=(CEIV<1, CEIC<1), 2=(CEIV=1, CEIC<1), 3=(CEIV=1, CEIC=1).
There are zero (CEIV<1, CEIC=1) rows. Plots display ascending numeric categories
from top to bottom, matching the existing Figure 6 orientation.
- Classification A counts (outcomes in ascending numeric order): 415, 117, 300, 472.
- Classification B counts (outcomes in ascending numeric order): 532, 378, 394.

## Numerical qualifications from the source workbook

The primary results use supplied point estimates and the draft's 1e-9 equality tolerance.
For group `13106211310615`, baseline, CEIC=0.83875 is marked `midpoint_unresolved`,
with bounds [0.836,0.8415] and precision flag 0. It remains included.
Another 56 CEIC point estimates are below one but have upper bound one;
classification B may therefore depend on finer numerical resolution for these rows.
CEIV classifications are invariant across their supplied interval endpoints.
The workbook also states that full-sample certificate verification was deferred;
this analysis validates the merge and supplied data, not the index calculation certificates.

## Reproduction

From `Code`:

```sh
/Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 99_23_new_indices.do
Rscript programs/plot_cei_ame.R results/new_indices/figure6_a_ame.csv results/new_indices a
Rscript programs/plot_cei_ame.R results/new_indices/figure6_b_ame.csv results/new_indices b
python3 programs/build_new_indices_review.py
cd results/new_indices
pdflatex -interaction=nonstopmode -halt-on-error review.tex
```

Check the final SUCCESS message in `analysis.log` (Stata batch exit status alone
is not sufficient). The review PDF was rendered and the new fractional-response page visually inspected after this extension.

## Fractional-response extension

`99_24_new_indices_fractional.do` is runnable separately and called automatically by
`99_23_new_indices.do`. It writes `table5_fractional.tex`, `fractional.log`, and
16 saved average-partial-effect estimates (`table5_fractional_*.ster`).
The table is included on page 2 of `review.tex` / `review.pdf`.
All four outcomes (CCEI, CEI, CEIV, CEIC) are shown for comparison.
Columns (1)-(3) use fractional logit with class fixed effects; column (4)
replicates the existing panel fractional probit, including pair means of
time-varying regressors and a wave effect, with class fixed effects.
Column (4) is not conditional logit or pair fixed effects.
Reported average partial effects hold pair-mean regressors fixed.
All 16 models converged on 1,304 observations and 64 class clusters.
The CCEI/CEI APEs and SEs reproduce the existing fractional-response table
to its displayed precision. CEIC precision caveats continue to apply.
