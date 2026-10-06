# Risk surveys across four CCEI-CEIV categories

All available respondents and a separate lower-CCEI-member section, pooled across both waves, without any RA-gap restriction. No manuscript edits.

Run from `Code`:

```sh
/Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 99_27_risk_survey_joint_categories.do
Rscript programs/plot_risk_survey_categories.R
python3 programs/build_risk_survey_categories.py
```

Compile `review.tex` from this directory with `pdflatex`; copy `review.pdf` to `output/pdf/risk_survey_by_ccei_ceiv_categories.pdf` at the repo root.

The shared input is `programs/load_risk_survey_panel.do`, also used by `99_26_risk_survey_review.do`. It merges CEIV into `panel_individual.dta` from `panel_group_new_indices.dta`, verifies pair-wave outcomes, and validates the survey scores.

Figure 6 classification uses a `1e-9` endpoint tolerance. CEIV status is identical at both numerical bounds:

| Category | CCEI | CEIV | Pair-waves | Respondents with survey answers |
| --- | --- | --- | ---: | ---: |
| 1 | <1 | <1 | 415 | 830 |
| 2 | =1 | <1 | 117 | 234 |
| 3 | <1 | =1 | 300 | 600 |
| 4 | =1 | =1 | 472 | 943 |

All three questions have the same 2,607 observed responses. Their distributions sum to one within each category. Group indices are shared by both members; survey reports remain respondent-level observations. Class clustering accommodates the two members, repeated waves, and within-class dependence.

The report retains the all-member section on pages 1–4 and adds less rational members on pages 5–8. In the added section, select the member with strictly lower individual CCEI in each pair and wave. Ties within `1e-9` are excluded because no member is uniquely less rational. All 199 ties in this data are exact: 37, 17, 31, and 114 in categories 1–4. This leaves 378, 100, 269, and 358 members (1,105 total), all with complete survey answers. Selection can switch members across waves. Differences from the all-member sample also reflect excluding ties, which are more prevalent in category 4.

- `category_counts.csv`: pair-wave category counts and shares, plus tied and untied pair-wave counts.
- `score_means.csv`: cooperation and similarity mean scores, SEs, and 95% class-clustered t intervals. Whose-suggestions responses are nominal and are not averaged.
- `response_shares.csv`: full response frequencies, shares, and class-clustered SEs/intervals. CSV intervals are unconstrained Wald intervals.
- `category_tests.csv`: mean-score omnibus tests, category-4-minus-category-3 contrasts, per-response share contrasts, and full-distribution omnibus tests.
- Distribution tests stack K-1 binary response indicators and fit unrestricted category-by-response means, clustering by 64 classes. Omitting one response avoids the adding-up redundancy. Tests compare all four categories or just categories 4 and 3.

The means, response shares, and tests include `sample=all` or `sample=less`; the same estimation and plotting code is used for both samples. Plots for the added section have a `less_` filename prefix.

The comparisons are descriptive and unadjusted for covariates or multiple testing. Ordinal mean scores assume equal response spacing; full distributions supplement them. Categories 4 and 3 both have CEIV=1, but their difference does not hold other pair characteristics fixed and does not identify stable weights or a deliberation process. Cooperation's original wording and answer anchors have not been recovered; only coded scores are displayed.
