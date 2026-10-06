# Risk-survey exploration

Local exploratory extension of Figure 5 to group CCEI and CEIV. No manuscript edits.

> Historical outputs. The estimation dofile, plotter, and report builder have been removed.
> See [the current Section 6 pipeline](../collective_rationality_summary/README.txt) for reproduction.

Saved report: `output/pdf/new_indices_risk_survey_review.pdf` at the repo root.

- Data: `panel_individual.dta`, merged by group and wave with `ceiv_g` from `panel_group_new_indices.dta`.
- Survey coding comes from `02_clean_survey.R`: cooperation = 6 minus Risk_q1 (1–5); similarity = Risk_q2 (1–4); whose suggestions = Risk_q3 (four nominal responses). Original cooperation wording/anchors have not been recovered; plots use coded scores.
- Distance is `Ihat_ig`, the raw normalized index currently used in Figure 5. All three outcomes' plots share its 2,560-observation sample. Distance means and counts reproduce Figure 5 exactly; confidence intervals are now class clustered rather than the original unclustered intervals.
- Figure 5(b) uses the pooled risk-gap median in the distance-observed sample (0.12672050582204158), yielding 1,280 respondents. This differs from the wave-specific split in `risk_split`.
- `response_means.csv` contains category means, class-clustered SEs, 95% t intervals, and response counts. `associations.csv` contains Pearson and Spearman correlations, class-clustered p-values, per-category slopes, and categorical R-squared/joint tests. Samples: `available` (2,560 distance; 2,607 group outcomes), `common` (2,560 for all), and `high_gap` (similarity only, 1,280).
- Correlation p-values use class-clustered regressions (raw or average tied ranks). Adjusted slopes include wave and class fixed effects, without Table 5's additional controls. Whose-suggestions responses are nominal, so no ordinal correlation is imposed.
- Group outcomes are identical for the two members; analyses describe the relationship with each respondent's report, with dependence accommodated by clustering at 64 classes. These are descriptive associations, not identification of a deliberation process or aggregation weights.
- Within each pair-wave the defined distances sum to one. Relative alignment should not be interpreted as absolute group decision quality.
