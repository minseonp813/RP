# Continuous individual CCEIs and relative preference distance

Run from `Code`:

1. Stata: `99_31_preference_distance_ccei_slopes.do`.
2. `Rscript programs/plot_preference_distance_slopes.R`.
3. `python3 programs/build_preference_distance_slopes.py`.
4. Compile `review.tex` with PDFLaTeX from this folder.

Final artifact: `output/pdf/preference_distance_ccei_coefficients.pdf`.

Outcome is the more rational member's normalized member-to-group distance, with maximum and minimum individual CCEI entering jointly. Categories interact with both CCEIs. This retains the same 1,101 complete untied pair-waves as the preceding role-mean comparison. An identically specified regression for the less rational member has opposite coefficients because the two distances sum to one.

The CCEI-only specification has category-specific intercepts and slopes. The controlled specification adds common Figure 6 student/personality and friendship/network coefficients and common class fixed effects. Choice-share and RA controls are excluded, and waves are pooled without wave fixed effects. Inference clusters by 64 classes. The controls are shared across categories to avoid estimating a full set of class effects and controls within the smallest category.

`ccei_coefficients.csv` reports coefficients per 0.1 CCEI increase; multiply by ten for one-unit coefficients. `common_ccei_means.csv` compares raw means with predicted means at common CCEIs, and with common CCEIs plus other controls averaged over their pooled distribution. Reference CCEIs are the pooled sample means (maximum 0.967364, minimum 0.834200); both lie in each category's observed joint support. Standard errors treat those reference values as fixed. `both_minus_ceiv_only.csv` gives targeted category-4-minus-category-3 mean and slope tests.

The comparison is descriptive. It assesses how much differences in CCEI levels account for the observed category means under a linear specification, while allowing different slopes by category. It does not identify aggregation weights or a causal communication mechanism.
