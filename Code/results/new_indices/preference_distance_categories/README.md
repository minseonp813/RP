# Relative revealed-preference distance by rationality role

Run from `Code`:

1. Stata: `99_30_preference_distance_categories.do`.
2. `Rscript programs/plot_preference_distance_categories.R`.
3. `python3 programs/build_preference_distance_categories.py`.
4. Compile `review.tex` with PDFLaTeX from this folder.

Final artifact: `output/pdf/preference_distance_by_ccei_ceiv_categories.pdf`.

Uses the paper's normalized `Ihat_ig`, separately for members with strictly higher/lower individual CCEI in each pair and wave. The two distances sum to one, so this is a relative alignment comparison, not an absolute individual-to-individual distance or an estimate of aggregation weights. Unadjusted means pool both waves with no RA-gap restriction; inference clusters by class.

Of 1,304 pair-waves, 199 are CCEI ties (tolerance 1e-9), and four additional untied pair-waves have undefined distances. Complete untied pair-wave counts in the four group CCEI/CEIV categories are 378, 100, 268, and 355. Role means, within-pair gaps, category contrasts, and exclusions are exported separately. The paired contrast respects the complementarity of the two distances. All tests are exploratory and unadjusted for multiple comparisons.
