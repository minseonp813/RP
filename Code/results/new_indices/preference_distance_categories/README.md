# Relative revealed-preference distance by rationality role

> Historical outputs. The estimation dofile, plotter, and report builder have been removed.
> See [the current Section 6 pipeline](../collective_rationality_summary/README.txt) for reproduction.

Final artifact: `output/pdf/preference_distance_by_ccei_ceiv_categories.pdf`.

Uses the paper's normalized `Ihat_ig`, separately for members with strictly higher/lower individual CCEI in each pair and wave. The two distances sum to one, so this is a relative alignment comparison, not an absolute individual-to-individual distance or an estimate of aggregation weights. Unadjusted means pool both waves with no RA-gap restriction; inference clusters by class.

Of 1,304 pair-waves, 199 are CCEI ties (tolerance 1e-9), and four additional untied pair-waves have undefined distances. Complete untied pair-wave counts in the four group CCEI/CEIV categories are 378, 100, 268, and 355. Role means, within-pair gaps, category contrasts, and exclusions are exported separately. The paired contrast respects the complementarity of the two distances. All tests are exploratory and unadjusted for multiple comparisons.
