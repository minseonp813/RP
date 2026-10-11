# Figure 6 by communication proxies

> Historical outputs. The estimation and validation dofiles and report builder have been removed.
> See [the current Section 6 pipeline](../collective_rationality_summary/README.txt) for reproduction.

The delivered PDF is `output/pdf/figure6_communication_subgroups.pdf`.

The categories are neither index equal to one, CCEI only, CEIV only, and both. AMEs are per 0.1 increase in individual CCEI. Models include both individual CCEIs, Figure 6's controls and class effects, with class-clustered inference; no RA controls or restriction. Splits and missing-score exclusions exactly match `99_28_communication_heterogeneity.do` through shared preparation.

`model_status.csv` records optimizer bases, attempts, sample counts, and likelihoods. Changing the omitted outcome is an equivalent parameterization. The mutual-friendship separate model approaches perfect prediction despite reporting convergence: its near-zero AMEs should not be interpreted. The PDF uses the explicitly labelled shared-controls alternative on that group's page. The separate fit and its raw estimates remain available for diagnostics.

`figure6_ame.csv` contains the exact separate fits; `friendship_shared_ame.csv` contains the pooled friendship sensitivity check; `derivative_validation.csv` compares all interpreted AMEs with central finite differences of fitted probabilities. The eight exact mutual-friendship AMEs are explicitly flagged as skipped: this near-separated model produces numerical prediction failures when inputs are perturbed. `.ster` files preserve fitted models. PNGs share an axis from -40 to +40 percentage points. Subgroup comparisons are descriptive; differences in significance are not formal heterogeneity tests.
