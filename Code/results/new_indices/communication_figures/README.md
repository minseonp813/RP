# Figure 6 by communication proxies

Run from `Code`:

For the R plotting step, load ggplot2 and the `plot_cei_ame()` function from
the Figure 8 section of `07_Figures_Main.R`.

1. Stata: `99_29_communication_figure6.do` (eleven separate four-category multinomial fits).
2. Stata: `99_29b_friendship_figure6_check.do` (friendship-specific CCEI slopes with shared controls/class effects).
3. Stata: `99_29c_validate_figure6.do` (independent numerical derivatives of fitted probabilities).
4. `python3 programs/build_communication_figure6.py` (combine plot inputs and write the PDF source).
5. `plot_cei_ame("results/new_indices/communication_figures/plot_input.csv", "results/new_indices/communication_figures", "a")` (one shared axis across all models).
6. Compile `review.tex` with PDFLaTeX from this folder.

The delivered PDF is `output/pdf/figure6_communication_subgroups.pdf`.

The categories are neither index equal to one, CCEI only, CEIV only, and both. AMEs are per 0.1 increase in individual CCEI. Models include both individual CCEIs, Figure 6's controls and class effects, with class-clustered inference; no RA controls or restriction. Splits and missing-score exclusions exactly match `99_28_communication_heterogeneity.do` through shared preparation.

`model_status.csv` records optimizer bases, attempts, sample counts, and likelihoods. Changing the omitted outcome is an equivalent parameterization. The mutual-friendship separate model approaches perfect prediction despite reporting convergence: its near-zero AMEs should not be interpreted. The PDF uses the explicitly labelled shared-controls alternative on that group's page. The separate fit and its raw estimates remain available for diagnostics.

`figure6_ame.csv` contains the exact separate fits; `friendship_shared_ame.csv` contains the pooled friendship sensitivity check; `derivative_validation.csv` compares all interpreted AMEs with central finite differences of fitted probabilities. The eight exact mutual-friendship AMEs are explicitly flagged as skipped: this near-separated model produces numerical prediction failures when inputs are perturbed. `.ster` files preserve fitted models. PNGs share an axis from -40 to +40 percentage points. Subgroup comparisons are descriptive; differences in significance are not formal heterogeneity tests.
