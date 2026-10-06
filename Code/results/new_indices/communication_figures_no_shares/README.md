# Sensitivity to individual choice-share controls

The analyses retain both individual CCEIs, all student/personality and friendship/network controls, class fixed effects, and class-clustered standard errors. They omit only `corner_share_max`, `corner_share_dist`, `mid_share_max`, and `mid_share_dist`. Samples and subgroup definitions are identical to the original analyses.

Run from `Code`:

1. Stata: `99_28_communication_heterogeneity.do no_shares`
2. Stata: `99_29_communication_figure6.do no_shares`
3. Stata: `99_29b_friendship_figure6_check.do no_shares`
4. Stata: `99_29c_validate_figure6.do no_shares`
5. `python3 programs/build_communication_figure6.py no_shares`
6. `Rscript programs/plot_cei_ame.R results/new_indices/communication_figures_no_shares/plot_input.csv results/new_indices/communication_figures_no_shares a`
7. Compile `review.tex` using PDFLaTeX from this folder.

The delivered PDF is `output/pdf/figure6_communication_without_choice_shares.pdf`. It includes a comparison of the original and reduced-control continuous regressions, followed by subgroup Figure 6 panels. All panels within this PDF use a common axis, from -30 to +50 percentage points.

Ten separate multinomial fits converge and produce validated marginal effects. The mutual-friendship separate model fails to converge under both omitted-outcome parameterizations. Its page uses the explicitly labelled shared-controls alternative, which allows friendship-specific CCEI slopes but shares other controls and class effects across groups. `model_status.csv` records the failed fit; it is not silently substituted in the separate-fit CSV.

All 104 exported AMEs from the ten successful separate fits and three shared-model subgroup averages match numerical derivatives of fitted probabilities within 1e-8. Continuous estimates and formal heterogeneity tests are in the adjacent `communication_heterogeneity_no_shares` folder. Existing original-control outputs are preserved.
