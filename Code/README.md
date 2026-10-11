# Distance analysis and manuscript assets

Run the scripts from `Code`. The existing preparation sequence ends with
`2_3_build_panel_individual.R`, which writes the canonical
`data/panel_individual.dta` containing the benchmarks, exact-choice shares,
sample flags and placebo-adjusted distance used by the manuscript.

`1_1_calculate_indices.R` calculates and merges the donor benchmarks; `2_1`
enriches the pair panel, `2_2` builds the group panel, and `2_3` maps member
benchmarks and adds exact corner/midpoint controls and `Istar_ccei = Ihat_ig - M_ccei`.
The CCEI benchmark must be present upstream before running `2_3`.
The panel retains all 2,608 student-waves; the current Table 3 sample is the
2,560 rows with defined distance. `balanced_t3` identifies the older balanced
subsample. There is one canonical individual panel and no additional
preparation step after `2_3`.

The manuscript generators read this panel:

| Script | Distance-related manuscript outputs |
| --- | --- |
| `06_Tables_Main.do` | Tables 1 and 3 |
| `07_Figures_Main.R` | Figures 3--8 |
| `08_Tables_Appendix.do` | Tables A1, A2, A3 and A6; Figure A7 calculations |
| `09_Figures_Appendix.R` | Figure A6; Figure A7 plots and Figure A8 placebo panels |

Supplementary analysis scripts use consecutive `99_*` numbers. Companion
scripts and notebooks share the same number:

| Prefix | Analysis |
| --- | --- |
| `99_1` | Placebo reassignment: CCEI, HM and MaxMPI |
| `99_2` | Odd/even choice indices |
| `99_3` | Taking-turns permutations |
| `99_4` | Taking-turns robustness tables |
| `99_5` | Taking-turns figures |
| `99_6` | Alpha simulations and paired I versus I-M experiments |
| `99_7` | Risk-survey mean checks |
| `99_8` | Within-wave median sensitivity |

The former standalone mover and alternative-measure appendix scripts were
retired; `08_Tables_Appendix.do` generates the current Tables A3 and A5 directly.
Loose logs are stored in `Logs`. Batch logs that shared a name with an existing
analysis log have a `_batch` or `_project_root` suffix. Historical logs and saved
hash records retain the script names used when those analyses ran. The cleanup
record is in `../Archive/code_cleanup_2026-10-11/README.txt`.

Table A5 uses full HM/MaxMPI benchmarks in `results/benchmarks/{hm,maxmpi}`
and the retained RA benchmark in
`results/benchmarks/retained_ra/ra_placebo_member_wave.dta`. Its RA donor
matrix is retained in that directory for provenance. Figure 4 continues to
use `results/placebo_normalized/placebo_donor_matrix.csv`.

The sections in 06/08 and 07/09 can be run independently. They write canonical
assets to `results/tables` and `results/figures` and copy manuscript assets to
`../Overleaf`. For Figure A7, run its section in 08 before its plotter in 09.
For Figure A8, run the Figure 4 section in 07 before the A8 plotter in 09.
Rebuilding a figure follows the current code and sample definitions; it is
separate from relocating its unchanged inputs.

Figures 3 and 7 include class fixed effects without additional controls.
`programs/class_fe_category_summary.R` standardizes predicted means to the
same sample-weighted class composition for every category. These means equal
the category averages of the class-adjusted outcomes used in the CDFs.
Both figures use mean +/- 1.96 SD/sqrt(n) bar intervals, treating the adjusted
values as observed. Mean contrasts use class-clustered CR1 and t(63).
CDFs show outcomes minus their estimated class effect plus its sample-weighted
average. KS p-values use ordinary asymptotic two-sample tests, treating the
adjusted values as observed. No bootstrapping is used in either figure.
Figure 7's adjusted CDF values can exceed one. The former standalone class-FE
review script was folded into 07.

Figure 8 and Appendix Figures A10--A11 express effects in percentage points
for a one-SD increase in the indicated member's rationality. Each member's SD
is computed from the pooled 1,304 pair-wave sample. Stored estimates remain
per 0.1 units; `programs/plot_collective_ame.R` rescales the points and intervals
by SD/0.1. `11_3_collective_quality_plots.R` generates Figure 8 and the A10
multinomial-logit counterpart. A11 uses HM and RevMaxMPI in the same three LPM
specifications as Figure 8, estimated by `12_1_collective_quality_extensions.do`
and plotted by `12_2_collective_quality_plots.R`. Both LPM analyses reuse
`programs/collective_linear_ame.ado` and class-clustered t(63) intervals.

The collective-quality scripts retain only analyses used in `Overleaf/main_v3.tex`:

| Script | Current draft use |
| --- | --- |
| `11_collective_quality.do` | CEIV inputs, categorical appendix table and A10 multinomial-logit estimates |
| `11_2_collective_quality_linear.do` | Figure 8 LPM estimates |
| `11_3_collective_quality_plots.R` | Figure 8 and A10 panels |
| `12_1_collective_quality_extensions.do` | Covariate/Shapley discussion and A11 alternative-index estimates |
| `12_2_collective_quality_plots.R` | A11 panels and SDs reported in their notes |

The unused individual/wide CEIV panel copies, continuous appendix-table
regressions and covariate review plots are no longer generated. The group
CEIV panel remains an input to Table 1. Historical result files are retained.

New benchmark calculations belong to section 7 of `1_1_calculate_indices.R`
and `programs/build_placebo_donor_matrices.R`. The retained CCEI and RA files
are completed historical inputs, not resumable caches for that builder.
Data and CSV files remain subject to the repository's existing Git ignores.

The historical review scripts, pilots, estimates and notes were retired to
`../Archive/IminusM_review_retirement_2026-10-11/Code/IminusM_review/`.
The retirement record and file hashes are in that archive's
`MIGRATION_CHECKS.md` and `migration_manifest.tsv`. The old review exporter
is also archived; the manuscript generators perform their own asset copies.
