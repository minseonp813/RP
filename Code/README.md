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
| `3_Tables_Main.do` | Tables 1--3, their Figure 4 reference coefficients and Figure 8 LPM estimates |
| `4_Figures_Main.R` | Main Figures 3--8 only |
| `5_Tables_Appendix.do` | Tables A1--A7 and estimates for Figures A7/A10--A12 |
| `6_Figures_Appendix.R` | Figures A4--A8 and A10--A12 only |

The main scripts are locked to the active `Overleaf/main_v3.tex` tables and
figures. The main table script contains Tables 1--3 and the preparation/LPMs
needed by Figures 6--8. The main figure script generates only the six empirical
main figures below, with no appendix panels, legacy CEI tables, unadjusted
collective-CCEI plots or chat composites. Figures 1/2 are LaTeX drawings in the
draft. Figure 9 and both ML scripts are unchanged and remain a separate workflow.

| Main output | Locked definition |
| --- | --- |
| Table 1 | Student-/pair-wave summaries; distance is I-M; group CEIV comes from the supplied three-decimal values |
| Table 2 | Twelve baseline randomization regressions and the stacked-sample attrition comparisons, with class clustering |
| Table 3 | Six I-on-relative-CCEI specifications controlling for M; 2,560 rows and 64 classes; individual-FE columns retain one-wave students |
| Figure 3 | Class-adjusted I-M means/CDFs; ties assigned higher CCEI; class-clustered contrasts and ordinary KS tests |
| Figure 4 | 500 cyclic non-own pair reassignments within wave; M and covariates fixed; Table 3 Columns (2)/(3) |
| Figure 5 | Pooled I-M survey means; similarity panel restricts to RA disagreement at or above the pooled median |
| Figure 6 | CCEI/CEIV scatter with coincident-point counts; CEIV endpoint status uses the solver attainment flag |
| Figure 7 | Pooled-median pair categories; class-adjusted CCEI/CEIV means/CDFs, clustered contrasts and ordinary KS tests |
| Figure 8 | Four joint-outcome LPMs with Class FE, Class FE + controls and Pair FE + controls; one-SD effects and t(63) intervals |

Table 2 reuses `data/cog_pre_clean.dta`, prepared by `1_2`, so its math scores
follow the same first-row/completeness rule as the canonical analysis data.
The former Table 2 import sorted duplicate IDs only by completeness, leaving
two conflicting scores unresolved. The corrected math row is coefficient
-0.067, randomization p=0.080 and attrition p=0.395; the joint tests are
chi-squared(12)=11.77 (p=0.464) and F(12,62)=1.11 (p=0.369).
The existing draft table still reports the older values.

The lock was checked in temporary project copies. Tables 1/3 and twelve of
the thirteen main empirical panels reproduce the draft's displayed output.
Figure 4(a)'s existing PDF is stale: it labels the actual coefficient -0.077,
whereas Table 3 Column (2) is -0.081. Figure 4 now reads its actual coefficients
directly from `results/tables/table3_focal_estimates.csv`, exported by 3.
Both new placebo panels reproduce the pre-cleanup code's plotted output and
all 1,000 main coefficient draws match exactly. No draft assets were updated
during this validation; Table 2 and Figure 4(a) await a separate draft refresh.

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
| `99_9` | Backup of appendix regressions/decompositions and hold-out validation omitted from the active draft |
| `99_10` | Backup of appendix risk-distance, legacy Shapley and hold-out plots, plus review previews |

The former standalone mover and alternative-measure appendix scripts were
retired; `5_Tables_Appendix.do` generates the current Tables A3 and A5 directly.
Loose logs are stored in `Logs`. Batch logs that shared a name with an existing
analysis log have a `_batch` or `_project_root` suffix. Historical logs and saved
hash records retain the script names used when those analyses ran. The cleanup
record is in `../Archive/code_cleanup_2026-10-11/README.txt`.

Table A5 uses full HM/MaxMPI benchmarks in `results/benchmarks/{hm,maxmpi}`
and the retained RA benchmark in
`results/benchmarks/retained_ra/ra_placebo_member_wave.dta`. Its RA donor
matrix is retained in that directory for provenance. Figure 4 and A8 use the same retained donor matrix in
`results/placebo_normalized/placebo_donor_matrix.csv`, through
`programs/estimate_table3_placebo.R`. The main run estimates Columns (2)/(3);
the appendix run estimates Columns (5)/(6), using the same seed and assignments.

Run 3 before 4 for the main outputs, and 3/5 before 6 for the appendix.
Sections define their own paths and can be run independently from `Code`.
They write canonical assets to `results/tables` and `results/figures` and copy
manuscript assets to `../Overleaf`. For Figure A7, run its section in 5 before
its plotter in 6. Figure A8 estimates its own 500 reassignments in 6; it reads
the Table 3 reference coefficients from 3 and does not require running 4.
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
review script was folded into 4. Its former chat composites are no longer generated
by the main manuscript script.

Figure 8 and Appendix Figures A11--A12 express effects in percentage points
for a one-SD increase in the indicated member's rationality. Each member's SD
is computed from the pooled 1,304 pair-wave sample. Stored estimates remain
per 0.1 units; `programs/plot_collective_ame.R` rescales the points and intervals
by SD/0.1. `4_Figures_Main.R` generates Figure 8; `6_Figures_Appendix.R`
generates the A11 multinomial-logit counterpart. A12 uses HM and RevMaxMPI in
the same three LPM specifications as Figure 8, estimated by
`5_Tables_Appendix.do` and plotted by `6_Figures_Appendix.R`. Both LPM analyses reuse
`programs/collective_linear_ame.ado` and class-clustered t(63) intervals.

Collective-quality analysis is integrated into 3--6:

| Script | Current draft use |
| --- | --- |
| `3_Tables_Main.do` | Shared CEIV/pair-wave sample and Figure 8 LPMs |
| `4_Figures_Main.R` | Figures 6--8 |
| `5_Tables_Appendix.do` | Table A7, A10 R2 decompositions, A11 multinomial estimates and A12 alternative-index estimates |
| `6_Figures_Appendix.R` | A10--A12 panels and alternative-index SDs |
| `programs/build_collective_analysis_sample.do` | Shared CEIV inputs and pair-wave sample, called by 3 and 5 |

The collective sections in 3 and 5 prepare their own sample and define their
own locals; each can run independently from `Code`. The A10, A11 and A12
plotting sections in 6 likewise define their own paths. Model and plot helpers
remain in `programs`. Figure 6 checks outcome shares against the main LPM export,
so running the main workflow has no dependency on appendix estimates. The old
covariate-AME export is retained as a historical result and is no longer generated.

Figure A10 decomposes the overall R-squared of the four joint-outcome LPMs
under Class FE + controls and Pair FE + controls. The exact Shorrocks--Shapley
calculation includes fixed effects as one of five blocks and evaluates all
32 coalitions on the same sample. 5 writes decomposition, full-model and
coalition CSVs to `results/collective_quality/extensions`; 6 generates
`joint_outcome_shapley.pdf` and copies only that manuscript panel to Overleaf.

The appendix scripts are locked to the active, uncommented draft. 5 keeps
Tables A1--A7 and estimates for Figures A7/A10--A12. 6 writes nineteen figure
assets: eighteen panels plus the A7 legend. Figures A1--A3 are photographs or
LaTeX drawings, and Figure A9 is generated by the separate `99_6` simulation.

`99_9_Tables_Appendix_Backup.do` retains the alternative ties-low regressions,
legacy collective-index/c_Ng regressions and Shapley decompositions, and the
leave-one-choice-out section. `99_10_Figures_Appendix_Backup.R` retains the
unused risk-distance figures, historical Shapley plots, hold-out review and
A7/A8/A10 review PNGs. Both run from `Code` and write review results without
copying assets to Overleaf. Run 5/6 before 99_10 for the current estimates,
and 99_9 for the historical Shapley exports. The hold-out input builder remains
in `programs/calculate_indices_disjoint.R` and now points to 99_9.
The R backup completed its smoke run; its optional legacy Shapley plots require
`ggpattern`, which is not installed here. The Stata backup's alternative tables
and initial legacy decompositions were smoke-tested; the remaining expensive
archival models and 18-fold hold-out were not rebuilt. Their code is preserved.

The appendix cleanup was checked in temporary project copies. All seven table
fragments reproduce the draft's numbers and formatting (whitespace aside);
the correlation-table generator now includes its draft's top horizontal rule.
All nineteen retained figure assets reproduce the fresh pre-cleanup run pixel
for pixel, and all 1,000 A8 coefficient draws match exactly. The exact R2,
LPM, alternative-index, SD and other table exports match byte-for-byte.
Multinomial AME interval exports differ by at most 1.1e-10, with unchanged point
estimates and interval signs. Historical CDF PNGs show small rendering differences
from both fresh runs; multinomial PNG differences are limited to 217/314 pixels.

Figure A8(a)'s existing draft PDF is stale: its reference is -0.228, while Table 3
Column (5) and the current code give -0.227. The code reads that reference directly
from Table 3's exported coefficients. The draft's A8 note also calls Columns
(5)/(6) higher-CCEI indicators; those columns and both plot axes use the CCEI
difference. These draft assets/notes were left unchanged. Figure A10 is now the
joint-outcome R2 decomposition, A11 the multinomial model and A12 the alternative
indices; code headings follow that current order. Historical records retain the
figure numbers used when they were written.

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
