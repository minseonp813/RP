# Code lock-in for Tables A5/A6 and Figures 4/A8 (2026-10-10)

This is a code-only update; the analysis was not rerun and the draft and saved
manuscript assets were left unchanged. The earlier build descriptions below
refer to saved results and are superseded where they conflict with this update.

`08_Tables_Appendix.do` now places Table A5 immediately after A4 and Table A6
immediately after A5. Both use Table 3's 2,560 defined-distance observations
and 64 class clusters, retaining 48 single-wave students under individual FE.
A5's HM/MaxMPI/RA outcomes, full 651-donor benchmarks, rationality definitions,
controls, panel summaries, and coefficient/SE formatting match the current
source tables. A6 assigns both members zero in a CCEI tie and repeats the three
higher-CCEI specifications with M and the same controls and fixed effects.

Per the user's sample decision, the next run of the Figure 4 section in
`07_Figures_Main.R` will use the full 2,560-observation Table 3 sample rather
than the previously saved 2,512-observation balanced sample. The 500 draws,
seed 20260812, same-wave cyclic donor reassignment across all classes, shared
donor within each pair, undefined-donor value of 0.5, fixed M and covariates,
and six specifications are retained. Individual FE explicitly keep single-wave
students. Class-clustered inference uses the reghdfe-compatible small-sample
correction. Actual-choice reference lines are refitted on this same sample.
The existing plotted distributions and red lines therefore remain the old
results until this section is rerun.

Figure 4 exports only Columns (2)/(3), immediately after Figure 3. Figure A8
exports Columns (5)/(6), immediately after Figure A7 in `09_Figures_Appendix.R`,
using the same saved six-specification draws and actual coefficients. The
shared display is `programs/plot_table3_placebo.R`; it rejects old 2,512-row
inputs. Run the Figure 4 section before the A8 section. Existing manuscript
PDF filenames are preserved; the separate two-panel review images are
`figure4_placebo_coefficients.png` and `figure_A8_placebo_coefficients.png`.
The optional asset exporter also follows this draft order.

One manuscript-only discrepancy is intentionally untouched: A8's note calls
its regressor the higher-CCEI indicator, although Columns (5)/(6) and its
axes use the signed CCEI difference. The code retains the signed difference.

# Current ToDo update (2026-10-10)

Appendix update (2026-10-10): Tables A1 and A3 and Figure A7 now use
Table 3's full 2,560-observation defined-distance sample and 64 classes.
They retain the 48 singleton students whenever individual FE enter,
including every A7 coalition. A1's no-buffer panel and A7's full models
match the updated Table 3 coefficients, SEs and R-squared within 1e-8.
A7 rationality shares for columns 2, 3, 5, 6 are 25.633%, 9.099%,
33.131% and 11.527%, respectively. Figure 4 and other appendix analyses
keep their existing samples until separately revised.

A1/A3 now mirror Table 3's latest header/summary labels and note wording.
The header mean/SD are calculated from the fits, not hard-coded. A1
keeps three buffer panels, panel-specific R-squared and a shared footer.
The stable Gram-form class-cluster covariance calculation, previously
embedded in the hold-out section, is shared in programs/hdfe_cluster_vce.do.
All six mover models match independent NumPy estimates/covariance within
1.1e-14 for coefficients and 8e-16 for SEs. Pair symmetry fixes Column
(4)'s gap-by-mover interaction at zero; the production export omits its
SE/test rather than reporting a spurious test of floating-point roundoff.
The hold-out caller retains the same numerical covariance formula.

Table 3 sample update (2026-10-10): the main table now uses all 2,560
student-waves with defined Ihat_ig, matching Figure 3's observation IDs.
There are 1,304 students, 652 pairs, 64 classes, 1,288 baseline observations,
and 1,272 endline observations. All six columns use the same sample.
Individual-FE columns explicitly retain 48 singleton students; their
observations supply no within-student variation. The exact-choice controls
and 651-donor M benchmark are unchanged. The shared balanced_t3 flag is
preserved for existing companion analyses. At the time of the main-table update, Figure A7, Figure 4, and the
appendix extensions still used their previously computed balanced samples;
these do not reproduce the newly updated Table 3 coefficients or R-squared.
The notes and discussion now make this distinction explicit.

Higher-CCEI coefficients are -0.096, -0.081, and -0.090; CCEI-gap coefficients
are -0.249, -0.227, and -0.214 (three-decimal display). All are significant
at 1%. Dependent-variable mean is 0.500 and SD is 0.3208968. There are 179
tied pair-waves (14.0% of 1,280). Among the 1,256 students observed in both
waves, 42.2% switch higher-CCEI status. The Column (6) standardized effect
is -0.117716, displayed as -0.118. All six e(sample) masks are asserted equal
to the defined-distance mask and N=2,560 with 64 class clusters.
The rebuilt 94-page PDF was checked on physical page 27; Table 3 and its
notes fit cleanly, with no overfull boxes. The verified PDF is saved at
output/pdf/main_v3.pdf, without overwriting the live Overleaf PDF.

This update supersedes historical placebo and hold-out descriptions below.
Figure 4 now uses all 652 pairs within each wave across all classes, with
500 cyclic reassignments and all six fits saved; the 2x2 display shows Table 3
columns 2, 3, 5, and 6. Its actual coefficients lie below each displayed
placebo distribution's 2.5th percentile. M, the sample (2,512), controls,
and 64-class inference are preserved. The coefficient draws, summaries,
and run metadata are in `results/figures/figure4_placebo_*`.

Figure 3 now uses class-level inference for the same 2,560 student-wave
observations in 64 classes. Panel (a) uses unadjusted OLS with two category
means, CR1 class-clustered covariance, and t(63) confidence intervals and
mean-difference inference. The higher-minus-lower difference remains
-0.078409994 (SE 0.012906958, p 7.90954e-8). Individual mean SEs are
0.007327848 (Lower) and 0.005606389 (Higher).

Panel (b) retains D=0.145703420 but replaces nominal iid inference with
9,999 centered pairs-bootstrap draws of whole classes (seed 20260812).
Both member categories and waves share each class resampling weight;
CDFs retain observation weighting and account for the resampled category
sizes. Each draw subtracts the observed CDF difference before taking the
absolute supremum, approximating the equality null. Zero draws exceed the
observed D, so the plus-one Monte Carlo p-value is 0.0001, displayed as
p < 0.001. Constructed distances and classifications are held fixed.
Statistics and draws are saved as `results/figures/ccei_IminusM_by_higher_ccei_stats.csv`
and `results/figures/figure3_cluster_KS_draws.csv`. The nominal iid results
in the historical review files are superseded for the current Figure 3.

Validation: means, sample sizes and D are unchanged; mean SEs and difference
inference match an independent Stata cluster regression within 1e-9, and
an independent CR1 sandwich within 1e-14. All 99 small-fixture bootstrap
draws match literal whole-class resampling, including ties and unequal
group sizes; all 9,999 production draws are finite. Figure notes and
nearby discussion now identify clustered inference. The rebuilt 94-page
draft has no overfull boxes; Figure 3 was visually checked on physical
page 25, retaining the user's revised captions and placement.

Table 3 and the alternative-measure appendix table now include outcome means
and SDs. The separate alternative-index summary table was removed. The latter
table's notes now describe the full 651-donor, uncapped benchmarks rather than
the historical pilot. Leave-one-choice-out validation replaces the former
split-choice analysis; see `results/tests/disjoint_choice/README.md` for the
production design, inputs, estimates, and diagnostics.

Appendix Tables A1 (choice buffers) and A3 (mover status) now report
Dependent variable mean and Dependent variable SD. A1 reports R-squared
within each panel and exports its common sample/mean/SD/control footer
from code. Table 3 and A5 use the same labels as the manually revised draft.
All affected tables were regenerated from 06/08; displayed coefficient and
standard-error rows match the prior draft. The common CCEI-distance sample
has N=2,512, mean 0.500, and SD 0.320042943. A1 and A3 were
visually checked on physical pages 70 and 72 of the rebuilt 94-page draft;
there are no overfull boxes. A separate temporary TeX output directory
was used for compilation to avoid interfering with the live draft build.

The collective-outcomes categorical appendix table now omits Low-High and
reports Low-Low/High-High directly, without post-estimation rows. An appendix
Figure 6 checks exact and 2.5-point-buffer exclusions using all collective
choices. Figure 8's math/friendship version is saved for review, not inserted
into the manuscript. Figure A7's four-column decomposition was adopted on
2026-10-10: separate vector panels for Table 3 columns 2, 3, 5, and 6 are
assembled with subfigure environments. Its overall-R-squared decomposition
treats fixed effects as a Shapley block; the cited discussion is updated.
CCEI is blue, individual/friendship controls translucent solid red,
corner/midpoint shares diagonally hatched red, placebo M light gray, and
fixed effects gray, ordered from top to bottom as requested.

Completed verification: all 18 hold-out folds and 108 fits are saved. Independent
R estimates match the focal coefficients and class-clustered SEs within 2.1e-15
and 3.7e-16, respectively, including the stable covariance calculation in 08.
The hold-out result is weaker and less stable than Table 3: median N is 662
(range 584-768), every descriptive fold range includes zero, and the higher-CCEI
individual-FE median is positive. The revised text reports this limitation.

The A7 adoption was verified on physical page 56 on 2026-10-10. The draft
compiles with resolved A7 references, unchanged decomposition inputs, and
no overfull boxes; the four panel PDFs and shared legend match their
manuscript copies. The former across/within plotting and estimation
sections are archived in `Archive/figure_A7_adoption_2026-10-10`.

The rebuilt draft has 94 pages. Updated physical pages 27, 30-32, 35, 69,
74-75, and 77 were inspected; the appendix layout on pages 58-59 was corrected
to prevent a footer overlap. There are no overfull boxes. Existing unresolved
references elsewhere in the draft remain outside this task. The verified PDF is `output/pdf/main_v3.pdf`;
`Overleaf/main_v3.tex` is the manuscript source.

The sections below document earlier decisions and builds; their numerical
and implementation descriptions are historical where superseded above.

# Adopted specification: manuscript update

Decision: Figure 3 uses I-M; regressions retain actual I and estimate the coefficient
on the measure-specific donor benchmark M freely. This is a linear control for M,
not a nonparametric adjustment. Updated captions carry `(Updated)`; revised Section 5
paragraphs and changed explanatory notes are red. The Section 4 regression equation
also has a red M term to keep the equation referenced by Section 5 consistent.

## Updated assets

- Table 1 and Figure A6: I-M summary statistics and higher/lower-CCEI histograms,
  using all 2,560 student-wave observations with defined actual distance. The
  wave-specific sample sizes remain 1,288 and 1,272.
- Figure 3: available I-M mean/CDF figures; means -0.033722 and 0.044688;
  higher-minus-lower difference -0.078410; 2,560 defined student-wave observations.
- Table 3: six I-on-rationality regressions controlling for M, with the original
  balanced 2,512-observation sample and 64 class clusters.
- Table 4: existing raw/M/I-M/M-controlled comparison retained; notes now correctly
  identify Panel D, rather than Panel A, as reproducing the main coefficients.
- Appendix Shapley figure: existing adjusted-model decomposition, including M as a
  separate block and excluding the outdated RA block.
- Appendix buffer and mover tables: rerun with I as outcome and M as a control.
- Appendix correlation table: existing baseline I-M review correlations.
- Appendix alternative-measure table: full 651-donor HM/MaxMPI benchmarks and the retained full RA benchmark; six Table 3 specifications are now generated by 08_Tables_Appendix.do (completed 2026-10-06).
- Appendix CCEI ties-low table: rerun with M; no RA controls.

No estimation samples were changed. Main coefficient/SE output matches the existing
review's adjusted estimates to displayed precision. All newly fitted regressions assert
N=2,512 and 64 clusters. The review data and original raw-result assets are preserved;
new manuscript files have `_M` or `_IminusM` suffixes.

## Provisional results and retained diagnostics

Historical Table A5 review outputs used 50 sampled HM donors and 20 MaxMPI donors,
with a two-second MaxMPI cross-cost cap and unresolved distances set to 0.5.
The 2026-10-06 update replaced these pilot inputs with full 651-donor,
uncapped benchmarks generated by section 7 of 1_1_calculate_indices.R. CCEI
and RA already use all 651 non-own donor pairs. The manuscript wording was
retained at the user's request; its old pilot note must be revised
separately after the numerical update.

The retained regression-based placebo-reassignment and disjoint-choice results remain raw-I diagnostics;
references that implied equality to the new M-controlled regressions were corrected.
Raw survey-validation and simulation results also remain, explicitly distinguished from
validation of I-M. Alternative ties-low and additional Shapley figures are not marked
Updated; notes state that the benchmark-controlled counterparts are not yet available.
The older disjoint-choice tie/vintage TODO already in the draft remains unresolved.

## Reproduce adopted assets

Updated 2026-10-06: the adopted Table 3 is generated directly by
`06_Tables_Main.do`; its mover and ties-low extensions are generated directly
by `08_Tables_Appendix.do`. Their regression specifications are written in
those dofiles. The former review estimation script is archived in
`../Archive/table3_before_migration_2026-10-06/` and removed from active code.

From Code, after the retained review data/results have been generated:

```sh
/Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 06_Tables_Main.do
/Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do 08_Tables_Appendix.do
Rscript 07_Figures_Main.R
Rscript 09_Figures_Appendix.R
python3 IminusM_review/10_export_adopted_assets.py
```

The Table 3 SUCCESS marker is in `../Logs/06_table3.log` relative to this
folder; the appendix extension marker is in `../Logs/08_table3_extensions.log`.
The migrated table sections can run independently from Code.

06 writes `results/tables/table_bargainingCCEI_M.tex` and the Table 1 summary
and copies both to Overleaf. Table 3 uses actual I, controls for the retained
full 651-donor M, and preserves the 2,512-observation/64-class sample, six
columns, exact-choice shares, and exclusion of risk-aversion controls.

08 writes the current mover, ties-low, and choice-buffer table fragments to
`results/tables` and copies them to Overleaf. The mover/ties-low section
retains the validated analysis panel's observation order, which matters for
covariance arithmetic in the symmetry-constrained mover specification.

The exporter reads these canonical main/appendix table fragments rather than
older copies under `outputs/adopted`; other review assets still come from the
retained review outputs. The remaining review analysis scripts reproduce the
comparison analyses from retained benchmark outputs; the obsolete benchmark
launchers 01, 04 and 04a have been removed. Script 10 exports remaining review
tables and canonical Figures 3, 4, A6 and A7 generated by 07/09. The former
plotter 11 is archived. Figures 4 and A7, including their captions, notes and
related result text, were added to the manuscript on 2026-10-06.

The migrated Table 3, mover and ties-low fragments were checked against the
current manuscript and matched byte for byte. No donor benchmarks were
recalculated. The old review outputs and saved main estimates remain for
comparison; no active code consumes those saved `.ster` files.

## PDF build

The existing main_v3.bbl was reused: citation keys were not changed, and the local
Biber invocation failed. Two successful pdflatex passes resolve the edited references.
Pre-existing duplicate eq:collective_objective and unresolved references outside
Section 5 (tab:collective_ccei2, tab:collective_fgarp, tab:collective_rev_max_mpi,
rem:largerN, rem:othermeasures) were not edited in this task.

## Final verification

Nine updated figure/table captions were located in the compiled PDF and all nine
pages were rendered and visually checked (pages 22, 23, 30, 64, 86, 87, 88, 90, 96).
No overfull boxes remain in the final build. Original LaTeX label keys are unchanged.
The final PDF is available as both Overleaf/main_v3.pdf and output/pdf/main_v3.pdf.

## Figure A7 and Figure 4 manuscript revisions (2026-10-06)

Updated 2026-10-06: 08 now calculates the Figure A7 inputs using Table 3
Column (3), and 09 draws the two panels. Across-individual fit (individual
effects entered first) accounts for 83.5% of total explained variation;
additional within-individual fit accounts for 16.5%. Within explained
variation, Higher CCEI contributes 28.1%, M contributes 67.4%, individual/
friendship controls 2.3%, and corner/midpoint shares 2.2%. These shares use
a different denominator and conditioning order from the older all-block
Shapley decomposition described above; they cannot replace those old numbers
without revising the figure note. The manuscript now uses the revised
conditioning order and denominators in both the note and result text.

07 generates the current Figure 3 and a single-donor version for Figure 4.
It samples one of all 651 foreign same-wave donors per target pair-wave,
with replacement and a fixed seed (20260812); both members share the donor.
Undefined donor distances receive 0.5. Both figures use the same 2,560
student-waves with defined actual distance and plot distance minus M.
Figure 4's higher-minus-lower gap is 0.008324 (Welch p=0.354; KS p=0.106),
compared with Figure 3's actual gap of -0.078410. Since M is the donor mean,
a zero expected adjusted-donor gap follows from the benchmark construction.
The approved images have been copied into the manuscript, with updated captions,
notes and related result text. Figure labels are unchanged.

Manuscript verification (2026-10-06): two pdflatex passes succeeded, with no
overfull boxes. Figures 4 and A7 retain their original labels and numbering.
Updated physical PDF pages 28, 30, 31 and 53 were rendered and visually checked.
Existing unresolved citations/references elsewhere in the draft remain.
The current PDF is available in Overleaf/main_v3.pdf and output/pdf/main_v3.pdf.


## Figure 4 restored to regression distributions (2026-10-06)

Superseding the single-draw mean/CDF version described above, `07_Figures_Main.R`
now runs 500 random cyclic donor reassignments within class and wave, following
`99_6_placebo_reassignment.R`. Each donor is used once per class-wave; own pairs
are excluded, and both members share one donor. Undefined donor distance receives
0.5. All six Table 3 specifications use reassigned raw distance as the outcome,
control for the retained full 651-donor M, retain Table 3's balanced N=2,512 sample
and 64 class clusters, use exact-choice shares, and exclude RA controls.

The six actual-choice coefficients, clustered SEs, and M coefficients reproduce
Table 3's adjusted estimates (coefficient differences below 1e-8; SE/M differences
below 1e-7). All 3,000 placebo fits completed. All six actual coefficients lie below
the 2.5th percentile of the corresponding placebo distribution. The actual
coefficients are -0.092003, -0.077296, -0.090458, -0.249094, -0.228260, and -0.214260.

The canonical figure is `results/figures/figure4_placebo_coefficients.png`;
coefficient draws, summaries, and run settings are saved as CSV files beside it.
The validation/progress log is `Logs/07_figure4_regressions.log`. The exporter and
`main_v3.tex` now reference this figure, and its caption, notes, and result text
have been updated. The seed remains 20260812. Older raw-I appendix placebo results
and the old single-draw files are retained but are not used for current Figure 4.


## Table A5 full benchmarks (2026-10-06)

Section 7 of `1_1_calculate_indices.R` completed the HM/MaxMPI benchmarks with
all 651 non-own same-wave donors and no solver time cap. The shared builder
uses compiled donor kernels and an uncapped HiGHS completion for difficult
MaxMPI problems, with 1e-10 optimality checks in money-pump units. Each matrix has
850,208 rows including 1,304 validated own pairs; each member summary has
2,608 rows. Original actual indices and all other wide-panel values were
preserved. Numerical, independent mixed-integer, exhaustive small-graph,
completed-cost, and donor-scheduling validations are archived with the
pre-update checkpoint under `Archive/table_A5_full_benchmarks_2026-10-06/`.

`08_Tables_Appendix.do` now directly estimates the six Table 3 specifications
for each alternative measure. HM/MaxMPI retain N=2,512; RA retains N=2,604
and four individual-FE singletons. All fits have 64 class clusters. The six
RA fits reproduce the previous full-benchmark results. The canonical table
and detailed estimates are `results/tables/table_bargaining_alternatives_M.tex`
and `.csv`; the review exporter copies this table rather than old pilot fits.

The main-text numeric citations are -0.071/-0.078 for Column (3), 29.1%/26.7%
of outcome SDs, and 0.121/0.169 standardized effects for Column (6). All four
are significant at 5%; the HM gap p-value is 0.011008. Only numerals changed
in the existing paragraph. The obsolete 50/20-donor/time-cap note remains
unchanged under the user's instruction to leave draft wording untouched.


PDF verification: two pdflatex passes succeeded (90 pages), with no overfull
boxes. Physical pages 29 and 69 were rendered and visually checked; table
coefficients, labels, standard errors, stars and updated main-text numerals
are legible and aligned. Existing unresolved references/citations elsewhere
in the draft remain. Output is saved in Overleaf/main_v3.pdf and
output/pdf/main_v3.pdf. No commit or push was performed in this task.

Latest formatting refinement: mean/SD appear once beneath the header, with no repeated dependent-variable summary rows in the Table 3/A1/A3 footers. Main and appendix code exports preserve this choice; A1 panel separators follow the hand-tuned draft.

The final 94-page draft was built from a temporary TeX snapshot to preserve concurrent manuscript edits. Figure A7 and Tables A1/A3 were inspected on physical pages 56, 70, and 72. Both tables and their notes fit on one page; A1 remains beneath the appendix-table heading. The verified PDF is saved to output/pdf/main_v3.pdf.

## Table A5 sample alignment (2026-10-10)

All three panels now use Table 3's exact 2,560 student-wave observations
with defined CCEI-based distance. All alternative outcomes are defined on
this sample. Individual fixed effects retain the same 48 single-wave
students with `keepsingletons`; each of the 18 fits asserts that its
`e(sample)` equals the Table 3 availability indicator and has 64 class
clusters. The donor benchmarks and control specifications are unchanged.

The canonical table and coefficient CSV were regenerated by the standalone
A5 section of `08_Tables_Appendix.do`. Column (3) coefficients are
-0.070605 (HM) and -0.077653 (RevMaxMPI), or 29.0% and 26.5% of outcome
SDs on the common sample. Column (6) standardized effects are -0.120018
and -0.166517; both remain significant at 5%. The draft table, sample
note, and main-text effect sizes reflect this run. Sample sizes
and validation results under earlier dated headings are historical.
