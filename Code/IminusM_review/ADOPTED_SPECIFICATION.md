# Adopted specification: manuscript update

Decision: Figure 2 uses I-M; regressions retain actual I and estimate the coefficient
on the measure-specific donor benchmark M freely. This is a linear control for M,
not a nonparametric adjustment. Updated captions carry `(Updated)`; revised Section 5
paragraphs and changed explanatory notes are red. The Section 4 regression equation
also has a red M term to keep the equation referenced by Section 5 consistent.

## Updated assets

- Table 1 and Figure A6: I-M summary statistics and higher/lower-CCEI histograms,
  using all 2,560 student-wave observations with defined actual distance. The
  wave-specific sample sizes remain 1,288 and 1,272.
- Figure 2: available I-M mean/CDF figures; means -0.033722 and 0.044688;
  higher-minus-lower difference -0.078410; 2,560 defined student-wave observations.
- Table 3: six I-on-rationality regressions controlling for M, with the original
  balanced 2,512-observation sample and 64 class clusters.
- Table 4: existing raw/M/I-M/M-controlled comparison retained; notes now correctly
  identify Panel D, rather than Panel A, as reproducing the main coefficients.
- Appendix Shapley figure: existing adjusted-model decomposition, including M as a
  separate block and excluding the outdated RA block.
- Appendix buffer and mover tables: rerun with I as outcome and M as a control.
- Appendix correlation table: existing baseline I-M review correlations.
- Appendix alternative-measure table: existing adjusted HM, MaxMPI, and RA regressions.
- Appendix CCEI ties-low table: rerun with M; no RA controls.

No estimation samples were changed. Main coefficient/SE output matches the existing
review's adjusted estimates to displayed precision. All newly fitted regressions assert
N=2,512 and 64 clusters. The review data and original raw-result assets are preserved;
new manuscript files have `_M` or `_IminusM` suffixes.

## Provisional results and retained diagnostics

HM uses 50 sampled donors and MaxMPI 20; full exact benchmark builds remain incomplete.
The MaxMPI review calculation caps individual cross-cost calculations at two seconds,
assigning unresolved donor distances 0.5 (2.0% of donor distances on average).
The updated alternative table and Section 5 prose explicitly disclose this limitation.
CCEI and RA use all 651 non-own donor pairs.

The available placebo-reassignment and disjoint-choice results remain raw-I diagnostics;
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
python3 IminusM_review/10_export_adopted_assets.py
Rscript IminusM_review/11_plot_adopted_shapley.R
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
retained review outputs. Review scripts 01--08 reproduce the comparison
analyses; scripts 10--11 export and plot the remaining review assets. Script
11 also reproduces Figure A6.

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
