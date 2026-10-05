# Adopted specification: manuscript update

Decision: Figure 2 uses I-M; regressions retain actual I and estimate the coefficient
on the measure-specific donor benchmark M freely. This is a linear control for M,
not a nonparametric adjustment. Updated captions carry `(Updated)`; revised Section 5
paragraphs and changed explanatory notes are red. The Section 4 regression equation
also has a red M term to keep the equation referenced by Section 5 consistent.

## Updated assets

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

From Code, after the original review data/results have been generated:

```sh
/Applications/StataNow/StataMP.app/Contents/MacOS/stata-mp -b do IminusM_review/09_export_adopted_specification.do
python3 IminusM_review/10_export_adopted_assets.py
Rscript IminusM_review/11_plot_adopted_shapley.R
```

The Stata SUCCESS marker is in outputs/logs/adopted_specification.log.
The exporter reads the review CSVs and copies separate adopted assets to Overleaf.
It does not rewrite manuscript prose. The prior 01-08 review scripts still reproduce
the comparison analyses; 09-11 reproduce the adopted manuscript assets.

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
