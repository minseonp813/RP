Retired Code/results/new_indices on 2026-10-11

All 352 original files were preserved and their SHA-256 hashes verified during
the move. 49 active files moved to the standard project folders; the remaining
303 historical files are retained under Code/results/new_indices in this archive.
No active script depends on the retired directory.

Active destinations, relative to the project root:
  Code/data/collective_analysis_sample.dta
  Code/results/collective_quality/              estimates, diagnostics, tests
  Code/results/collective_quality/extensions/   Section 6 extension estimates
  Code/results/figures/collective_rationality/  current figures and review plots
  Code/12_1_collective_quality_extensions.do    former section6_xxx/analysis.do
  Code/12_2_collective_quality_plots.R          former section6_xxx/plots.R
  Code/Logs/12_1_collective_quality_extensions.log

The legacy figure6.ster and figure6_ame.csv are now joint_outcome_model.ster and
joint_outcome_ame.csv. Main generators 07_Figures_Main.R, 08_Tables_Appendix.do,
and 11_collective_quality.do, plus the relocated extension scripts, use the new
paths. Overleaf figure and table filenames were preserved.

migration_manifest.tsv maps every original path to its current location and
records its hash before source path updates or output regeneration. The two
relocated scripts were subsequently edited to use the new paths. Historical
files in this archive remain unchanged; their READMEs describe the old layout.

Current workflow documentation:
  ../../Code/results/collective_quality/README.txt

Verification after migration: all fifteen saved models loaded; Figures 6-8 and
the alternative-index appendix figure regenerated; data, estimates, and CSVs
matched the manifest hashes; the appendix table matched its previous bytes.
The draft compiled successfully, with its existing unresolved-reference and
citation warnings. No model refitting was needed for this path-only migration.
