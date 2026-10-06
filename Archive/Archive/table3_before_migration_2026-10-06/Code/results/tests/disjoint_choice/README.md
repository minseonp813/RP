# Table A7: disjoint-choice validation

Last updated: 2026-10-06.

Run from `Code`:

1. Run `programs/calculate_indices_disjoint.R` to calculate the split-choice inputs. It reads
   the balanced pair roster from `data/panel_final.dta` and choices from
   `base_raw.dta` and `end_raw.dta`, using the shared RP functions and donor
   benchmark builder. It does not estimate regressions.
2. Run the final **Table A7** section of `08_Tables_Appendix.do` to estimate,
   summarize, and export the six columns. The section can run on its own.

The default is 500 independent partitions, with both A-to-B and B-to-A fits,
so each column summarizes 1,000 coefficient estimates. `DISJOINT_REPS` can
reduce the count for a pilot, `DISJOINT_CORES` controls benchmark workers,
and `DISJOINT_OUTPUT_DIR` selects another cache directory (set the same
variable in R and Stata when changing it). Partial calculations resume;
changed choices, rosters, numerical routines or calculation code require a
new output directory. A complete repetition is saved atomically as
`indices/split_0001.dta`, etc.

For each direction, source-half individual choices determine the higher-CCEI
indicator, signed CCEI difference, and exact corner/equal-allocation shares.
Outcome-half choices determine actual distance and its benchmark M. The
benchmark uses every non-own donor pair in the same wave (651 in the current
sample); undefined donor distances receive 0.5. Both members are assigned
high in a CCEI tie. Risk-aversion controls are excluded. The benchmark builder
retains compact member summaries rather than large donor matrices for this
test; completed compact runs remove their temporary chunks.

The regressions are written directly in `08_Tables_Appendix.do`, matching the
adopted Table 3 specification in `IminusM_review/09_export_adopted_specification.do`.
The older Table 3 block in `06_Tables_Main.do` still omits M. Columns 1--3 use the higher-CCEI indicator;
columns 4--6 use the signed difference. Within each block, specifications are
baseline class effects, full controls with class effects, and full controls
with individual effects. All include M and cluster standard errors by class.
Each partition/direction starts with Table 3's balanced sample and retains
students with defined outcome-half distance in both waves, using a common
sample across all six columns.

Stata writes `disjoint_estimates.dta` and `disjoint_summary.dta` here. The
summary contains coefficient medians and 2.5th/97.5th percentiles across fits;
these ranges are not confidence intervals obtained by pooling partitions.
A complete 500-partition, 652-pair run writes
`results/tables/table_disjoint_choice.tex` and copies it to Overleaf. Pilot
runs write `table_disjoint_choice_pilot.tex` here and leave manuscript assets
alone. The draft uses the new export when present and retains its old table
and numerical discussion until then.

The previous R/Stata implementations and affected files are preserved in
`Archive/disjoint_choice_before_migration_2026-10-06/` for comparison. Their
coefficient caches are not inputs to the new analysis. The old Stata entry
script has been archived; regression and export code now lives in 08.

The full simulation was not run during migration. Checks used a three-pair,
two-partition choice fixture and separate synthetic estimation inputs. The
existing main and choice-buffer table exports were byte-identical after
moving the appendix regressions directly into 08.
