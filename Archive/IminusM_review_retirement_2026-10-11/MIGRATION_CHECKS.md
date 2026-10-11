# Retirement of IminusM_review, 2026-10-11

## Consolidation into the canonical individual panel

The final design uses `Code/data/panel_individual.dta` for all individual
and distance analyses. Its existing prepared values were promoted without
recalculating any benchmark. `2_3_build_panel_individual.R` now constructs
the exact-choice controls, sample flags and `Istar_ccei` during future runs,
using `M_ccei` carried from `1_1`. The new section matches all 508 canonical
columns exactly, including the original 494 columns and observation order.
The unused `I_ccei_cached` validation column was removed.

The intermediate `2_4` script, the former Table 3 control helper and the
separate `distance_analysis.dta` are preserved in `intermediate_preparation/`,
along with the individual panel before consolidation. Active consumers load
the canonical panel directly; duplicate raw-share preparation and benchmark
self-merges were removed. Historical taking-turns and placebo routines retain
their original precision conventions.

Focused checks after consolidation reproduced all seven manuscript table
fragments and the checked figure images byte-for-byte. Figure A7's numerical
exports differ only by floating-point rounding (at most 2.85e-14 percentage
points). All 147 compared collective-analysis columns match exactly on 1,304
pair-waves, and 60 existing affected canonical/Overleaf assets retain their
original hashes. The isolated checks, logs and comparisons are preserved in
`validation/consolidation/`.

The dated retirement checks below document the earlier intermediate layout.
This consolidation supersedes their active filenames and preparation sequence.
The full run starting at `1_1` is deferred until the user finishes the other
draft work later today. The consolidation remains uncommitted.

## Original retirement record

The pre-retirement workspace was committed and pushed as GitHub commit
`eefd3f6d` on `snapshot/local-2026-09-17`. The Overleaf checkpoint is
`42dadc5`, including the clean merge of remote text edits.

The entire former `Code/IminusM_review` directory is preserved under
`Code/IminusM_review` in this archive. `migration_manifest.tsv` records the
original path, archive path and SHA-256 of every file. All 383 archived files
matched their original hashes immediately after relocation. Historical scripts
and notes are preserved as written; their paths and sample descriptions are
not instructions for the active production pipeline.

## Active replacements

- `Code/2_4_build_distance_panel.do` rebuilds
  `Code/data/distance_analysis.dta` from the current individual panel,
  the existing exact-choice control helper and the retained CCEI benchmark.
  It does not estimate the historical review models.
- `Code/results/benchmarks/retained_ra/ra_placebo_member_wave.dta` and
  `ra_donor_matrix.csv` are unchanged copies of the original RA inputs.
- Scripts 06-09 read the new paths. The mover section no longer asserts the
  unused historical `sample_ccei` alias; its current 2,560-row sample checks
  remain in place.
- Review scripts 02, 03, 05, 06, 07, 08 and the redundant exporter 10 are
  archived with the pilot outputs, partial caches, old tables and figures.

## Verification

The extracted production sections were run before and after migration in
isolated directories. Both Stata runs completed without errors. The checks
passed as follows:

- All 509 retained analysis columns match the former panel exactly on all
  2,608 rows, including observation order and keys. Forty unused review-only
  columns were removed. The balanced flag and exact-choice controls match.
- The seven LaTeX fragments for Tables 1, 3, A1, A2, A3, A5 and A6 are
  byte-for-byte identical. All 45 affected table regressions passed their
  existing sample and cluster assertions. Table A5's 18 coefficient-export
  rows also match byte-for-byte.
- Figure A7's four five-block decompositions, including its 128 coalitions,
  passed their existing assertions. The resulting CSV is byte-for-byte
  identical before and after migration.
- Figure 3's statistics and 9,999 class-bootstrap draws, both Figure 3
  images, both Figure 5 images and the Figure A6 histogram are byte-for-byte
  identical. The appendix CCEI/RA CDF images also match.
- The full donor-matrix checks and all six actual Figure 4 reference fits
  passed and produced identical coefficients and standard errors. The 500
  placebo reassignments were not rerun: their generator is unchanged and all
  of its retained input values, keys and row order match exactly. Figure A8
  continues to use the same saved placebo distributions.
- The relocated RA summary and donor matrix match their archived originals'
  SHA-256 hashes. No donor benchmarks were recalculated.
- Both R generators parse successfully. No executable file under active
  `Code` references the retired folder.
- Fifty existing distance-related canonical and manuscript assets retained
  their pre-migration SHA-256 hashes. The verification runs did not write to
  the production results or Overleaf directories.

`validation/summary.txt` records the original comparisons. The Stata logs are
retained under `validation/`; the intermediate preparation log is preserved in
`intermediate_preparation/02_distance_panel.log`.

The cleanup is separate from the pushed checkpoint and remains uncommitted.
Unrelated edits made elsewhere in the shared workspace during this migration
were preserved.
