# Table A5: full-benchmark checkpoint (2026-10-06)

Pre-edit scripts, manuscript/table sources, saved estimates and the wide actual-index panel are retained here. The current task calculates all 651 non-own same-wave donors for HM and MaxMPI, retains RA's existing 651-donor benchmark, and moves the six-specification Table A5 fits into 08_Tables_Appendix.do.

`checks/` retains the exact-kernel conformance, exhaustive small-graph, independent mixed-integer, sample, and cache-migration checks. The native-only implementation/configurations record the initial completed HM benchmark and seven valid MaxMPI chunks. Only implementation hashes were migrated after demonstrating equivalence; raw inputs, roster, donor count and time-cap settings did not change. MaxMPI uses a compiled branch search with an uncapped mixed-integer completion for difficult cases, rather than dropping unresolved searches.

The numerical reference calculate_rp_indices.R is unchanged. Production builders remain in Code/programs and outputs in Code/results/benchmarks. Manuscript edits are limited to the regenerated table fragment and the numerals cited in the existing main-text paragraph; its historical pilot note is retained under the user's wording restriction.


Completed: full matrices and wide-panel mappings passed all checks; all 18
Table A5 fits have the intended samples and 64 class clusters; all six RA
fits reproduce their previous full-benchmark results. The manuscript edit
changed only numerals in its Table A5 result paragraph. Two PDF passes
succeeded, and physical pages 29/69 passed visual QA. The old pilot note
remains unchanged under the user's wording restriction. No commit/push.
