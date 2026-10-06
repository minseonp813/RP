M integration baseline — 2026-10-06

This local archive preserves the pre-edit 01 entry script, RP/Risk-aversion
benchmark builders and launchers, and panel_final/panel_group/panel_individual
files. SHA256.json records each preserved file's original content hash.
These code copies are historical snapshots; their relative paths refer to the
original Code layout. The project excludes Archive and data files from Git.

The revised 01_calculate_indices.R calculates all four M benchmarks from its
balanced roster and choice data, writes new caches under results/benchmarks/,
and carries member-specific M values into panel_final.dta. Section 7 can be
run from Code after the actual-index checkpoint exists. 03 retains the M
columns, 04 separates waves, and 05 maps them to canonical member-level names.

Current benchmark outputs are retained at their original locations:
- CCEI: results/placebo_normalized/
- RA: IminusM_review/outputs/data/ra_placebo_member_wave.*
- HM: IminusM_review/outputs/benchmarks/hm_sample50/
- MaxMPI: IminusM_review/outputs/benchmarks/maxmpi_sample20/

Comparison must match records by group_id, post and id. Compare old versus new
CCEI and RA numerically before replacing manuscript inputs. HM and MaxMPI
current caches use 50 and 20 donors; the new full benchmarks use all 651 donors
and no solver time cap, so their values may change by design.

No full index or benchmark pipeline was run during this refactor. The live
panels, benchmark results, manuscript tables and figures were not regenerated.
