# Review package: placebo-adjusted distance as the main outcome

This directory is an isolated analysis workspace for comparing three outcome
definitions without changing the manuscript or its current tables and figures:

1. the observed distance, `I`;
2. the placebo-adjusted distance, `I - M`;
3. the observed distance controlling flexibly for `M`.

All generated files are written under `outputs/`. The scripts read the existing
replication data and benchmark files but do not overwrite them.

## Order of execution

1. `01_build_ra_benchmark.R`
2. `02_ccei_ra_analysis.do`
3. `03_review_outputs.R`
4. `04a_build_review_benchmarks.R` (50-donor HM and 20-donor MaxMPI review builds; resumable)
5. `05_alternative_measure_analysis.do`
6. `07_hm_sample_stability.R`
7. `08_hm_sample_stability.do`

The review builds use 50 reproducibly sampled HM donors and 20 reproducibly
sampled MaxMPI donors per target and include split-half stability checks. Exact
full HM and MaxMPI builds are available in
`04_build_full_rp_benchmarks.R`; they are deliberately separated because each
requires 850,208 target-donor comparisons and some exact cross-cost problems are
computationally intensive.

For the MaxMPI review build, an individual cross-cost calculation that does not
finish within two seconds is recorded as undefined and receives the neutral
distance of one half. The generated diagnostics report the resulting undefined
share. This cap is only for deciding whether the specification is worth a full
exact build.

`outputs/benchmarks/hm/` contains the first resumable chunk of the exact
651-donor HM build. `outputs/checkpoints/` retains discarded uncapped MaxMPI
review chunks for provenance; none of those partial files enter the reported
results.
