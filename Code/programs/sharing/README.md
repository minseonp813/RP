# Table 1 / Figure 2 under three inconsistency measures

Three self-contained scripts regenerate Table 1 and Figure 2 of the paper from the raw choice
data, each with a different measure attached to the cross violations of a pair's merged dataset:

| script | measure | direction | runtime (Apple silicon, R 4.6) |
|---|---|---|---|
| `20_table1_figure2_ccei.R` | CCEI (the paper's baseline; reproduces the published Table 1 and Figure 2) | higher = more rational | ~20 s |
| `21_table1_figure2_maxmpi.R` | maximum money-pump index, computed under `p.x = 1` (appendix `def:app_mp`) | lower = more rational | ~1.5 min |
| `22_table1_figure2_hm.R` | Houtman–Maks **count** of observations to delete (appendix `def:app_hm`) | lower = more rational | ~3–4 min |

All three source `programs/rp_cross_costs.R`, which holds every algorithm and is documented
function by function. The theory that justifies replacing the CCEI by the other two measures —
and shows that the *fraction* form of Houtman–Maks and the min/mean/median money pump do **not**
work — is in the paper's appendix (subsection *Other Rationality Measures*).

## How to run

```
cd sharing
Rscript 20_table1_figure2_ccei.R
Rscript 21_table1_figure2_maxmpi.R
Rscript 22_table1_figure2_hm.R
```
or open a script in RStudio and *Source* it (the working directory is set automatically).
Inputs: `data/base_raw.dta`, `data/end_raw.dta`. Outputs go to `out_measures/`.

**Platform.** `.R-library` contains macOS arm64 binaries built under R 4.6 (`haven`, `ggplot2`,
`igraph`). On any other platform or R version, delete or rename that folder and install those
three packages; `rstudioapi` is optional. No external solver is needed.

## Outputs (`out_measures/`, prefixed `ccei_`, `maxmpi_`, `hm_`)

| file | content |
|---|---|
| `*_pairwave.csv` | one row per pair-wave (1,304): `c_mover`, `c_nonmover`, `c_N` (cross costs of D^ig, D^jg, D^Ng); `I_mover` (index of the mover; the non-mover's is `1 - I_mover`); `high_mover` (1 if the mover is classified as more rational); `score_1`, `score_2`, `score_g`, `score_1g`, `score_2g` (measure of each member, the group, and each member merged with the group — the last two only break ties); `ra_*` (risk aversion); `certified`, `exhausted` (money-pump search flags, NA for the other measures) |
| `*_table1.csv`, `*_table1.tex` | Table 1 rows (the `.tex` body can be `\input` into the paper's tabular) |
| `*_figure2_bar.png`, `*_figure2_cdf.png` | Figure 2 panels (a) and (b) |
| `*_figure2_stats.csv` | `mean_lower`, `mean_higher`, `diff`, `t`, `p`, `ks_D`, `ks_p`, `n` |

## What to check after running

1. `20_` reproduces the paper's Table 1 (59 of 60 cells to three decimals; the endline group
   risk-aversion median prints 0.279 here and 0.280 in the paper because the two middle order
   statistics, 0.27930 and 0.27970, straddle the rounding boundary) and Figure 2
   (lower 0.668, higher 0.332, gap 0.335, n = 2,560). `high_mover` agrees with `high_base` /
   `high_end` in `data/panel_final.dta` in all 1,304 pair-waves.
2. In `maxmpi_pairwave.csv`, `exhausted` is TRUE in every row (the script stops otherwise).
3. The `stopifnot` checks pass: costs monotone (`c_mover, c_nonmover <= c_N`), index in [0,1],
   Houtman–Maks index on the grid k / (2 c_N).

## Facts worth knowing

* The index is undefined in the same 24 pair-waves (8 baseline, 16 endline) under every
  measure, because `c_N = 0` exactly when the merged dataset has no cross violation. Hence the
  distance rows have N = 1,288 and 1,272.
* Individual measures tie in 199 pair-waves under the CCEI and MaxMPI (all at the value that
  signals GARP consistency) and in 343 under the Houtman–Maks count. Ties are broken as in
  `01_calculate_ccei.R`: by the member's individual-plus-group score, then in favour of the
  mover. Each script prints how much the convention matters.
* All revealed-preference comparisons are exact integer comparisons (the data are integer KRW
  and exact expenditure ties occur); only money-pump *values* are floating point.
* Do **not** divide the Houtman–Maks count by the size of the merged dataset (the "FGARP"
  fraction): on this data the fraction-based cost is non-monotone in 631 of 1,280 pair-waves
  and puts the index outside [0,1] in 219 of them.

## Independent checks (`recheck/`, python3 only, no packages beyond the standard library)

```
cd recheck
python3 verify_rcode_bruteforce_gen.py 11 60     # writes difftest_cases.json (exact brute force)
Rscript  verify_rcode_bruteforce_run.R           # runs the R routines on the same cases
```
Expected: `mismatches = 0`.

