# Exploratory Table 3: choice buffers

Created 18 September 2026. These results are separate from the manuscript and production tables.

- `table3_choice_buffers.pdf`: comparison and all three six-column alternatives.
- `table3_choice_buffers.tex`: standalone review document.
- `table3_*.tex`: regression fragments, including a replication of the current table.
- `coefficients.csv`: unrounded focal coefficients, standard errors, p-values, sample sizes, R-squared, residual degrees of freedom, and cluster counts.
- `choice_rates.csv`: mean corner/equal-allocation shares in the balanced sample.
- `estimation.log`: Stata estimation and validation log.

Run `do 99_18_table3_choice_buffers.do` from `Code` to reproduce regressions. The script reads the existing panel and raw choices; it writes only this results directory. It uses reghdfe and esttab, as does the production analysis. The review document's overview is a snapshot of this run; refresh its summary entries if specifications or inputs change.

All alternatives omit RA_i and RA_diff, retain all other Table 3 controls and its 2,512 observations, and cluster by the same 64 classes. The baseline and all three alternatives use the same tie coding. The exact benchmark reproduces the published-in-draft table cells. Buffers use payoff share x/(x+y), with inclusive bounds at 2.5 and 5 percentage points. Integer inequalities avoid floating-point errors at the boundaries.
