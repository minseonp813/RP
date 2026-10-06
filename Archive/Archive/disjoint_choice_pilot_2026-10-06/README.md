# Table A7: real-data pilot, 2026-10-06

This is **one partition**, with both A-to-B and B-to-A estimates. It is not
the requested 500-partition simulation and is not a manuscript table update.

All 652 pairs supply choices; M uses all 651 non-own same-wave donors in
each half, with undefined donor distances imputed to 0.5. Starting from
Table 3's balanced sample, the two directions retain 1,968 and 1,956
member-wave observations with defined held-out distances in both waves.
Every fit has 64 class clusters. Specifications match the six columns of
Table 3, including M, source-half choice shares, and no risk-aversion controls.

| Coefficient | (1) | (2) | (3) | (4) | (5) | (6) |
|---|---:|---:|---:|---:|---:|---:|
| Table 3 | -0.092 | -0.077 | -0.090 | -0.249 | -0.228 | -0.214 |
| Pilot A-to-B | -0.027 | -0.022 | -0.032 | -0.129 | -0.153 | -0.163 |
| Pilot B-to-A | -0.036 | -0.032 | -0.032 | -0.039 | -0.052 | 0.001 |
| Median of two directions | -0.031 | -0.027 | -0.032 | -0.084 | -0.102 | -0.081 |

Columns (1)-(3) use the higher-CCEI indicator, assigning ties high.
Columns (4)-(6) use the signed CCEI difference. Within each block the
specifications are class effects, controls with class effects, and controls
with individual effects. Every column controls for held-out-half M.

None of the higher-CCEI coefficients is significant at 5% in either
direction. The CCEI-gap coefficient is significant at 5% only in Column (5)
of A-to-B (p=0.039); its B-to-A estimates are not significant. These are
individual-fit clustered tests, not inference from pooling split draws.
The two draws are insufficient for meaningful split-distribution ranges;
the generic pilot TeX export's percentile brackets only span those two
estimates and must not be read as confidence intervals.

`inputs_and_results/disjoint_estimates.dta` retains every fitted coefficient,
clustered standard error, N, R-squared and cluster count.
`inputs_and_results/table_disjoint_choice_pilot.tex` contains the six-column
pilot export. To re-estimate these inputs, run the final Table A7 section
of Code/08_Tables_Appendix.do with DISJOINT_OUTPUT_DIR set to this archive's
inputs_and_results folder. It will produce a pilot, without copying to Overleaf.

The original R calculation was stopped after four benchmark batches.
A separate temporary native implementation of the same exact rational-grid
CCEI algorithm completed the pilot. The tracked numerical code was not changed.
Checks matched 4,500 numerical costs bit for bit; all 5,216 own member-wave-half
CCEIs, distances and choice-share inputs matched the original R calculation
exactly; every benchmark-summary column agreed within 1e-12 for 512
member-waves computed by both implementations using all 651 donors.
The prototype and temporary programs are retained for auditing in checks
and temporary_programs; they are not production entry scripts. Their dynamic
library path refers to the temporary compilation location, so rebuild/repoint
the library before reusing the temporary programs in another environment.

Input and production-source hashes are recorded in input_source_sha256.json.
The partial original-R cache is retained under reference_R_partial for audit
or resumption via DISJOINT_OUTPUT_DIR. No production split inputs or revised
manuscript Table A7 were created.
