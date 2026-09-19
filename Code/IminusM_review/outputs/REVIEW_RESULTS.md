# Review results: placebo-adjusted distance

This file is generated from the isolated review package. No manuscript tables or figures are overwritten.

## Individual-fixed-effects comparison

| measure | definition | outcome_model | estimate_se | coefficient_over_outcome_sd | standardized | M_coefficient | N | r2 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| CCEI distance | CCEI difference | raw | -0.751** (0.057) | -2.345 | -0.416 |  | 2512 | 0.639 |
| CCEI distance | CCEI difference | normalized | -0.126* (0.052) | -0.458 | -0.081 |  | 2512 | 0.554 |
| CCEI distance | CCEI difference | adjusted | -0.214** (0.068) | -0.669 | -0.119 | 0.858 [p(M=1)=0.161] | 2512 | 0.673 |
| CCEI distance | Higher CCEI | raw | -0.233** (0.022) | -0.728 | -0.361 |  | 2512 | 0.625 |
| CCEI distance | Higher CCEI | normalized | -0.070** (0.019) | -0.254 | -0.126 |  | 2512 | 0.558 |
| CCEI distance | Higher CCEI | adjusted | -0.090** (0.024) | -0.283 | -0.140 | 0.874 [p(M=1)=0.170] | 2512 | 0.676 |
| Risk-aversion distance | CCEI difference | raw | -0.159 (0.107) | -0.428 | -0.075 |  | 2604 | 0.564 |
| Risk-aversion distance | CCEI difference | normalized | -0.144 (0.111) | -0.371 | -0.065 |  | 2604 | 0.581 |
| Risk-aversion distance | CCEI difference | adjusted | -0.155 (0.107) | -0.418 | -0.073 | 0.248 [p(M=1)=0.000] | 2604 | 0.567 |
| Risk-aversion distance | Higher CCEI | raw | -0.047 (0.029) | -0.126 | -0.062 |  | 2604 | 0.564 |
| Risk-aversion distance | Higher CCEI | normalized | -0.069* (0.029) | -0.179 | -0.089 |  | 2604 | 0.582 |
| Risk-aversion distance | Higher CCEI | adjusted | -0.053+ (0.030) | -0.143 | -0.071 | 0.273 [p(M=1)=0.000] | 2604 | 0.567 |

## Choice-buffer results using I - M

| buffer | focal | specification | estimate | N | r2 |
| --- | --- | --- | --- | --- | --- |
| exact | Higher CCEI | 2 | -0.067** (0.013) | 2512 | 0.05817915 |
| exact | Higher CCEI | 3 | -0.070** (0.019) | 2512 | 0.55803389 |
| exact | CCEI difference | 2 | -0.155** (0.026) | 2512 | 0.05391769 |
| exact | CCEI difference | 3 | -0.126* (0.052) | 2512 | 0.55393981 |
| pp2_5 | Higher CCEI | 2 | -0.066** (0.013) | 2512 | 0.05878749 |
| pp2_5 | Higher CCEI | 3 | -0.070** (0.020) | 2512 | 0.55689236 |
| pp2_5 | CCEI difference | 2 | -0.152** (0.029) | 2512 | 0.05448778 |
| pp2_5 | CCEI difference | 3 | -0.121* (0.054) | 2512 | 0.55251425 |
| pp5 | Higher CCEI | 2 | -0.066** (0.013) | 2512 | 0.06048352 |
| pp5 | Higher CCEI | 3 | -0.070** (0.020) | 2512 | 0.55772385 |
| pp5 | CCEI difference | 2 | -0.154** (0.029) | 2512 | 0.05637342 |
| pp5 | CCEI difference | 3 | -0.120* (0.054) | 2512 | 0.55345607 |

## Mover results using I - M

| focal | specification | main | mover | interaction | N | r2 |
| --- | --- | --- | --- | --- | --- | --- |
| Higher CCEI | 1 | -0.068** (0.016) | -0.009 (0.013) | -0.015 (0.015) | 2512 | 0.01931263 |
| Higher CCEI | 2 | -0.059** (0.016) | -0.006 (0.013) | -0.016 (0.015) | 2512 | 0.05915903 |
| Higher CCEI | 3 | -0.057** (0.021) | 0.000 (0.000) | -0.026 (0.018) | 2512 | 0.55827331 |
| CCEI difference | 1 | -0.155** (0.030) | -0.016 (0.015) | -0.000 () | 2512 | 0.01084806 |
| CCEI difference | 2 | -0.154** (0.026) | -0.014 (0.014) | -0.000 (0.001) | 2512 | 0.05454652 |
| CCEI difference | 3 | -0.126* (0.052) | 0.000 (0.000) | -0.000 (0.001) | 2512 | 0.55393982 |

## Shorrocks-Shapley decomposition

| outcome_model | block | shapley_value | shapley_percent | total_r2 |
| --- | --- | --- | --- | --- |
| normalized | Higher CCEI | 0.012 | 2.120 | 0.558 |
| normalized | Individual/Friendship | 0.014 | 2.544 | 0.558 |
| normalized | Corner/Midpoint shares | 0.012 | 2.167 | 0.558 |
| normalized | Individual FE | 0.520 | 93.169 | 0.558 |
| adjusted | Higher CCEI | 0.062 | 9.118 | 0.676 |
| adjusted | M benchmark | 0.144 | 21.359 | 0.676 |
| adjusted | Individual/Friendship | 0.013 | 1.995 | 0.676 |
| adjusted | Corner/Midpoint shares | 0.005 | 0.737 | 0.676 |
| adjusted | Individual FE | 0.451 | 66.790 | 0.676 |

## Alternative revealed-preference measures: individual fixed effects

HM uses 50 reproducibly sampled donors per target. MaxMPI uses 20 when available. These are review-stage benchmarks; exact full builds are retained as a separate resumable step.

| measure | definition | outcome_model | estimate | coefficient_over_outcome_sd | standardized | M_coefficient | donor_count | N | r2 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| hm | Higher-rationality indicator | raw | -0.174** (0.018) | -0.716 | -0.347 |  | 50 | 2512 | 0.6468481 |
| hm | Rationality difference | raw | -0.048** (0.004) | -0.199 | -0.400 |  | 50 | 2512 | 0.6572394 |
| hm | Higher-rationality indicator | normalized | -0.060** (0.017) | -0.280 | -0.136 |  | 50 | 2512 | 0.5802134 |
| hm | Rationality difference | normalized | -0.010* (0.004) | -0.048 | -0.097 |  | 50 | 2512 | 0.5763951 |
| hm | Higher-rationality indicator | adjusted | -0.083** (0.018) | -0.343 | -0.166 | 0.792 [p(M=1)=0.024] | 50 | 2512 | 0.6802420 |
| hm | Rationality difference | adjusted | -0.020** (0.006) | -0.084 | -0.168 | 0.735 [p(M=1)=0.035] | 50 | 2512 | 0.6776844 |
| hm | Higher-rationality indicator | normalized_drop | -0.060** (0.017) | -0.280 | -0.136 |  | 50 | 2512 | 0.5803191 |
| hm | Rationality difference | normalized_drop | -0.010* (0.004) | -0.048 | -0.096 |  | 50 | 2512 | 0.5764945 |
| hm | Higher-rationality indicator | adjusted_drop | -0.084** (0.018) | -0.345 | -0.167 | 0.788 [p(M=1)=0.022] | 50 | 2512 | 0.6799916 |
| hm | Rationality difference | adjusted_drop | -0.020** (0.006) | -0.084 | -0.170 | 0.729 [p(M=1)=0.032] | 50 | 2512 | 0.6774486 |
| maxmpi | Higher-rationality indicator | raw | -0.223** (0.021) | -0.766 | -0.379 |  | 20 | 2512 | 0.6529513 |
| maxmpi | Rationality difference | raw | -0.661** (0.042) | -2.272 | -0.501 |  | 20 | 2512 | 0.6850569 |
| maxmpi | Higher-rationality indicator | normalized | -0.071** (0.017) | -0.289 | -0.143 |  | 20 | 2512 | 0.5764763 |
| maxmpi | Rationality difference | normalized | -0.152** (0.038) | -0.622 | -0.137 |  | 20 | 2512 | 0.5746952 |
| maxmpi | Higher-rationality indicator | adjusted | -0.103** (0.022) | -0.353 | -0.175 | 0.789 [p(M=1)=0.007] | 20 | 2512 | 0.7041328 |
| maxmpi | Rationality difference | adjusted | -0.341** (0.055) | -1.173 | -0.259 | 0.628 [p(M=1)=0.000] | 20 | 2512 | 0.7068626 |
| maxmpi | Higher-rationality indicator | normalized_drop | -0.066** (0.017) | -0.270 | -0.134 |  | 20 | 2512 | 0.5768842 |
| maxmpi | Rationality difference | normalized_drop | -0.130** (0.038) | -0.530 | -0.117 |  | 20 | 2512 | 0.5742883 |
| maxmpi | Higher-rationality indicator | adjusted_drop | -0.101** (0.022) | -0.348 | -0.172 | 0.777 [p(M=1)=0.004] | 20 | 2512 | 0.7042402 |
| maxmpi | Rationality difference | adjusted_drop | -0.332** (0.057) | -1.140 | -0.251 | 0.620 [p(M=1)=0.000] | 20 | 2512 | 0.7061032 |

## Alternative-measure donor-sample stability

| measure | benchmark | focal | estimate | N | r2 |
| --- | --- | --- | --- | --- | --- |
| hm | half1 | HighHM_both_high | -0.059** (0.017) | 2512 | 0.5760136 |
| hm | half1 | hm_gap_ij | -0.010* (0.004) | 2512 | 0.5723913 |
| hm | half2 | HighHM_both_high | -0.060** (0.017) | 2512 | 0.5828986 |
| hm | half2 | hm_gap_ij | -0.010* (0.004) | 2512 | 0.5789898 |
| hm | fullsample | HighHM_both_high | -0.060** (0.017) | 2512 | 0.5802134 |
| hm | fullsample | hm_gap_ij | -0.010* (0.004) | 2512 | 0.5763951 |
| maxmpi | half1 | HighMaxMPI_both_high | -0.070** (0.017) | 2512 | 0.5739112 |
| maxmpi | half1 | maxmpi_gap_ij | -0.143** (0.037) | 2512 | 0.5714722 |
| maxmpi | half2 | HighMaxMPI_both_high | -0.071** (0.018) | 2512 | 0.5734194 |
| maxmpi | half2 | maxmpi_gap_ij | -0.162** (0.041) | 2512 | 0.5724378 |
| maxmpi | fullsample | HighMaxMPI_both_high | -0.071** (0.017) | 2512 | 0.5764763 |
| maxmpi | fullsample | maxmpi_gap_ij | -0.152** (0.038) | 2512 | 0.5746952 |

## Donor-pool and undefined-donor sensitivity

| measure | benchmark | focal | estimate | standardized | N | r2 |
| --- | --- | --- | --- | --- | --- | --- |
| ccei | all_imp | Higher CCEI | -0.070** (0.019) | -0.12595515 | 2512 | 0.5580339 |
| ccei | all_imp | CCEI difference | -0.126* (0.052) | -0.08130507 | 2512 | 0.5539398 |
| ccei | all_drop | Higher CCEI | -0.070** (0.019) | -0.12605987 | 2512 | 0.5581347 |
| ccei | all_drop | CCEI difference | -0.126* (0.052) | -0.08124512 | 2512 | 0.5540255 |
| ccei | sameclass | Higher CCEI | -0.054* (0.021) | -0.09396410 | 2512 | 0.5333666 |
| ccei | sameclass | CCEI difference | -0.106+ (0.058) | -0.06653847 | 2512 | 0.5313875 |
| ccei | outclass_imp | Higher CCEI | -0.070** (0.019) | -0.12633236 | 2512 | 0.5585591 |
| ccei | outclass_imp | CCEI difference | -0.126* (0.052) | -0.08150093 | 2512 | 0.5544374 |
| ra | all_imp | Higher CCEI | -0.069* (0.029) | -0.08860146 | 2604 | 0.5822468 |
| ra | all_imp | CCEI difference | -0.144 (0.111) | -0.06484737 | 2604 | 0.5805978 |
| ra | all_drop | Higher CCEI | -0.069* (0.029) | -0.08860146 | 2604 | 0.5822468 |
| ra | all_drop | CCEI difference | -0.144 (0.111) | -0.06484737 | 2604 | 0.5805978 |
| ra | outclass_imp | Higher CCEI | -0.070* (0.029) | -0.08861788 | 2604 | 0.5823498 |
| ra | outclass_imp | CCEI difference | -0.144 (0.111) | -0.06494812 | 2604 | 0.5807048 |
| ra | outclass_drop | Higher CCEI | -0.070* (0.029) | -0.08861788 | 2604 | 0.5823498 |
| ra | outclass_drop | CCEI difference | -0.144 (0.111) | -0.06494812 | 2604 | 0.5807048 |

## MaxMPI review-build diagnostics

The review build uses 20 sampled donors and a two-second cap per exact cross-cost calculation. Unresolved donor distances receive one half in the main normalized outcome; the `normalized_drop` rows above exclude them.

| mean_undefined_share | median_undefined_share | share_with_all_20_solved | minimum_solved_donors | member_wave_rows |
| --- | --- | --- | --- | --- |
| 0.020 | 0.000 | 0.825 | 1 | 2608 |

## Output files

- `tables/main_comparison.csv`: all raw, normalized, and M-adjusted specifications.
- `tables/figure_distribution_stats.csv`: means, t tests, and KS tests.
- `tables/buffer_results.csv`: exact, 2.5-point, and 5-point choice buffers.
- `tables/mover_results.csv`: mover and interaction specifications.
- `tables/baseline_correlations_IminusM.csv`: Section 5.1 correlation inputs.
- `tables/alternative_measure_comparison.csv`: HM and, when complete, MaxMPI comparisons.
- `tables/alternative_sample_stability.csv`: split-half donor-sample checks.
- `tables/benchmark_sensitivity.csv`: undefined-donor and donor-pool checks.
- `tables/maxmpi_review_diagnostics.csv`: timeout/undefined rates for the provisional MaxMPI benchmark.
- `figures/ccei_coefficient_comparison.png`: side-by-side main estimates.
- `figures/ccei_IminusM_by_higher_ccei_bar.png` and `...cdf.png`: candidate main figure.
