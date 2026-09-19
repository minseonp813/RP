# Decision summary: should the main analysis use `I - M` or control for `M`?

No manuscript files or existing paper tables were changed for this review.

## Main CCEI result

The central qualitative result survives either adjustment. In the individual
fixed-effects specification:

| Rationality variable | Raw `I` | `I - M` | Raw `I`, controlling for `M` |
|---|---:|---:|---:|
| Higher-CCEI indicator | -0.233** (0.022) | -0.070** (0.019) | -0.090** (0.024) |
| CCEI difference | -0.751** (0.057) | -0.126* (0.052) | -0.214** (0.068) |

For the higher-CCEI indicator, the `I - M` coefficient is 25.4% of the
adjusted outcome's standard deviation; the estimate controlling for `M` is
28.3% of the raw outcome's standard deviation. For a one-standard-deviation
increase in the CCEI difference, the corresponding magnitudes are 0.081 and
0.119 outcome standard deviations.

The descriptive higher-minus-lower mean difference falls from -0.251 for raw
`I` to -0.078 for `I - M`, but remains highly statistically significant. The
two adjusted distributions also differ in the Kolmogorov-Smirnov test
(`p < 0.001`).

The coefficient on `M` in the individual fixed-effects models is 0.874 for the
indicator specification and 0.858 for the CCEI-difference specification. The
hypothesis that it equals one is not rejected (`p = 0.170` and `p = 0.161`).
This is why subtraction and flexible control give relatively similar CCEI
results, although the focal coefficients are not identical.

## Section 5.1 supporting results

- The exact, 2.5-point, and 5-point corner/equal-allocation buffers produce
  nearly identical `I - M` estimates.
- Mover status and its interaction with rationality remain statistically
  insignificant.
- Assigning undefined donor distances one half or dropping them produces the
  same CCEI estimates to three decimal places.
- Excluding every donor from the target's own class also leaves the estimates
  unchanged. A benchmark using only the small same-class donor pool is noisier.
- In the corrected Shorrocks-Shapley decomposition of the current Column (3),
  the higher-CCEI block accounts for 2.1% of explained variation in `I - M`,
  while individual fixed effects account for 93.2%. In the model of raw `I`
  controlling for `M`, higher CCEI accounts for 9.1%, `M` for 21.4%, and
  individual fixed effects for 66.8%.

The Shorrocks-Shapley numbers differ from the current manuscript partly because
the existing generator still included the risk-aversion block after risk
aversion had been removed from Table 3. The review calculation matches the
current Table 3 controls.

## Alternative outcomes from Section 5.2

Individual fixed-effects estimates are:

| Outcome and rationality variable | Raw `I` | `I - M` | Raw `I`, controlling for `M` |
|---|---:|---:|---:|
| Risk distance: Higher CCEI | -0.047 (0.029) | -0.069* (0.029) | -0.053+ (0.030) |
| Risk distance: CCEI difference | -0.159 (0.107) | -0.144 (0.111) | -0.155 (0.107) |
| HM: Higher rationality | -0.174** (0.018) | -0.060** (0.017) | -0.083** (0.018) |
| HM difference | -0.048** (0.004) | -0.010* (0.004) | -0.020** (0.006) |
| MaxMPI: Higher rationality | -0.223** (0.021) | -0.071** (0.017) | -0.103** (0.022) |
| MaxMPI difference | -0.661** (0.042) | -0.152** (0.038) | -0.341** (0.055) |

The two HM donor halves give estimates of -0.059 and -0.060 for the
higher-rationality indicator and -0.010 in both halves for the HM difference.
The two MaxMPI donor halves give -0.070 and -0.071 for the indicator and -0.143
and -0.162 for the difference. Thus the review-stage donor sampling does not
drive the qualitative results.

For MaxMPI, dropping unresolved donor calculations gives -0.066 for the
indicator and -0.130 for the difference; both remain significant at the 1%
level. This is close to the neutral-imputation estimates of -0.071 and -0.152.

## Interpretation for the specification choice

The evidence consistently says that the raw relationship contains two parts:

1. more rational individuals generally resemble collective choices, including
   choices made by unrelated groups; and
2. their own pair's collective choices are especially close to them beyond
   that general resemblance.

The second component is smaller than the raw coefficient but remains negative
and statistically significant for CCEI, HM, and MaxMPI. The higher-CCEI result
also becomes stronger for the risk-aversion outcome after subtraction.

Using `I - M` fixes the coefficient on `M` at one. That restriction is
empirically plausible for the main CCEI specification but is rejected for HM,
MaxMPI, and risk-aversion distance. Controlling for `M` therefore provides the
more flexible common specification across all four outcomes and preserves the
interpretation and `[0,1]` scale of the original distance. `I - M` has the
cleaner interpretation as pair-specific excess alignment but is centered at
zero, ranges from -1 to 1, and is no longer literally a distance.

## Review-build caveat

CCEI and risk-aversion benchmarks use all 651 non-own donor pairs. HM uses 50
reproducibly sampled donors per target. MaxMPI uses 20 sampled donors; exact
cross-cost calculations exceeding two seconds are assigned the neutral value
one half in the main review benchmark. On average, 2.0% of MaxMPI donor
distances were unresolved, and 82.5% of member-waves had all 20 solved exactly.
Exact full HM and MaxMPI builds are checkpointed as a separate resumable step
and should be completed before changing the manuscript.

