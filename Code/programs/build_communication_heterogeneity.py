"""Build the exploratory Table 5(3) heterogeneity review from Stata estimates."""
import csv
from pathlib import Path

root = Path(__file__).resolve().parents[2]
out = root / "Code/results/new_indices/communication_heterogeneity"
moderators = ["wave", "friend", "math_pair", "math_less", "friend_any"]
titles = {"wave": "Baseline versus endline", "friend": "Dyadic friendship: three nomination categories",
          "math_pair": "Pair-average math score", "math_less": "Math score of the less rational member",
          "friend_any": "Dyadic friendship: the paper's binary measure"}
labels = {"wave": ["Baseline", "Endline"], "friend": ["No nomination", "One-sided nomination", "Mutual nomination"],
          "math_pair": ["Below wave median", "At/above wave median"],
          "math_less": ["Below wave median", "At/above wave median"],
          "friend_any": ["No nomination", "Any nomination"]}


def read(name):
    with (out / name).open() as f:
        return list(csv.DictReader(f))


coeff = {m: read(f"{m}_coefficients.csv") for m in moderators}
tests = {m: read(f"{m}_tests.csv") for m in moderators}
diag = {m: read(f"{m}_diagnostics.csv") for m in moderators}
robust = {m: read(f"{m}_wavefe_tests.csv") for m in moderators if m != "wave"}


def estimate(m, outcome, member, group=None):
    return next(r for r in coeff[m] if r["outcome"] == outcome and r["member"] == member
                and (r["sample"] == "pooled" if group is None else r["sample"] == "subgroup" and int(float(r["group"])) == group))


def difference(m, outcome, member, higher=1, lower=0, wavefe=False):
    data = robust[m] if wavefe else tests[m]
    return next(r for r in data if r["outcome"] == outcome and r["member"] == member
                and r["test"] == "pairwise" and int(float(r["higher"])) == higher and int(float(r["lower"])) == lower)


def pvalue(value):
    return "$<0.001$" if float(value) < .001 else f"{float(value):.3f}"


def ptext(value):
    return "$p<0.001$" if float(value) < .001 else f"$p={float(value):.3f}$"


def slope(r):
    return f"{float(r['estimate']):.3f} ({float(r['se']):.3f})"


def contrast(r):
    return f"{float(r['estimate']):.3f} ({pvalue(r['p'])})"


parts = [r"""\documentclass[11pt]{article}
\usepackage[letterpaper,landscape,margin=0.65in]{geometry}
\usepackage{booktabs,graphicx}
\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt}
\begin{document}
\begin{center}\Large\textbf{Communication proxies and the individual rationality gradient}\end{center}
\textbf{Finding.} These splits do not support a stronger minimum-CCEI association with group CCEI when communication
is presumed easier. Point estimates are smaller at endline, among friends, and in higher-math groups. The friendship
differences are statistically clearer; the wave and math differences in minimum-CCEI slopes are not.

\begin{center}\small
\begin{tabular}{lrrrrr}\toprule
Comparison & Group CCEI: $\beta_{\min}$ & Difference $p$ & CEIV: $\beta_{\min}$ & Difference $p$ & Selectivity $p$\\
& Lower $\to$ higher proxy & & Lower $\to$ higher proxy & & CCEI minus CEIV\\\midrule
"""]
for m, label in [("wave", r"Baseline $\to$ endline"), ("friend_any", r"No $\to$ any nomination"),
                 ("math_pair", r"Pair math: below $\to$ at/above median"), ("math_less", r"Less-rational math: below $\to$ at/above")]:
    slopes = {}
    for o in ["ccei_g", "ceiv_g"]:
        slopes[o] = f"{float(estimate(m,o,'min',0)['estimate']):.3f} $\\to$ {float(estimate(m,o,'min',1)['estimate']):.3f}"
    parts.append(label + " & " + slopes["ccei_g"] + " & " + pvalue(difference(m,"ccei_g","min")["p"])
                 + " & " + slopes["ceiv_g"] + " & " + pvalue(difference(m,"ceiv_g","min")["p"])
                 + " & " + pvalue(difference(m,"ccei_ceiv_gap","min")["p"]) + r"\\" + "\n")
parts.append(r"""\bottomrule\end{tabular}\end{center}
\small
\textit{Specification:} Table 5 column (3): both individual CCEIs, student/personality, friendship/network, and
corner/midpoint-share controls, with class fixed effects and class-clustered standard errors. Risk-aversion controls
are excluded. Coefficients are per one-unit increase in individual CCEI; multiply by 0.1 for a 0.1 increase. Comparisons
are descriptive and unadjusted for multiple testing. Math cutoffs are wave-specific; median ties enter the higher bin.

\textbf{Tests of the proposed channel.} Comparing significance across outcomes is insufficient. ``Selectivity'' tests
whether the moderator changes the minimum-CCEI coefficient differently for group CCEI and CEIV, by fitting the same
model to CCEI minus CEIV. The binary friendship contrast is a larger decline for group CCEI ($\Delta=-0.146$, $p=0.002$),
the opposite direction to the proposed complementarity. This selective decline is less precisely estimated when
one-sided and mutual friendships have separate slopes and controls. Adding wave fixed effects preserves the main direction.

\textbf{Another pattern.} Maximum individual CCEI has a larger CEIV coefficient in higher-math groups: a slope difference
of 0.551 ($p<0.001$) for pair-average math and 0.452 ($p=0.004$) for the less rational member's math. This is a different
pattern from the proposed minimum-CCEI channel. The cross-outcome tests do not establish that this moderation is
larger for CEIV than for CCEI.

\textbf{Implication for the draft.} Point (2) should remain an open hypothesis. An alternative possibility is that easier
coordination substitutes for the less rational member's rationality, but these observational proxies do not identify that
mechanism. The survey evidence supports perceived similarity more clearly than shared or own influence. Group CCEI
does not identify stable weights, CEIV does not identify turn-taking, and the prior RA-gap split weakens rather than
fully excludes a risk-preference explanation. A normative reason to prefer stable weights requires an additional argument.
""")

intro = {
    "wave": "Split the same 652 pairs into baseline and endline (652 pair-waves each). Endline is a proxy for additional experience, not a randomized communication treatment.",
    "friend": "Friendship uses directed nominations: none, one-sided, or mutual. Eligible counts are 912/225/167 pair-waves; 167 pairs change category between waves. The split is contemporaneous rather than fixed at baseline.",
    "math_pair": "Compute the average of both members' observed math scores (0--5). Exclude 13 pair-waves with at least one missing score. The median is 2.5 at baseline and 3.0 at endline; high means at or above that wave's median. Eligible low/high counts are 530/761. Median ties number 111/123 at baseline/endline and are retained in the high group.",
    "math_less": "Select the member with strictly lower individual CCEI in each pair and wave. Exclude 199 CCEI-tied pair-waves and 11 additional pair-waves missing the selected member's math score. The math median is 3 in both waves; low is below 3, high is at least 3. Eligible low/high counts are 522/572 (1,094 total). Median ties are 116/104 at baseline/endline. The selected member can change across waves.",
    "friend_any": "Use the existing Table 5 friendship indicator: at least one directed nomination versus none. This pools one-sided and mutual friendship with common subgroup slopes and controls. Eligible counts are 912/392 pair-waves; the three-category results appear on page 3.",
}
reading = {
    "wave": "The minimum-CCEI association is smaller at endline, with no clear slope difference for either outcome. The endline group CCEI level and share equal to one are higher, so the split also changes outcome distributions and available room for improvement.",
    "friend": "The minimum-CCEI coefficient on group CCEI is smaller in both friendship groups. The CEIV coefficient is also smaller, particularly for mutual nominations. Neither separate friendship contrast establishes CCEI-specific moderation; a difference in within-model significance is not such a test.",
    "math_pair": "Minimum-CCEI point estimates are smaller in the higher-math group, but their differences are imprecise. Maximum-CCEI coefficients are larger for both outcomes, with a clearer CEIV difference. These findings do not support the predicted stronger minimum-CCEI gradient.",
    "math_less": "The minimum-CCEI gradient remains positive in both math groups, but is smaller in the higher-math group without a clear difference. The maximum-CCEI coefficient on CEIV is larger in the higher-math group. The math split excludes tied pairs and uses a role defined by the same individual CCEIs entering the regression, so it is descriptive rather than an exogenous ability interaction.",
    "friend_any": "The minimum-CCEI slope is near zero among pairs with any nomination, compared with positive slopes among pairs with no nomination. The decline is larger for group CCEI than CEIV in this pooled friendship specification, including after adding wave controls. This is opposite to the predicted stronger minimum-CCEI association under easier communication.",
}
for m in moderators:
    parts.append("\\clearpage\n\\begin{center}\\Large\\textbf{" + titles[m] + "}\\end{center}\n\\small\n" + intro[m] + "\n")
    parts.append(f"\\begin{{center}}\\includegraphics[width=\\textwidth]{{{m}_coefficients.png}}\\end{{center}}\n")
    parts.append(r"\begin{center}\begin{tabular}{lrrrrrr}\toprule" + "\n")
    parts.append(r"Sample & Fitted $N$ & Classes & CCEI: maximum & CCEI: minimum & CEIV: maximum & CEIV: minimum\\\midrule" + "\n")
    for g, name in [(None, "Eligible pooled reference")] + list(enumerate(labels[m])):
        size = estimate(m, "ccei_g", "min", g)
        cells = [slope(estimate(m,o,member,g)) for o in ["ccei_g","ceiv_g"] for member in ["max","min"]]
        parts.append(f"{name} & {size['n']} & {size['clusters']} & " + " & ".join(cells) + r"\\" + "\n")
    parts.append(r"\bottomrule\end{tabular}\end{center}" + "\n")

    parts.append(r"\begin{center}\begin{tabular}{llrrr}\toprule" + "\n")
    parts.append(r"Higher minus lower group & Individual CCEI & $\Delta$ group CCEI ($p$) & $\Delta$ CEIV ($p$) & $\Delta$(CCEI--CEIV) ($p$)\\\midrule" + "\n")
    comparisons = [(hi,lo) for hi in range(len(labels[m])) for lo in range(hi)]
    for hi, lo in comparisons:
        for member in (["min"] if m == "friend" else ["min","max"]):
            cells = [contrast(difference(m,o,member,hi,lo)) for o in ["ccei_g","ceiv_g","ccei_ceiv_gap"]]
            name = (f"{hi} minus {lo}" if m == "friend" else "Higher minus lower")
            parts.append(name + " & " + ("Minimum" if member == "min" else "Maximum") + " & " + " & ".join(cells) + r"\\" + "\n")
    if m != "wave":
        parts.append(r"\addlinespace\multicolumn{5}{l}{\textit{Wave-fixed-effects check: minimum CCEI}}\\" + "\n")
        robust_pairs = [(1,0),(2,0)] if m == "friend" else [(1,0)]
        for hi,lo in robust_pairs:
            cells = [contrast(difference(m,o,"min",hi,lo,wavefe=True)) for o in ["ccei_g","ceiv_g","ccei_ceiv_gap"]]
            parts.append((f"{hi} minus {lo}" if m == "friend" else "Higher minus lower") + " & Minimum & " + " & ".join(cells) + r"\\" + "\n")
    parts.append(r"\bottomrule\end{tabular}\end{center}" + "\n")
    if m == "friend":
        parts.append("Friendship coding in the contrast table: 0=no nomination, 1=one-sided, 2=mutual.\n")
    parts.append("\\textbf{Reading the pattern.} " + reading[m] + "\n")
    ds = sorted(diag[m], key=lambda r:int(r["group"]))
    parts.append("\\textit{Support check:} Mean minimum individual CCEI by subgroup: " + ", ".join(f"{float(r['mean_min']):.3f}" for r in ds)
                 + "; share of group CCEI equal to one: " + ", ".join(f"{100*float(r['full_ccei_share']):.1f}\\%" for r in ds) + ". These describe eligible samples before singleton exclusions.\n")
    parts.append("\\textit{Notes:} Class-clustered SEs are in parentheses; 95\\% intervals are plotted. Differences use pooled OLS fully interacting subgroup membership with both CCEIs, all controls, and class effects, accounting for shared classes. Pooled references use eligible samples; separate fits drop observations in singleton classes. Eligible counts are in the introduction; fitted counts are in the table.\n")
parts.append("\\end{document}\n")
(out / "review.tex").write_text("".join(parts))
