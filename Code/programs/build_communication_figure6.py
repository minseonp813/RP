"""Build the subgroup Figure 6 review from the exported Stata AMEs."""
import csv
import math
import sys
from pathlib import Path

root = Path(__file__).resolve().parents[2]
variant = sys.argv[1] if len(sys.argv) > 1 else ""
assert variant in ("", "no_shares")
suffix = "_no_shares" if variant else ""
out = root / f"Code/results/new_indices/communication_figures{suffix}"


def read(name):
    with (out / name).open() as f:
        return list(csv.DictReader(f))


effects = read("figure6_ame.csv")
shared = read("friendship_shared_ame.csv")
status = read("model_status.csv")
assert len(status) == 11
assert all(r["success"] == "1" or (r["moderator"] == "friend" and r["group"] == "2") for r in status)
with (out / "plot_input.csv").open("w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=effects[0].keys())
    writer.writeheader()
    writer.writerows(effects + shared)


def rows(moderator, group, alternative=False):
    return [r for r in (shared if alternative else effects)
            if r["moderator"] == moderator and int(r["group"]) == group]


def full_effect(moderator, group, categories):
    return 100 * sum(float(r["estimate"]) for r in rows(moderator, group)
                     if r["member"] == "minimum" and int(r["outcome"]) in categories)


parts = [r"""\documentclass[11pt]{article}
\usepackage[letterpaper,landscape,margin=0.65in]{geometry}
\usepackage{booktabs,graphicx,xcolor}
\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt}
\begin{document}
\begin{center}\Large\textbf{Figure 6 by communication-proxy subgroups}\end{center}
\textbf{What is plotted.} Average marginal effects from Figure 6's four-category multinomial logit,
re-estimated separately in each subgroup. Both maximum and minimum individual CCEI enter together.
Points measure the probability change, in percentage points, per 0.1 increase in individual CCEI;
bars are 95\% class-clustered confidence intervals. All panels use the same axis, from $-40$ to $40$ points.

\textbf{Main comparison.} The weaker minimum-CCEI association among friendship and higher-math groups
largely carries over to the probability of full group consistency. The wave comparison does not:
the minimum-CCEI marginal effect on reaching group CCEI equal to one is larger at endline.
These endpoint outcomes differ from the continuous group-index regressions previously reported.

\begin{center}\small
\begin{tabular}{lrrrr}\toprule
& \multicolumn{2}{c}{Probability of CCEI $=1$} & \multicolumn{2}{c}{Probability of CEIV $=1$}\\
Split & Lower proxy & Higher proxy & Lower proxy & Higher proxy\\\midrule
"""]
for moderator, label in [("wave", "Baseline / endline"), ("friend_any", "No / any friendship nomination"),
                         ("math_pair", "Pair math: below / at-or-above wave median"),
                         ("math_less", "Less-rational math: below / at-or-above wave median")]:
    values = [full_effect(moderator, g, cats) for cats in [(2, 4), (3, 4)] for g in [0, 1]]
    parts.append(label + " & " + " & ".join(f"{v:.2f}" for v in values) + r"\\" + "\n")
parts.append(r"""\bottomrule\end{tabular}\end{center}
\small
The table sums the minimum-CCEI marginal effects across the relevant categories. It reports point estimates,
not formal tests of differences across subgroups. Comparing which confidence intervals contain zero is not a test
of heterogeneity. Subgroup distributions and room for improvement also differ.

\textbf{Same controls as Figure 6.} Student/personality, friendship/network, and corner/midpoint-share controls,
plus class fixed effects and class-clustered standard errors. No risk-aversion controls or risk-gap restriction.
The primary non-wave splits pool baseline and endline, as in the preceding heterogeneity analysis.
CCEI and CEIV equal one when their computed value is at least $1-10^{-9}$; CEIV lower and upper bounds agree
on this classification. All requested separate fits retain their entire eligible subgroup.

\textbf{Small-subgroup limitation.} The mutual-friendship sample has 167 observations and 54 classes.
Its separate full-control model approaches perfect prediction (log likelihood $\simeq -0.0003$).
Although the optimizer reports convergence, its essentially zero local marginal effects are uninformative.
The final page therefore shows an explicitly labelled alternative: estimate all friendship categories jointly,
interacting both CCEIs with friendship, while sharing the other controls and class effects.

\textbf{Interpretation.} The splits are descriptive communication proxies. Group CCEI measures internal
consistency; it does not identify constant aggregation weights. CEIV does not identify turn-taking.
Experience, friendship, and math scores do not directly measure the content of communication.
""")

if variant:
    parts[0] = parts[0].replace("Figure 6 by communication-proxy subgroups", "Figure 6 without individual choice-share controls")
    # Keep the omitted controls and observed numerical limitation explicit.
    parts[-1] = parts[-1].replace(
        "Student/personality, friendship/network, and corner/midpoint-share controls,\nplus class fixed effects and class-clustered standard errors.",
        "Student/personality and friendship/network controls,\nplus class fixed effects and class-clustered standard errors. Individual corner- and midpoint-choice shares are excluded.")
    parts[-1] = parts[-1].replace(
        "Its separate full-control model approaches perfect prediction (log likelihood $\\simeq -0.0003$).\nAlthough the optimizer reports convergence, its essentially zero local marginal effects are uninformative.",
        "Its separate model does not produce a converged, validated fit with either omitted-outcome parameterization.")
    parts.append(r"""\clearpage
\begin{center}\Large\textbf{Does removing choice-share controls change the continuous results?}\end{center}
\textbf{Finding.} The principal minimum-CCEI pattern is essentially unchanged. In the pooled sample its
group-CCEI coefficient moves from 0.235 to 0.228, and its CEIV coefficient from 0.086 to 0.081.
The friendship difference remains clear; wave and math differences remain imprecise.

\begin{center}\small\begin{tabular}{lrrrr}\toprule
& \multicolumn{2}{c}{Group CCEI: minimum individual CCEI} & \multicolumn{2}{c}{CEIV: minimum individual CCEI}\\
Sample & With shares & Without shares & With shares & Without shares\\\midrule
""")
    comparisons = [("wave", None, "All pair-waves"), ("wave", 0, "Baseline"), ("wave", 1, "Endline"),
                   ("friend_any", 0, "No friendship nomination"), ("friend_any", 1, "Any friendship nomination"),
                   ("math_pair", 0, "Pair math: below median"), ("math_pair", 1, "Pair math: at/above median"),
                   ("math_less", 0, "Less-rational math: below median"), ("math_less", 1, "Less-rational math: at/above median")]
    for moderator, group, label in comparisons:
        values = []
        for outcome in ["ccei_g", "ceiv_g"]:
            for control_suffix in ["", "_no_shares"]:
                folder = root / f"Code/results/new_indices/communication_heterogeneity{control_suffix}"
                with (folder / f"{moderator}_coefficients.csv").open() as f:
                    data = list(csv.DictReader(f))
                r = next(x for x in data if x["outcome"] == outcome and x["member"] == "min"
                         and (x["sample"] == "pooled" if group is None else x["sample"] == "subgroup" and int(float(x["group"])) == group))
                values.append(f"{float(r['estimate']):.3f} ({float(r['se']):.3f})")
        parts.append(label + " & " + " & ".join(values) + r"\\" + "\n")
    parts.append(r"""\bottomrule\end{tabular}\end{center}
\small
Coefficients are per one-unit increase in individual CCEI, with class-clustered standard errors in parentheses.
Multiply by 0.1 for a 0.1 increase. Both individual CCEIs enter together. The original and reduced-control models
use identical samples and retain class fixed effects and all other controls. These continuous regressions differ
from the four-category multinomial probability model shown in the remaining figures.

\textbf{Formal differences without choice-share controls.} Any friendship nomination minus none changes the
minimum-CCEI slope by $-0.304$ for group CCEI ($p<0.001$), and $-0.170$ for CEIV ($p=0.023$).
The larger decline for group CCEI is supported by the direct cross-outcome test ($p=0.005$).
The group-CCEI slope differences have $p=0.390$ for endline minus baseline, $p=0.169$ for higher minus lower
pair math, and $p=0.200$ for higher minus lower less-rational-member math.

\textbf{What this check addresses.} Individual CCEI and these choice shares are calculated from the same individual
choices. Including the shares asks about rationality conditional on those choice patterns; excluding them gives a
broader adjusted association. The similarity of results reduces concern that those controls drive the present pattern.
It does not identify the bargaining or communication process.
""")

descriptions = {
    "wave": "The same 652 pairs are observed in each wave. Endline may capture experience as well as other changes between waves.",
    "friend_any": "Friendship means at least one directed nomination. It is contemporaneous: 167 pairs change the three-category friendship classification between waves.",
    "math_pair": "Average both observed math scores (0--5); exclude 13 pair-waves with a missing score. Wave medians are 2.5 at baseline and 3 at endline. Median ties enter the higher bin; eligible low/high counts are 530/761.",
    "math_less": "Select the member with strictly lower individual CCEI in each wave. Exclude 199 CCEI-tied pair-waves and 11 additional pair-waves with missing selected-member math. The median is 3 in both waves; ties enter the higher bin. Eligible low/high counts are 522/572. The selected member can change across waves.",
    "friend": "Directed friendship nominations distinguish none, one-sided, and mutual. Eligible counts are 912/225/167 pair-waves. The no-nomination figure duplicates the corresponding binary-friendship reference.",
}
titles = {
    "wave": ["Baseline", "Endline"],
    "friend_any": ["No friendship nomination", "At least one friendship nomination"],
    "math_pair": ["Pair-average math: below wave median", "Pair-average math: at or above wave median"],
    "math_less": ["Less rational member's math: below wave median", "Less rational member's math: at or above wave median"],
    "friend": ["No friendship nomination: three-category reference", "One-sided friendship nomination", "Mutual friendship nomination"],
}
category_headers = [r"CCEI $<1$, CEIV $<1$", r"CCEI $=1$, CEIV $<1$",
                    r"CCEI $<1$, CEIV $=1$", r"CCEI $=1$, CEIV $=1$"]
for moderator in ["wave", "friend_any", "math_pair", "math_less", "friend"]:
    for group, title in enumerate(titles[moderator]):
        alternative = moderator == "friend" and group == 2
        plot_moderator = "friend_pool" if alternative else moderator
        selected = rows(plot_moderator, group, alternative)
        assert len(selected) == 8
        r = selected[0]
        parts.append("\\clearpage\n\\begin{center}\\Large\\textbf{" + title + "}\\end{center}\n")
        if alternative:
            parts.append(r"\textcolor{red!65!black}{\textbf{Shared-controls alternative; not an exact separate-subgroup replication.}}" + "\n")
        parts.append("\\small " + descriptions[moderator] + "\n")
        if alternative:
            parts.append("The model fits all 1,304 pair-waves (64 class clusters), allowing friendship-specific slopes for both individual CCEIs. Marginal effects are averaged over the 167 mutual-friendship observations only. Other controls and class effects are shared across friendship categories.\n")
        else:
            parts.append(f"Separate-subgroup fit: {r['n']} pair-waves, {r['pairs']} distinct pairs, {r['clusters']} class clusters.\n")
        parts.append(r"\begin{center}\begin{minipage}{0.48\textwidth}\centering" + "\n")
        parts.append(f"\\includegraphics[width=\\linewidth]{{figure6_a_{plot_moderator}_{group}_ame_maximum.png}}\n")
        parts.append(r"\textbf{(a) More rational member: maximum CCEI}\end{minipage}\hfill\begin{minipage}{0.48\textwidth}\centering" + "\n")
        parts.append(f"\\includegraphics[width=\\linewidth]{{figure6_a_{plot_moderator}_{group}_ame_minimum.png}}\n")
        parts.append(r"\textbf{(b) Less rational member: minimum CCEI}\end{minipage}\end{center}" + "\n")
        parts.append(r"\begin{center}\begin{tabular}{lrrrr}\toprule" + "\n")
        parts.append("Outcome & " + " & ".join(category_headers) + r"\\\midrule" + "\n")
        category_rows = sorted([x for x in selected if x["member"] == "minimum"], key=lambda x:int(x["outcome"]))
        counts = [round(float(x["share"])*int(r["n"])) for x in category_rows]
        assert sum(counts) == int(r["n"])
        parts.append("Observed pair-waves & " + " & ".join(map(str, counts)) + r"\\" + "\n")
        parts.append("Share & " + " & ".join(f"{100*float(x['share']):.1f}\\%" for x in category_rows) + r"\\\bottomrule\end{tabular}\end{center}" + "\n")
        parts.append(r"\footnotesize\textit{Notes:} Effects are percentage points per 0.1 increase in individual CCEI, holding the other member's CCEI and controls fixed locally, then averaging over this subgroup. Four-category effects sum to zero for each member. Intervals use class-clustered standard errors. The class effects, coefficients, and outcome distributions can differ across separate fits; visual comparisons are descriptive." + "\n")
        if alternative:
            explanation = ("The exact separate mutual-friendship fit did not converge; the exported status table records this failure. " if variant else
                           "The exact separate mutual-friendship fit is retained in the exported estimates for diagnostics, but not plotted here: near-perfect prediction makes those local derivatives unreliable. ")
            parts.append(explanation + "The shared-controls results depend on the pooling restriction and serve as a sensitivity check.\n")
parts.append("\\end{document}\n")
axis_low = min(-20, 10*math.floor(min(float(r["low"]) for r in effects+shared)*10))
axis_high = max(30, 10*math.ceil(max(float(r["high"]) for r in effects+shared)*10))
source = "".join(parts).replace("from $-40$ to $40$ points", f"from ${axis_low}$ to ${axis_high}$ points")
source = source.replace("All requested separate fits retain", "All successful separate fits retain")
(out / "review.tex").write_text(source)
