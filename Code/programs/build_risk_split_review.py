"""Assemble the CCEI/CEIV review split at wave-specific risk-preference-gap medians."""
import csv
import re
from pathlib import Path

root = Path(__file__).resolve().parents[2]
out = root / "Code/results/new_indices/risk_split"


def rows(name):
    with (out / name).open() as f:
        return list(csv.DictReader(f))


summary = rows("split_summary.csv")
medians = {int(float(r["post"])): float(r["median_ra"]) for r in summary}
effects = rows("figure6_ame.csv")
differences = rows("slope_differences.csv")

header = r"""\documentclass[11pt]{article}
\usepackage[margin=0.7in]{geometry}
\usepackage{booktabs,amsmath,amssymb,graphicx,subcaption}
\newcommand{\sym}[1]{\ifmmode^{#1}\else\(^{#1}\)\fi}
\setlength{\parindent}{0pt}
\begin{document}
\begin{center}\large\textbf{Risk-preference similarity: group CCEI and CEIV}\end{center}
\begin{center}\small
\begin{tabular}{lccc}\toprule
& Full sample & Similar risk preferences & Different risk preferences\\
& & Below wave median & Above wave median\\
TABLE
Class fixed effects & $\checkmark$ & $\checkmark$ & $\checkmark$\\
Student and friendship controls & $\checkmark$ & $\checkmark$ & $\checkmark$\\
Corner/midpoint share controls & $\checkmark$ & $\checkmark$ & $\checkmark$\\
\bottomrule
\end{tabular}\end{center}
\small
\textit{Notes:} Reproduces Table 5 column (3) for group CCEI and CEIV, separately
by similarity of the members' individually revealed risk preferences.
The gap is $|RA_i-RA_j|$, where $RA_m$ is the member's mean share of the allocation
to the more expensive security over her 18 individual choices in that wave.
The median is computed across 652 pairs separately within each wave:
MEDIANS. There are no missing gaps or median ties. Each subgroup contains 326
pair-waves per wave, totaling 652; pairs can switch subgroups across waves.
All models include both individual CCEIs, class fixed effects, and the original
student, friendship, and corner/midpoint-share controls. Risk-aversion controls
are excluded, matching column (3). Standard errors are clustered by 64 classes
and reported in parentheses. +, *, and ** indicate $p<0.10$, $p<0.05$, and $p<0.01$.

\medskip
\textbf{Tests of subgroup coefficient differences}\par
\begin{center}
\begin{tabular}{llrr}\toprule
Outcome & Individual CCEI & Above minus below & $p$-value\\\midrule
DIFFERENCES
\bottomrule\end{tabular}\end{center}
\textit{Notes:} Class-clustered Wald tests from a pooled OLS regression that fully
interacts subgroup membership with all regressors and class fixed effects.
These tests account for dependence across the two subgroups within a class.
Differences in significance across separate models alone do not establish
significant heterogeneity. The risk-preference split is descriptive and uses
the same individual choices used to measure individual CCEI.
"""
test_rows = "\n".join(
    f"{r['outcome'].upper()} & "
    + (r"$\text{CCEI}_{\text{max}}$" if r["member"] == "max" else r"$\text{CCEI}_{\text{min}}$")
    + f" & {float(r['difference']):.3f} & {float(r['p']):.3f}" + r"\\"
    for r in differences
)
header = header.replace("MEDIANS", f"{medians[0]:.6f} at baseline and {medians[1]:.6f} at endline")
header = header.replace("DIFFERENCES", test_rows)
table = (out / "table5_split.tex").read_text().replace(r"\_", "_")
header = header.replace("TABLE", table.replace("\n\n", "\n"))

figures = []
for split, name in [(0, "similar"), (1, "different")]:
    sample = [r for r in effects if int(r["ra_high"]) == split]
    size = sample[0]
    shares = {int(r["outcome"]): 100 * float(r["share"]) for r in sample}
    title = "Similar risk preferences: below the wave median" if split == 0 else "Different risk preferences: above the wave median"
    figure = r"""\clearpage
\begin{center}\large\textbf{Figure 6: group CCEI and CEIV}\par
\normalsize TITLE\end{center}
\begin{figure}[ht]\centering
\begin{subfigure}[b]{0.485\textwidth}\centering
\includegraphics[width=\textwidth]{figure6_a_NAME_ame_maximum.png}
\caption{$\text{CCEI}_{\text{max},gt}$}\end{subfigure}\hfill
\begin{subfigure}[b]{0.485\textwidth}\centering
\includegraphics[width=\textwidth]{figure6_a_NAME_ame_minimum.png}
\caption{$\text{CCEI}_{\text{min},gt}$}\end{subfigure}
\end{figure}
\small
\textit{Notes:} Separate four-category multinomial-logit model in the indicated
risk-preference-gap subgroup, using Table 5 column (3)'s controls and class fixed
effects. Both maximum and minimum individual CCEI enter jointly. Risk-aversion
controls are excluded. Plotted average marginal effects are in percentage points
for a 0.1 increase in individual CCEI; they are scaled average derivatives,
not exact finite probability changes. Error bars are 95\% confidence intervals
using class-clustered standard errors.

\medskip
Sample: N pair-waves from PAIRS distinct pairs in CLUSTERS classes.
The gap $|RA_i-RA_j|$ is classified within each wave using medians MEDIANS;
there are 326 observations per wave in this subgroup.
Category shares, from top to bottom: SHARES.
Classification uses the supplied indices and the original $10^{-9}$ tolerance;
CEIV classifications are unchanged across their supplied numerical bounds.
Both subgroup figures use a common horizontal scale covering all confidence
intervals. The blue/red member colors and panel styling follow Figure 6.
"""
    replacements = {
        "TITLE": title, "NAME": name, "N": size["n"], "PAIRS": size["pairs"],
        "CLUSTERS": size["clusters"], "MEDIANS": f"{medians[0]:.6f} and {medians[1]:.6f}",
        "SHARES": ", ".join(f"{shares[k]:.2f}\\%" for k in range(1, 5)),
    }
    # Replace whole placeholder tokens so short names cannot alter prose or commands.
    figure = re.sub(r"\b(TITLE|NAME|N|PAIRS|CLUSTERS|MEDIANS|SHARES)\b",
                    lambda m: replacements[m[0]], figure)
    # Filename placeholders adjoin underscores and are therefore not word-delimited.
    figure = figure.replace("_NAME_", f"_{name}_")
    figures.append(figure)

(out / "review.tex").write_text(header + "".join(figures) + "\\end{document}\n")
