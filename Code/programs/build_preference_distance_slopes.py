"""Coefficient and composition comparison for normalized preference distance."""
import csv
from pathlib import Path

root = Path(__file__).resolve().parents[2]
out = root / "Code/results/new_indices/preference_distance_slopes"


def read(name):
    with (out / name).open() as f:
        return list(csv.DictReader(f))


slopes = read("ccei_coefficients.csv")
means = read("common_ccei_means.csv")
contrasts = read("both_minus_ceiv_only.csv")
diagnostics = read("ccei_diagnostics.csv")
labels = {1: r"CCEI$<1$, CEIV$<1$", 2: r"CCEI$=1$, CEIV$<1$",
          3: r"CCEI$<1$, CEIV$=1$", 4: r"CCEI$=1$, CEIV$=1$"}


def row(data, specification, category=None, member=None, measure=None):
    return next(r for r in data if r["specification"] == specification
                and (category is None or int(r["joint_category"]) == category)
                and (member is None or r["member"] == member)
                and (measure is None or r["measure"] == measure))


def ptext(value):
    return "$p<0.001$" if float(value) < .001 else f"$p={float(value):.3f}$"


reference_max = float(diagnostics[0]["reference_max"])
reference_min = float(diagnostics[0]["reference_min"])
parts = [r"""\documentclass[11pt]{article}
\usepackage[letterpaper,landscape,margin=0.6in]{geometry}
\usepackage{booktabs,graphicx,amsmath}
\setlength{\parindent}{0pt}
\setlength{\parskip}{5pt}
\begin{document}
\begin{center}\Large\textbf{Individual CCEI coefficients and relative distance to group choices}\end{center}
\small
Outcome: normalized distance of the more rational member to the group, $\widehat I_{\mathrm{more},g}$.
Both members' continuous CCEIs enter together, with category-specific slopes; no more/less-rational indicator enters.
\[
\widehat I_{\mathrm{more},g}=\alpha_c+\beta_{\max,c}\mathrm{CCEI}_{\max}
+\beta_{\min,c}\mathrm{CCEI}_{\min}+Z_g'\gamma+\lambda_{\mathrm{class}}+\varepsilon_g.
\]
\begin{center}\begin{minipage}{0.48\textwidth}\centering
\includegraphics[width=\linewidth]{unadjusted_coefficients.png}\\
\textbf{(a) Both CCEIs and category interactions only}
\end{minipage}\hfill\begin{minipage}{0.48\textwidth}\centering
\includegraphics[width=\linewidth]{controlled_coefficients.png}\\
\textbf{(b) Other characteristics and class fixed effects}
\end{minipage}\end{center}
\begin{center}\small\begin{tabular}{lrrrrr}\toprule
& \multicolumn{2}{c}{CCEI-only specification} & \multicolumn{2}{c}{With other controls} & Pair-waves\\
Group outcomes & Maximum CCEI & Minimum CCEI & Maximum CCEI & Minimum CCEI &\\\midrule
"""]
for c in range(1, 5):
    cells = []
    for specification in ["unadjusted", "controlled"]:
        for member in ["max", "min"]:
            r = row(slopes, specification, c, member)
            cells.append(f"{float(r['estimate']):.3f} ({float(r['se']):.3f})")
    n = row(slopes, "controlled", c, "max")["category_n"]
    parts.append(labels[c] + " & " + " & ".join(cells) + f" & {n}" + r"\\" + "\n")
parts.append(r"""\bottomrule\end{tabular}\end{center}
\textbf{Interpretation.} Increasing maximum CCEI predicts a smaller distance for the more rational member.
Increasing minimum CCEI predicts a larger distance for that member, hence a smaller distance for the less rational member.
Because the two distances sum to one, an identically specified regression for the less rational member has opposite
coefficients and identical standard errors. These signs describe relative alignment, not changes in absolute distance.

\footnotesize
\textit{Notes:} Table and chart coefficients are rescaled to a 0.1 increase in individual CCEI; multiply by ten for
the equation's one-unit coefficients. Parentheses contain class-clustered standard errors; bars are 95\% intervals.
Both specifications fit all 1,101 untied pair-waves with observed distances, pooling both waves, with 64 class clusters.
Controls are the Figure 6 student/personality and friendship/network controls, plus class effects, sharing control
coefficients across categories. Individual choice-share and RA controls are excluded; no wave fixed effect enters.
Near-one CCEIs have limited room to increase, so the coefficient scaling is not a feasible 0.1 change for every observation.
Categories are defined by group outcomes; coefficients are descriptive conditional associations.
""")
parts.append(r"""\clearpage
\begin{center}\Large\textbf{Are category differences driven by different individual CCEI levels?}\end{center}
\small
Evaluate each category's fitted distance at the same two individual CCEIs. The reference values are the pooled means
among the same 1,101 included pair-waves:
""" + f"$\\mathrm{{CCEI}}_{{\\max}}={reference_max:.3f}$ and $\\mathrm{{CCEI}}_{{\\min}}={reference_min:.3f}$.\n")
parts.append(r"""The CCEI-only specification changes just these two levels. The controlled specification also averages other
characteristics and class effects over their pooled distribution. Slopes remain category-specific.

\begin{center}\begin{tabular}{lrrrrr}\toprule
& \multicolumn{2}{c}{Observed individual CCEI means} & \multicolumn{3}{c}{More rational member's distance}\\
Group outcomes & Maximum & Minimum & Raw mean & Common CCEIs & Common CCEIs + controls\\\midrule
""")
for c in range(1, 5):
    d = next(r for r in diagnostics if int(r["joint_category"]) == c)
    values = [float(d["mean_max"]), float(d["mean_min"])]
    values += [float(row(means, specification, c)["estimate"]) for specification in ["raw", "unadjusted", "controlled"]]
    parts.append(labels[c] + " & " + " & ".join(f"{v:.3f}" for v in values) + r"\\" + "\n")
parts.append(r"""\bottomrule\end{tabular}\end{center}
\textbf{Within CEIV$=1$: both indices equal one minus CEIV-only.}
\begin{center}\begin{tabular}{lrrr}\toprule
Comparison & Distance difference & 95\% interval & $p$\\\midrule
""")
for specification, label in [("raw", "Raw means"), ("unadjusted", "Common individual CCEIs"),
                             ("controlled", "Common CCEIs and other characteristics")]:
    r = row(contrasts, specification, measure="mean")
    p = "$<0.001$" if float(r["p"]) < .001 else f"{float(r['p']):.3f}"
    parts.append(f"{label} & {float(r['difference']):.3f} & [{float(r['low']):.3f}, {float(r['high']):.3f}] & {p}"
                 + r"\\" + "\n")
raw = float(row(contrasts, "raw", measure="mean")["difference"])
adjusted = float(row(contrasts, "unadjusted", measure="mean")["difference"])
parts.append(r"\bottomrule\end{tabular}\end{center}" + "\n")
parts.append(f"\\textbf{{Finding.}} The distance contrast changes from {raw:.3f} to {adjusted:.3f} when individual CCEIs are set to common levels. Most of the observed gap remains in this linear specification; differences in the distribution of members' CCEIs account for little of it. This is a model-based comparison, not a causal decomposition.\n\n")
max_difference = row(contrasts, "controlled", measure="max_slope")
min_difference = row(contrasts, "controlled", measure="min_slope")
parts.append("\\textbf{Slope comparison.} Within CEIV$=1$, the maximum-CCEI slope is more negative when group CCEI also equals one (difference "
             + f"{float(max_difference['difference']):.3f} per 0.1, {ptext(max_difference['p'])}). The minimum-CCEI slope difference is small and imprecise ({ptext(min_difference['p'])}).\n\n")
parts.append(r"""\footnotesize
\textit{Notes:} Roles are defined separately in each wave; individual CCEI ties within $10^{-9}$ are excluded, as in
the preceding distance comparison. Four additional untied pair-waves have undefined distances. CCEI/CEIV values at
least $1-10^{-9}$ count as one. The common CCEI values are within observed joint support in each category. Predicted
distances for the less rational member are one minus those shown. Class-clustered uncertainty treats the chosen
reference CCEI values as fixed; no uncertainty is added for estimating their pooled means. The linear model allows
category-specific intercepts and CCEI slopes, and common coefficients for additional controls. These are comparisons
of groups defined by outcomes, with related revealed-preference measures computed from the same choice data;
they do not identify aggregation weights or a communication mechanism. Tests are exploratory and unadjusted for
multiple comparisons.
\end{document}
""")
(out / "review.tex").write_text("".join(parts))
