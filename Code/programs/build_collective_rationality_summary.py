"""Assemble the Section 6.3 figures and three-column appendix table."""
import csv
from pathlib import Path

root = Path(__file__).resolve().parents[2]
out = root / "Code/results/new_indices/collective_rationality_summary"


def read(name):
    with (out / name).open() as stream:
        return list(csv.DictReader(stream))


coefficients = read("table5_coefficients.csv")
diagnostics = read("table5_diagnostics.csv")
means = read("group_ccei_category_means.csv")
counts = read("joint_outcome_counts.csv")
effects = read("figure6_ame.csv")
assert sum(int(row["n"]) for row in counts) == 1304
for row in effects:
    assert abs(float(row["share"]) - float(counts[int(row["outcome"]) - 1]["share"])) < 1e-12


def coefficient(outcome, column, term):
    return next(r for r in coefficients if r["outcome"] == outcome
                and int(r["column"]) == column and r["term"] == term)


def diagnostic(outcome, column):
    return next(r for r in diagnostics if r["outcome"] == outcome and int(r["column"]) == column)


def stars(p):
    return "**" if p < .01 else "*" if p < .05 else "+" if p < .1 else ""


def pvalue(value):
    return r"$<0.001$" if float(value) < .001 else f"{float(value):.3f}"


def table_row(label, values):
    return label + " & " + " & ".join(values) + r"\\" + "\n"


quadrant_summary = "; ".join(
    f"{label}: {int(row['n']):,} ({100 * float(row['share']):.1f}\\%)"
    for label, row in zip(["both below one", "only CCEI equal to one",
                           "only CEIV equal to one", "both equal to one"], counts)
)
comparison_summary = "\n".join(
    label + ": High--High minus Low--High comparison $p$-values are "
    + ", ".join(pvalue(diagnostic(outcome, c)["equality_p"]) for c in range(1, 4))
    + " in columns (1)--(3), respectively."
    for outcome, label in [("ccei", "Group CCEI"), ("ceiv", "Group CEIV")]
)

parts = [r"""\documentclass[11pt]{article}
\usepackage[letterpaper,margin=0.6in]{geometry}
\usepackage{booktabs,graphicx,amsmath,amssymb}
\setlength{\parindent}{0pt}
\setlength{\parskip}{5pt}
\pagestyle{plain}
\begin{document}
\begin{center}\large\textbf{Section 6.3: Individual Rationality and Collective Outcomes}\\[4pt]
\normalsize Revised figure sequence and appendix placement\end{center}
\begin{enumerate}
\setlength{\itemsep}{10pt}
\item \textbf{Bar and CDF panels (Figure 1).} Show group CCEI and CEIV by members' individual CCEI
categories, using the pooled median across both waves (0.9806594). High means strictly above this median
and Low otherwise. The CDF panels complement the mean comparisons
by showing the full distributions, including the mass at one.

\item \textbf{Four joint outcomes (Figure 2).} Place group CCEI status on the horizontal axis and group CEIV
status on the vertical axis. Each quadrant reports its count and share of the 1,304 pair-wave observations:
""" + quadrant_summary + r""". These are categorical quadrants, with each index either below one or equal to one.

\item \textbf{Average marginal effects (Figure 3).} Use the pooled four-outcome multinomial logit with both
members' individual CCEIs, student, friendship, and choice-pattern controls, and class fixed effects.
For a 0.1 increase in maximum individual CCEI, the average marginal effect on the probability of both group
indices equaling one is +19.2 percentage points; the corresponding minimum-CCEI effect is +2.4 percentage
points. Both are positive with 95\% confidence intervals excluding zero. Minimum individual CCEI also has
a positive effect on the CCEI$=1$, CEIV$<1$ outcome. These estimates describe conditional associations;
the plot does not test equality of the two members' effects.
\end{enumerate}

\textbf{Appendix Table A1.} Retain only columns (1)--(3) of the attached table, with both group CCEI and CEIV
panels. Column (1) includes class fixed effects; column (2) adds student, friendship, and choice-pattern controls;
column (3) uses these controls with pair fixed effects. Low--Low is the omitted category.
""" + comparison_summary + r"""

\textit{Measurement and sample:} CEIV is used throughout. All figures and appendix regressions use
1,304 pair-waves from 652 pairs in 64 classes. CEIV$=1$ denotes rationalizability under the specified
individual calibrations and varying weights; it does not identify actual aggregation weights or a communication
process. Figure and table numbers in this review packet indicate the proposed sequence within Section 6.3.

\clearpage
\begin{center}\large\textbf{Figure 1: Collective CCEI and CEIV}\\
\normalsize By members' individual CCEI category\end{center}
\begin{center}
\begin{minipage}{0.485\textwidth}\centering
\includegraphics[width=\linewidth]{group_ccei_bar.pdf}\\[-2pt]
(a) Mean collective CCEI
\end{minipage}\hfill
\begin{minipage}{0.485\textwidth}\centering
\includegraphics[width=\linewidth]{group_ccei_cdf.pdf}\\[-2pt]
(b) CDFs of collective CCEI
\end{minipage}

\vspace{3pt}
\begin{minipage}{0.485\textwidth}\centering
\includegraphics[width=\linewidth]{group_ceiv_bar.pdf}\\[-2pt]
(c) Mean collective CEIV
\end{minipage}\hfill
\begin{minipage}{0.485\textwidth}\centering
\includegraphics[width=\linewidth]{group_ceiv_cdf.pdf}\\[-2pt]
(d) CDFs of collective CEIV
\end{minipage}
\end{center}
\footnotesize
\textit{Notes:} Each member is High if individual CCEI exceeds the pooled median across both waves
(0.9806594), and Low otherwise; no individual CCEI equals the median. The sample contains 1,304 pair-waves
from 652 pairs. Low--Low, Low--High, and High--High contain
""" + ", ".join(r["n"] for r in means) + r""" pair-waves, respectively.
Bars show unadjusted means with 95\% Student-$t$ confidence intervals. Bracket labels are upper-category
minus lower-category means; significance uses Welch two-sample $t$-tests. CDFs use all observations,
including the mass at one. These descriptive intervals and tests do not adjust for class clustering;
Figure 3 and Appendix Table A1 do. +, *, and ** denote $p<0.10$, $p<0.05$, and $p<0.01$.

\clearpage
\normalsize
\begin{center}\large\textbf{Figure 2: Joint Collective Outcomes}\\
\normalsize Counts and shares by group CCEI and CEIV status\end{center}
\begin{center}
\includegraphics[width=0.95\textwidth]{joint_outcome_quadrants.pdf}
\end{center}
\footnotesize
\textit{Notes:} The horizontal axis classifies group CCEI and the vertical axis classifies group CEIV.
Diagonal cells are blue and off-diagonal cells are white; stripes and dots distinguish outcomes within each color.
Each quadrant reports the observed count and percentage of all 1,304 pair-waves from 652 pairs in 64 classes;
each pair contributes one observation in each of two waves. These are pooled pair-wave frequencies,
rather than a classification of unique pairs. Percentages sum to 100\% before rounding.
An index is classified as one at values at least $1-10^{-9}$; all other observations are below one.
CEIV endpoint classifications agree with both supplied numerical bounds. The axes represent binary status,
rather than distances between continuous index values. CEIV$=1$ denotes rationalizability under the specified
individual calibrations and varying weights.

\clearpage
\normalsize
\begin{center}\large\textbf{Figure 3: Individual Rationality and Joint Collective Outcomes}\end{center}
\begin{center}
\begin{minipage}{0.82\textwidth}\centering
\includegraphics[width=\linewidth]{figure6_a_ame_maximum.png}\\
(a) $\mathrm{CCEI}_{\max,gt}$
\end{minipage}

\vspace{4pt}
\begin{minipage}{0.82\textwidth}\centering
\includegraphics[width=\linewidth]{figure6_a_ame_minimum.png}\\
(b) $\mathrm{CCEI}_{\min,gt}$
\end{minipage}
\end{center}
\footnotesize
\textit{Notes:} Pooled four-category multinomial-logit average marginal effects, expressed in percentage points
and scaled to a 0.1 increase in the indicated member's individual CCEI. These are scaled average derivatives,
rather than exact finite changes in predicted probabilities. Both individual CCEIs enter jointly, with student,
friendship, and corner/midpoint share controls and class fixed effects. The control set matches Appendix
Table A1, Column (2); risk-aversion controls and wave fixed effects are excluded.
The sample is 1,304 pair-waves from 652 pairs in 64 classes. Horizontal bars are 95\% normal confidence intervals
based on class-clustered standard errors. Effects sum to zero across the four outcomes for each member.
An index is classified as one within a numerical tolerance of $10^{-9}$. The joint-outcome counts and shares
appear in Figure 2. CEIV$=1$ denotes rationalizability under the specified individual calibrations and varying
weights; it does not identify actual aggregation weights or a communication process.
The effects describe conditional associations.

\clearpage
\normalsize
\setlength{\parskip}{3pt}
\begin{center}\large\textbf{Appendix Table A1: Individual Rationality and Collective Outcomes}\\
\normalsize Columns (1)--(3) of the attached table\end{center}
\begin{center}\small
\setlength{\tabcolsep}{14pt}
\renewcommand{\arraystretch}{1.12}
"""]

table = [r"""
\begin{tabular}{lccc}
\toprule
& \multicolumn{3}{c}{Individual CCEI category dummies}\\
\cmidrule(lr){2-4}
& (1) & (2) & (3)\\
\midrule
"""]

terms = [("low_high", "Low--High pair"), ("high_high", "High--High pair")]
for outcome, panel in [("ccei", "A: Group CCEI"), ("ceiv", "B: Group CEIV")]:
    table.append(r"\multicolumn{4}{l}{\textit{Panel " + panel + r"}}\\[3pt]" + "\n")
    for term, label in terms:
        rows = [coefficient(outcome, column, term) for column in range(1, 4)]
        table.append(table_row(label, [f"{float(r['estimate']):.3f}" + r"\textsuperscript{"
                                     + stars(float(r["p"])) + "}" for r in rows]))
        table.append(table_row("", [f"({float(r['se']):.3f})" for r in rows]))
    table.append(r"\midrule\multicolumn{4}{l}{\textit{Post-estimation comparisons}}\\[2pt]" + "\n")
    contrasts = [coefficient(outcome, column, "hh_minus_lh") for column in range(1, 4)]
    table.append(table_row("High--High minus Low--High", [f"{float(r['estimate']):.3f}" for r in contrasts]))
    table.append(table_row("Standard error of difference", [f"{float(r['se']):.3f}" for r in contrasts]))
    table.append(table_row(r"$p$: High--High = Low--High",
                           [pvalue(diagnostic(outcome, c)["equality_p"]) for c in range(1, 4)]))
    table.append(r"\addlinespace" + "\n")
    table.append(table_row("Observations", [f"{int(diagnostic(outcome, c)['n']):,}" for c in range(1, 4)]))
    table.append(table_row(r"$R^2$", [f"{float(diagnostic(outcome, c)['r2']):.3f}" for c in range(1, 4)]))
    table.append(r"\midrule" + "\n")

table.extend([
    table_row("Fixed effects", ["Class", "Class", "Pair"]),
    table_row("Student and friendship controls", ["", r"$\checkmark$", r"$\checkmark$"]),
    table_row("Corner/midpoint share controls", ["", r"$\checkmark$", r"$\checkmark$"]),
    "\\bottomrule\n\\end{tabular}\n",
])
table_tex = "".join(table)
(out / "collective_ccei_ceiv_categories.tex").write_text(table_tex)
parts.extend([table_tex, r"""\end{center}
\footnotesize
\textit{Notes:} OLS regression coefficients have class-clustered standard errors in parentheses. All columns
use 1,304 pair-waves from 652 pairs in 64 classes. Low--Low is omitted. High means individual CCEI exceeds
the pooled median across both waves (0.9806594), and Low otherwise, as in Figure 1. Column (1) includes
class fixed effects; Column (2) adds full controls with class fixed effects; Column (3) uses full controls
with pair fixed effects.

The post-estimation section reports the linear contrast $\beta_{HH}-\beta_{LH}$ and its class-clustered
standard error; the contrast is not an additional regressor. Its two-sided $p$-value tests
$H_0:\beta_{HH}=\beta_{LH}$ using the estimated coefficient covariance.

Full controls comprise pair maxima and absolute differences in math score, height, Big Five traits,
network degree, popularity, and exact corner/equal-allocation shares, with mixed-gender, friendship,
and missing-value indicators. Risk-aversion controls and wave fixed effects are excluded.
Estimates and category comparisons describe conditional associations. +, *, and ** denote significance
at the 10\%, 5\%, and 1\% levels.
\end{document}
""",
])
(out / "collective_rationality_summary.tex").write_text("".join(parts))
