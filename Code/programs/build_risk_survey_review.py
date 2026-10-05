"""Build a local exploratory review from the Stata survey estimates and R plots."""
import csv
from pathlib import Path

root = Path(__file__).resolve().parents[2]
out = root / "Code/results/new_indices/risk_survey"
with (out / "associations.csv").open() as f:
    estimates = list(csv.DictReader(f))
names = {"distance": "Preference distance", "ccei_g": "Group CCEI", "ceiv_g": "Group CEIV"}


def result(question, outcome, sample="available"):
    return next(r for r in estimates if (r["question"], r["outcome"], r["sample"]) == (question, outcome, sample))


def pvalue(value):
    p = float(value)
    return "$<0.001$" if p < .001 else f"{p:.3f}"


def cell(r, coefficient, p):
    return f"{float(r[coefficient]):.3f} ({pvalue(r[p])})"


parts = [r"""\documentclass[11pt]{article}
\usepackage[letterpaper,landscape,margin=0.65in]{geometry}
\usepackage{booktabs,graphicx}
\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt}
\begin{document}
\begin{center}\Large\textbf{Risk-survey responses, preference distance, and group rationality}\end{center}
\textbf{Exploratory associations.} Higher reported similarity to hypothetical own choices is associated with lower
preference distance and modestly higher group CCEI and CEIV. Cooperation scores have near-zero Pearson correlations,
but small positive rank correlations with group CCEI and CEIV. Reports about whose suggestions prevailed distinguish
preference distance much more clearly than group rationality.

\begin{center}\small
\begin{tabular}{llrrrr}\toprule
Survey & Outcome & $N$ & Pearson $r$ ($p$) & Spearman $\rho$ ($p$) & Adjusted slope ($p$)\\\midrule
"""]
for q, label in [("cooperation", "Cooperation score"), ("similar", "Own-choice similarity")]:
    for k, outcome in enumerate(names):
        r = result(q, outcome)
        parts.append(f"{label if k == 0 else ''} & {names[outcome]} & {r['n']} & "
                     + " & ".join([cell(r, "pearson", "pearson_p"), cell(r, "spearman", "spearman_p"),
                                   cell(r, "adjusted_slope", "adjusted_p")]) + r"\\" + "\n")
    parts.append(r"\addlinespace" + "\n")
parts.append(r"""\bottomrule\end{tabular}\end{center}
\small
All available respondent-wave observations for each outcome. Higher cooperation scores run from 1 to 5;
similarity runs from 1 (very differently) to 4 (mostly similar). Adjusted slopes are index-point changes per response category
after wave and class fixed effects; these are descriptive checks, not Table 5 specifications. All $p$-values use standard
errors clustered by 64 classes. Spearman tests use average tied ranks and class-clustered rank regressions.

\begin{center}
\begin{tabular}{lrrr}\toprule
Whose suggestions were reflected? & Preference distance & Group CCEI & Group CEIV\\\midrule
""")
for label, field, formatter in [("Category means: $R^2$", "categorical_r2", lambda x: f"{float(x):.3f}"),
                               ("Joint test of categories: $p$", "categorical_p", pvalue)]:
    parts.append(label + " & " + " & ".join(formatter(result("whose", o)[field]) for o in names) + r"\\" + "\n")
parts.append(r"""\bottomrule\end{tabular}\end{center}
``Whose suggestions?'' is nominal: Mostly Partner's, Both, Mostly Mine, Neither. No ordinal correlation is imposed.

\textbf{Interpretation.} These results validate perceived alignment and influence more directly than a deliberation mechanism.
Cooperation provides weak support for the proposed story. Similarity can reflect preference agreement, influence, or an
easier task; it does not establish a discussion of preferences or stable aggregation weights. Group CCEI does not identify
constant weights, and CEIV does not identify turn-taking. Outcomes and reports were measured in the same wave.
""")

panels = [
    ("cooperation", "common", "Reported cooperation", "Cooperation score (coded 1 to 5)",
     "Higher coded scores indicate more cooperation. The exact questionnaire wording and answer anchors have not been recovered, so they are not reconstructed here. The distribution is concentrated at score 5; the low-score bins are small.",
     "Mean outcomes do not display a clear monotonic gradient. Small positive rank correlations for group outcomes coexist with near-zero linear associations."),
    ("similar", "common", "Similarity of hypothetical own choices", "How similar would own choices have been?",
     "Survey: If you had individually made decisions under the same choice environments as in the group experiment, how similar do you think those decisions would have been?",
     "Higher reported similarity tracks lower preference distance and somewhat higher group CCEI and CEIV. This is consistent with agreement or alignment, but the report does not reveal the process used to reach it."),
    ("whose", "common", "Whose suggestions were reflected?", "Whose suggestions were most reflected in final choices?",
     "Survey: During the collective choice experiment, whose suggestions were most reflected in the final choices? The Neither category is shaded gray, following Figure 5.",
     "The preference-distance bars and counts reproduce Figure 5(a). Group outcomes vary much less clearly across these reports; the full-sample categorical tests give p=0.098 for CCEI and p=0.339 for CEIV."),
    ("similar", "high_gap", "Similarity among pairs with high risk-preference disagreement", "Figure 5(b) sample restriction",
     "This reproduces Figure 5(b)'s restriction: the pair's absolute individually revealed risk-preference gap is at or above the pooled median in the distance-observed sample (0.126720506). It is not the wave-specific median split used in the separate heterogeneity exercise.",
     "The distance relationship is stronger in this subgroup. Mean group outcomes show weaker linear gradients than in the full sample; rank correlations remain positive. See the checks below."),
]
for q, sample, title, subtitle, wording, interpretation in panels:
    parts.append("\\clearpage\n\\begin{center}\\Large\\textbf{" + title + "}\\par\n\\normalsize " + subtitle + "\\end{center}\n")
    parts.append("\\begin{center}\n")
    for outcome in names:
        parts.append(f"\\includegraphics[width=0.326\\textwidth]{{{q}.{sample}.{outcome}.png}}\n")
    parts.append("\\end{center}\n\\small\n" + wording + "\n\n")
    n = 2560 if sample == "common" else 1280
    pairs = n // 2
    parts.append(f"\\textit{{Notes:}} Means and 95\\% confidence intervals clustered by 64 classes. All three panels use the same {n:,} respondent-wave observations ({pairs:,} pair-waves). Group CCEI and CEIV are shared by the two members; plots sort them by each member's own survey response. Counts therefore refer to respondents, not independent groups. All panels use a common 0--1 scale.\n\n")
    parts.append("\\textbf{Reading the pattern.} " + interpretation + "\n\n")
    if sample == "high_gap":
        parts.append(r"\begin{center}\begin{tabular}{lrrr}\toprule Outcome & Pearson $r$ ($p$) & Spearman $\rho$ ($p$) & Adjusted slope ($p$)\\\midrule" + "\n")
        for outcome in names:
            r = result(q, outcome, sample)
            parts.append(names[outcome] + " & " + " & ".join([cell(r, "pearson", "pearson_p"),
                cell(r, "spearman", "spearman_p"), cell(r, "adjusted_slope", "adjusted_p")]) + r"\\" + "\n")
        parts.append(r"\bottomrule\end{tabular}\end{center}" + "\n")
    else:
        parts.append("Preference distance is the current raw normalized $I_{ig}$ used in Figure 5, with lower values indicating greater alignment. Within each pair-wave, the two defined distances sum to one; it is a relative member-to-group measure, not an absolute measure of group quality. The 48 observations without distance are excluded from these plots, while the summary correlations use all available observations for each outcome.\n")
parts.append("\\end{document}\n")
(out / "review.tex").write_text("".join(parts))
