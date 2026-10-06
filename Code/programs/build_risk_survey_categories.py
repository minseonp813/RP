"""Assemble the four-category survey review from class-clustered Stata estimates."""
import csv
from pathlib import Path

root = Path(__file__).resolve().parents[2]
out = root / "Code/results/new_indices/risk_survey_categories"


def read(name):
    with (out / name).open() as f:
        return list(csv.DictReader(f))


means = read("score_means.csv")
shares = read("response_shares.csv")
tests = read("category_tests.csv")
counts = read("category_counts.csv")
labels = {1: r"CCEI$<1$, CEIV$<1$", 2: r"CCEI$=1$, CEIV$<1$",
          3: r"CCEI$<1$, CEIV$=1$", 4: r"CCEI$=1$, CEIV$=1$"}
titles = {"cooperation": "Reported cooperation", "similar": "Similarity of hypothetical own choices",
          "whose": "Whose suggestions were reflected?"}
answers = {"cooperation": ["1", "2", "3", "4", "5"],
           "similar": ["Very differently", "Somewhat differently", "Somewhat similar", "Mostly similar"],
           "whose": ["Mostly partner's", "Both", "Mostly mine", "Neither"]}


def row(data, q, category=None, response=None, sample="all", **fields):
    return next(r for r in data if r["question"] == q
                and r["sample"] == sample
                and (category is None or int(r["joint_category"]) == category)
                and (response is None or int(r["response"]) == response)
                and all(r[k] == v for k, v in fields.items()))


def pvalue(value):
    return "$<0.001$" if float(value) < .001 else f"{float(value):.3f}"


def ptext(value):
    return "$p<0.001$" if float(value) < .001 else f"$p={float(value):.3f}$"


def test(q, measure, comparison, sample="all"):
    return row(tests, q, sample=sample, measure=measure, comparison=comparison)


def summary_table(sample):
    field = "pairwaves" if sample == "all" else "untied_pairwaves"
    people = "Respondents" if sample == "all" else "Lower-CCEI members"
    content = [r"\begin{center}\small\begin{tabular}{clrrrrr}\toprule" + "\n",
               f"Category & Group outcomes & Pair-waves & {people} & Cooperation & Similarity & ``Both''" + r"\\" + "\n",
               r"& & & with answers & Mean (1--5) & Mean (1--4) & Share (\%)\\\midrule" + "\n"]
    for c in range(1, 5):
        n = next(r[field] for r in counts if int(r["joint_category"]) == c)
        coop = row(means, "cooperation", c, sample=sample)
        similar = row(means, "similar", c, sample=sample)
        both = row(shares, "whose", c, 2, sample=sample)
        content.append(f"{c} & {labels[c]} & {n} & {coop['n']} & {float(coop['mean']):.3f} & "
                       + f"{float(similar['mean']):.3f} & {100*float(both['share']):.1f}" + r"\\" + "\n")
    content.append(r"\bottomrule\end{tabular}\end{center}" + "\n")
    return content


def contrast_table(sample):
    content = [r"""\textbf{Relevant contrast: category 4 minus category 3.} Both categories have CEIV$=1$;
they differ in whether group CCEI also equals one. This comparison does not hold other pair characteristics fixed.
\begin{center}\small
\begin{tabular}{lrrr}\toprule
Survey measure & Difference & 95\% confidence interval & $p$\\\midrule
"""]
    for q, measure, label, scale in [("cooperation", "mean_score", "Cooperation score", 1),
                                    ("similar", "mean_score", "Similarity score", 1),
                                    ("whose", "response_2", "Both suggestions reflected (percentage points)", 100)]:
        r = test(q, measure, "both_minus_ceiv", sample)
        content.append(f"{label} & {scale*float(r['difference']):.3f} & "
                       + f"[{scale*float(r['low']):.3f}, {scale*float(r['high']):.3f}] & {pvalue(r['p'])}" + r"\\" + "\n")
    content.append(r"\bottomrule\end{tabular}\end{center}" + "\n")
    return content


parts = [r"""\documentclass[11pt]{article}
\usepackage[letterpaper,landscape,margin=0.65in]{geometry}
\usepackage{booktabs,graphicx}
\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt}
\begin{document}
\begin{center}\Large\textbf{Risk-survey responses across four group CCEI-CEIV categories}\end{center}
\textbf{Sample.} All available risk-survey responses from both waves, with no restriction on the members' risk-preference
gap. Classification matches Figure 6: indices within $10^{-9}$ of one count as one. CEIV classifications agree at both
supplied numerical bounds. The 1,304 pair-wave observations supply 2,608 respondent-waves; one respondent has missing
answers to all three questions, leaving 2,607 responses for each question.

"""]
parts.extend(summary_table("all"))
parts.extend(contrast_table("all"))
parts.append(r"""
\small
\textbf{Reading the pattern.} Groups with both indices equal to one report somewhat more cooperation and greater
similarity to hypothetical own choices. They do not report ``Both'' suggestions more often than CEIV-only groups.
These findings support perceived alignment more directly than a particular method of preference aggregation.
The survey cannot identify a conversation about preferences, stable weights, or turn-taking; group CCEI and CEIV do
not identify those processes either.

\textit{Inference:} Descriptive, unadjusted comparisons with standard errors clustered by 64 classes. Ordinal means assume
equal spacing between coded responses and are supplemented by full response distributions. No mean is assigned to the
nominal whose-suggestions question. Reported $p$-values are exploratory and have no multiple-testing adjustment.
""")

wording = {
    "cooperation": "Cooperation uses coded scores 1 to 5, with higher scores indicating greater cooperation. Exact original wording and answer anchors have not been recovered and are not reconstructed here.",
    "similar": "Survey: If you had individually made decisions under the same choice environments as in the group experiment, how similar do you think those decisions would have been? Scores run from 1 (very differently) to 4 (mostly similar).",
    "whose": "Survey: During the collective choice experiment, whose suggestions were most reflected in the final choices? Responses are nominal; neither their mean nor an ordinal correlation is reported.",
}

def survey_pages(sample):
    content = []
    prefix = "" if sample == "all" else "less_"
    short_titles = {"cooperation": "cooperation", "similar": "own-choice similarity", "whose": "whose suggestions?"}
    for q in titles:
        title = titles[q] if sample == "all" else "Less rational members: " + short_titles[q]
        content.append("\\clearpage\n\\begin{center}\\Large\\textbf{" + title + "}\\end{center}\n")
        content.append("\\begin{center}\n")
        if q == "whose":
            content.append(f"\\includegraphics[width=0.58\\textwidth]{{{prefix}{q}_distribution.png}}\n")
        else:
            content.append(f"\\includegraphics[width=0.595\\textwidth]{{{prefix}{q}_distribution.png}}\n")
            content.append(f"\\includegraphics[width=0.395\\textwidth]{{{prefix}{q}_mean.png}}\n")
        content.append("\\end{center}\n\\small\n" + wording[q] + "\n\n")
        content.append("\\begin{center}\\begin{tabular}{cl" + "r"*len(answers[q]) + "}\\toprule\n")
        content.append("Category & Group outcomes & " + " & ".join(answers[q]) + r"\\\midrule" + "\n")
        for c in range(1, 5):
            percentages = [f"{100*float(row(shares,q,c,r,sample=sample)['share']):.1f}" for r in range(1, len(answers[q])+1)]
            content.append(f"{c} & {labels[c]} & " + " & ".join(percentages) + r"\\" + "\n")
        content.append("\\bottomrule\\end{tabular}\\end{center}\n")
        ns = ", ".join(row(shares, q, c, 1, sample=sample)["total"] for c in range(1, 5))
        selection = ("Category 4 has one missing respondent; all other responses are observed. " if sample == "all"
                     else "One lower-CCEI member per untied pair-wave; all 1,105 selected members have observed answers. CCEI ties within $10^{-9}$ are excluded. ")
        all_p = ptext(test(q, "distribution", "all_categories", sample)["p"])
        contrast_p = ptext(test(q, "distribution", "both_vs_ceiv", sample)["p"])
        content.append("\\textit{Notes:} Table entries are percentages within each group category. Response counts are " + ns + ". " + selection
                       + "Joint class-clustered tests of equal response distributions: all four categories, " + all_p
                       + "; categories 4 and 3, " + contrast_p
                       + ". Tests use stacked linear probability models for all but one response indicator, with unrestricted category-by-response means.\n")
        if q == "whose":
            both4 = 100*float(row(shares,q,4,2,sample=sample)["share"])
            both3 = 100*float(row(shares,q,3,2,sample=sample)["share"])
            both_p = ptext(test(q,"response_2","both_minus_ceiv",sample)["p"])
            content.append(f"``Both'' is reported by {both4:.1f}\\% in category 4 and {both3:.1f}\\% in category 3 ({both_p}); this difference is not statistically distinguishable from zero.\n")
        else:
            r = test(q, "mean_score", "both_minus_ceiv", sample)
            content.append(f"Category 4 minus category 3 in mean score: {float(r['difference']):.3f} ({ptext(r['p'])}). Mean-score intervals are 95\\% t intervals using class-clustered standard errors.\n")
    return content


parts.extend(survey_pages("all"))
parts.append(r"""\clearpage
\begin{center}\Large\textbf{Risk-survey responses: less rational member only}\end{center}
\textbf{Selection.} Keep the member with strictly lower individual CCEI in each pair and wave, using a $10^{-9}$ tie tolerance.
All 199 ties are exact ties in the stored data. These pair-waves have no uniquely less rational member and are excluded:
37, 17, 31, and 114 in categories 1--4, respectively. The remaining 1,105 lower-CCEI members have observed answers to
all three questions. Ranking is wave-specific, so the selected member can change between waves. No RA-gap restriction
is applied, and group categories are defined exactly as in the preceding section.

""")
parts.extend(summary_table("less"))
parts.extend(contrast_table("less"))
parts.append(r"""\small
\textbf{Reading the pattern.} Among less rational members, similarity to hypothetical own choices remains higher in
category 4 than category 3 (0.222 points, $p<0.001$). Cooperation is also higher (0.150 points), but its confidence interval
includes zero ($p=0.067$). ``Both'' suggestions being reflected shows little difference ($p=0.777$). This is consistent
with greater perceived alignment, but does not establish stable aggregation weights or a conversation about preferences.

\textit{Notes:} Descriptive, unadjusted comparisons clustered by 64 classes, without multiple-testing adjustment.
Cooperation and similarity means assume equal spacing between coded responses. The nominal whose-suggestions answers
are shown as distributions. Comparisons with the all-member section reflect both selecting the lower-CCEI member and
excluding tied pair-waves; tied pairs are disproportionately represented in category 4.
""")
parts.extend(survey_pages("less"))
parts.append("\\end{document}\n")
(out / "review.tex").write_text("".join(parts))
