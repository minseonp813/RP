"""One-page comparison of normalized RP distance across joint group outcomes."""
import csv
from pathlib import Path

root = Path(__file__).resolve().parents[2]
out = root / "Code/results/new_indices/preference_distance_categories"


def read(name):
    with (out / name).open() as f:
        return list(csv.DictReader(f))


means = read("distance_means.csv")
counts = read("sample_counts.csv")
gaps = read("within_pair_gaps.csv")
contrasts = read("category_contrasts.csv")
contrast = next(r for r in contrasts if r["measure"] == "more" and r["higher"] == "4" and r["lower"] == "3")
more = {int(r["joint_category"]): float(r["mean"]) for r in means if r["role"] == "1"}
labels = [r"CCEI$<1$, CEIV$<1$", r"CCEI$=1$, CEIV$<1$", r"CCEI$<1$, CEIV$=1$", r"CCEI$=1$, CEIV$=1$"]
parts = [r"""\documentclass[11pt]{article}
\usepackage[letterpaper,landscape,margin=0.6in]{geometry}
\usepackage{booktabs,graphicx}
\setlength{\parindent}{0pt}
\setlength{\parskip}{5pt}
\begin{document}
\begin{center}\Large\textbf{Revealed-preference distance by rationality role and group outcomes}\end{center}
\small
Compare each member's normalized distance to the group, $\widehat I_{ig}$, using the same index as the paper.
Lower distance means greater relative alignment with group choices. Roles use individual CCEI separately in each wave.
\begin{center}\includegraphics[width=0.81\textwidth]{distance_by_role.png}\end{center}
\begin{center}\small\begin{tabular}{lrrrrrr}\toprule
Group outcomes & More rational & Less rational & Less minus more & Pair-waves & CCEI ties excluded & Missing excluded\\\midrule
"""]
for c, label in enumerate(labels, 1):
    m = [next(r for r in means if int(r["joint_category"]) == c and int(r["role"]) == role) for role in [1, 2]]
    n = next(r for r in counts if int(r["joint_category"]) == c)
    gap = next(r for r in gaps if int(r["joint_category"]) == c)
    parts.append(label + " & " + " & ".join(f"{float(r['mean']):.3f}" for r in m)
                 + f" & {float(gap['difference']):.3f} & {n['included_pairwaves']} & {n['tied_pairwaves']} & {n['missing_untied_pairwaves']}"
                 + r"\\" + "\n")
parts.append(r"""\bottomrule\end{tabular}\end{center}
\textbf{Reading the pattern.} The more rational member is closer to group choices in all four categories (paired
within-category differences: $p<0.001$ throughout). The difference is largest when both group indices equal one.
Within CEIV$=1$, the more rational member's mean distance is 0.261 when CCEI$=1$, compared with 0.360 when CCEI$<1$;
the difference is $-0.098$ ($p<0.001$). The less rational member's distance increases by the same amount.

\footnotesize
\textit{Notes:} Unadjusted descriptive means, pooling both waves without an RA-gap restriction. Bars are 95\% confidence
intervals clustered by 64 classes, accounting for both members and repeated waves. Among 1,304 eligible pair-waves,
199 have individual CCEIs tied within $10^{-9}$; four additional untied pair-waves lack both distances. The plotted sample
contains 1,101 pair-waves (2,202 member-waves). ``Missing excluded'' refers only to untied pairs; ties are counted separately.
Categories match Figure 6; CCEI/CEIV values at least $1-10^{-9}$ count as one.

The two distances sum to one within each pair-wave, so the two role means are complementary and reflect one comparison.
The index measures relative member-to-group closeness, not absolute disagreement between the two individuals and not
aggregation weights. A higher normalized distance for the less rational member does not establish greater absolute
distance from the group. Outcome categories and distances are constructed from related choice data; the associations
do not identify a decision process. Comparisons are exploratory, with no multiple-testing adjustment.
\end{document}
""")
source = "".join(parts).replace("0.261", f"{more[4]:.3f}").replace("0.360", f"{more[3]:.3f}")
source = source.replace("$-0.098$", f"${float(contrast['difference']):.3f}$")
(out / "review.tex").write_text(source)
