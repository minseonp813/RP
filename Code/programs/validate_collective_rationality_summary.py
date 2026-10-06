"""Independent within-OLS and cluster-covariance check of all twelve table models."""
from pathlib import Path
import subprocess
import numpy as np
import pandas as pd

root = Path(__file__).resolve().parents[2]
out = root / "Code/results/new_indices/collective_rationality_summary"
data = pd.read_stata(out / "analysis_sample.dta", convert_categoricals=False)
coefficients = pd.read_csv(out / "table5_coefficients.csv")
diagnostics = pd.read_csv(out / "table5_diagnostics.csv")
controls = (
    "mathscore_max mathscore_dist height_max height_dist male_diff outgoing_max outgoing_dist "
    "opened_max opened_dist agreeable_max agreeable_dist conscientious_max conscientious_dist "
    "stable_max stable_dist mathscore_diff_missing outgoing_diff_missing opened_diff_missing "
    "agreeable_diff_missing conscientious_diff_missing stable_diff_missing "
    "inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist friend "
    "corner_share_max corner_share_dist mid_share_max mid_share_dist"
).split()
records = []
for row in diagnostics.itertuples(index=False):
    specification = (row.column - 1) % 3 + 1
    terms = ["low_high", "high_high"] if row.column <= 3 else ["ccei_max", "ccei_dist"]
    columns = terms + (controls if specification > 1 else [])
    fe = "pair_fe" if specification == 3 else "class_fe"
    x = data[columns].astype(float)
    y = data[row.outcome + "_g"].astype(float)
    within_x = (x - x.groupby(data[fe]).transform("mean")).to_numpy()
    within_y = (y - y.groupby(data[fe]).transform("mean")).to_numpy()
    beta, _, rank, _ = np.linalg.lstsq(within_x, within_y, rcond=None)
    assert rank == row.df_m
    residual = within_y - within_x @ beta
    bread = np.linalg.pinv(within_x.T @ within_x, rcond=1e-12)
    scores = within_x * residual[:, None]
    cluster_scores = pd.DataFrame(scores).groupby(data.class_fe).sum().to_numpy()
    # Absorbed effects are nested within the clusters, so reghdfe excludes them
    # from its parameter penalty; the constant still contributes one parameter.
    covariance = bread @ (cluster_scores.T @ cluster_scores) @ bread
    covariance *= row.clusters / (row.clusters - 1) * (row.n - 1) / (row.n - rank - 1)
    se = np.sqrt(np.maximum(np.diag(covariance), 0))
    contrast = np.zeros(len(columns))
    contrast[:2] = [-1, 1] if row.column <= 3 else [1, 2]
    contrast_estimate = contrast @ beta
    contrast_se = np.sqrt(contrast @ covariance @ contrast)
    statistic = contrast_estimate / contrast_se
    r2 = 1 - residual @ residual / ((y - y.mean()) ** 2).sum()
    estimates = [(term, beta[index], se[index]) for index, term in enumerate(terms)]
    if row.column <= 3:
        estimates.append(("hh_minus_lh", contrast_estimate, contrast_se))
    for term, estimate, standard_error in estimates:
        expected = coefficients[(coefficients.outcome == row.outcome)
                                & (coefficients.column == row.column)
                                & (coefficients.term == term)].iloc[0]
        errors = [abs(estimate - expected.estimate), abs(standard_error - expected.se),
                  abs(r2 - row.r2)]
        assert max(errors) < 1e-7, (row.outcome, row.column, term, errors)
        records.append(dict(outcome=row.outcome, column=row.column, term=term,
                            coefficient_error=errors[0], se_error=errors[1], r2_error=errors[2],
                            t_stat=estimate/standard_error, equality_t_stat=statistic,
                            df=row.clusters-1, expected_p=expected.p,
                            expected_equality_p=row.equality_p))

pd.DataFrame(records).to_csv(out / "independent_validation.csv", index=False)
subprocess.run(["Rscript", "-e", r"""
path <- commandArgs(TRUE)[1]
x <- read.csv(path)
x$p_error <- abs(2*pt(-abs(x$t_stat), x$df)-x$expected_p)
x$equality_p_error <- abs(2*pt(-abs(x$equality_t_stat), x$df)-x$expected_equality_p)
stopifnot(max(x$p_error, x$equality_p_error)<1e-7)
write.csv(x, path, row.names=FALSE)
""", str(out / "independent_validation.csv")], check=True)
print("Validated all 12 regressions: coefficients, class-clustered SEs, coefficient p-values, equality tests, and R-squared.")
print(pd.read_csv(out / "independent_validation.csv").filter(like="error").max().to_string())
