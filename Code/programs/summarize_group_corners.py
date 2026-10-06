"""Corner and equal-allocation shares for the Figure 6 CCEI/CEIV categories."""
from pathlib import Path

import numpy as np
import pandas as pd


root = Path(__file__).resolve().parents[2]
out = root / "Code/results/new_indices/group_corners"
out.mkdir(exist_ok=True)
keys = ["group_id", "post"]
sample = pd.read_stata(
    root / "Code/results/new_indices/analysis_sample.dta",
    columns=keys + ["ccei_g", "ceiv_g"],
    convert_categoricals=False,
)
assert len(sample) == 1304 and not sample.duplicated(keys).any()
sample["category"] = (
    1 + (sample.ccei_g >= 1 - 1e-9).astype(int)
    + 2 * (sample.ceiv_g >= 1 - 1e-9).astype(int)
)
labels = {
    1: "CCEI<1, CEIV<1",
    2: "CCEI=1, CEIV<1",
    3: "CCEI<1, CEIV=1",
    4: "CCEI=1, CEIV=1",
}
choices = []
coordinates = ["coord_x", "coord_y", "intercept_x", "intercept_y"]
for post, name in enumerate(["base_raw", "end_raw"]):
    raw = pd.read_stata(root / f"Code/data/{name}.dta", convert_categoricals=False)
    raw["post"] = post
    raw = raw[raw.game_type == 2].merge(sample[keys], on=keys, validate="many_to_one")
    mover = raw[raw.mover == 1]
    partner = raw[raw.mover == 0]
    duplicate = mover.merge(
        partner, on=keys + ["round_number"], suffixes=("_m", "_p"),
        validate="one_to_one",
    )
    assert len(duplicate) == len(mover) == len(partner)
    for col in coordinates:
        assert (duplicate[f"{col}_m"] == duplicate[f"{col}_p"]).all()
    choices.append(mover[keys + ["round_number"] + coordinates])

choices = pd.concat(choices, ignore_index=True)
assert not choices.duplicated(keys + ["round_number"]).any()
assert choices.groupby(keys).size().eq(18).all()
assert choices[coordinates].notna().all().all()
assert choices[["intercept_x", "intercept_y"]].gt(0).all().all()
assert choices[["coord_x", "coord_y"]].ge(0).all().all()
xshare = choices.coord_x / choices.intercept_x
yshare = choices.coord_y / choices.intercept_y
# Integer coordinate rounding puts recorded choices slightly off the budget line.
assert np.abs(xshare + yshare - 1).max() < .002
distance = np.minimum(xshare, yshare)
# Exact uses the existing paper convention; near-corner cutoffs are inclusive.
choices["exact"] = (choices.coord_x == 0) | (choices.coord_y == 0)
choices["within_1pct"] = distance <= .01 + 1e-12
choices["within_5pct"] = distance <= .05 + 1e-12
thresholds = ["exact", "within_1pct", "within_5pct"]
# On the budget line, |x-y|/(intercept_x+intercept_y) is the fractional
# distance along the line to its equal-allocation point (the paper's midpoint).
midpoint_distance = abs(choices.coord_x - choices.coord_y) / (
    choices.intercept_x + choices.intercept_y
)
choices["midpoint_exact"] = choices.coord_x == choices.coord_y
choices["midpoint_within_1pct"] = midpoint_distance <= .01 + 1e-12
choices["midpoint_within_5pct"] = midpoint_distance <= .05 + 1e-12
measure_sets = {
    "corner": thresholds,
    "midpoint": [f"midpoint_{threshold}" for threshold in thresholds],
    "corner_or_midpoint": [f"combined_{threshold}" for threshold in thresholds],
}
for threshold in thresholds:
    choices[f"combined_{threshold}"] = (
        choices[threshold] | choices[f"midpoint_{threshold}"]
    )
for names in measure_sets.values():
    assert (choices[names[0]] <= choices[names[1]]).all()
    assert (choices[names[1]] <= choices[names[2]]).all()
measures = [name for names in measure_sets.values() for name in names]
shares = choices.groupby(keys)[measures].mean().reset_index()
shares = sample.merge(shares, on=keys, validate="one_to_one")
assert len(shares) == 1304 and shares[measures].notna().all().all()
shares.to_csv(out / "group_wave_shares.csv", index=False)

rows = []
for category, group in shares.groupby("category", sort=True):
    for choice_type, names in measure_sets.items():
        for threshold, measure in zip(thresholds, names):
            rows.append({
                "category": category,
                "label": labels[category],
                "choice_type": choice_type,
                "threshold": threshold,
                "group_waves": len(group),
                "choices": 18 * len(group),
                "mean_share_pct": 100 * group[measure].mean(),
                "all_18_count": int(group[measure].eq(1).sum()),
                "all_18_pct": 100 * group[measure].eq(1).mean(),
            })
summary = pd.DataFrame(rows)
summary.to_csv(out / "category_summary.csv", index=False)
print(summary.to_string(index=False, float_format=lambda x: f"{x:.2f}"))
target = shares[shares.category == 2]
print("\nTarget category: share of group-waves with no exact corners:",
      f"{100 * target.exact.eq(0).mean():.2f}%")
print("Target category exact-corner count distribution:")
print((target.exact * 18).round().astype(int).value_counts().sort_index().to_string())
