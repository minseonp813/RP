from __future__ import annotations

import argparse
import sys
from pathlib import Path

import numpy as np
import pandas as pd


def normalize(prices: np.ndarray, choices: np.ndarray) -> np.ndarray:
    expenditure = np.sum(prices * choices, axis=1)
    if np.any(expenditure <= 0):
        raise ValueError("Non-positive expenditure")
    return prices / expenditure[:, None]


def arrays(rows: pd.DataFrame):
    rows = rows.sort_values("round_number")
    prices = np.column_stack(
        [
            1.0 / rows["intercept_x"].to_numpy(float),
            1.0 / rows["intercept_y"].to_numpy(float),
        ]
    )
    choices = rows[["coord_x", "coord_y"]].to_numpy(float)
    return normalize(prices, choices), choices


def load_pair_wave(raw: pd.DataFrame, panel_row, suffix: str):
    group_id = panel_row.group_id
    id_i = getattr(panel_row, f"id_mover_{suffix}")
    id_j = getattr(panel_row, f"id_nonmover_{suffix}")
    pair = raw[raw["group_id"] == group_id]
    individual = pair[pair["round_number"].between(1, 18)]
    group = pair[
        pair["round_number"].between(19, 36)
        & (pair["id"] == id_i)
    ]
    if len(individual[individual["id"] == id_i]) != 18:
        raise AssertionError((group_id, suffix, "member i count"))
    if len(individual[individual["id"] == id_j]) != 18:
        raise AssertionError((group_id, suffix, "member j count"))
    if len(group) != 18:
        raise AssertionError((group_id, suffix, "group count"))
    return (
        *arrays(individual[individual["id"] == id_i]),
        *arrays(individual[individual["id"] == id_j]),
        *arrays(group),
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    root = Path(args.root).resolve()
    supplied_dir = root / "programs" / "CEI-files"
    sys.path.insert(0, str(supplied_dir))
    import cei

    data_dir = root / "data"
    panel = pd.read_stata(data_dir / "panel_final.dta", convert_categoricals=False)
    raw_by_wave = {
        "base": pd.read_stata(data_dir / "base_raw.dta", convert_categoricals=False),
        "end": pd.read_stata(data_dir / "end_raw.dta", convert_categoricals=False),
    }

    rows = []
    for suffix, raw in raw_by_wave.items():
        for row in panel.itertuples(index=False):
            p_i, x_i, p_j, x_j, p_g, x_g = load_pair_wave(raw, row, suffix)
            result_t = cei.cei_pair_wave(
                p_i, x_i, p_j, x_j, p_g, x_g, tempered=True
            )
            result_u = cei.cei_pair_wave(
                p_i, x_i, p_j, x_j, p_g, x_g, tempered=False
            )
            if not np.all(result_t.e_t >= result_u.e_t - 1e-12):
                raise AssertionError((row.group_id, suffix, "tempering monotonicity"))
            if not (0 < result_t.e_coll <= 1 and 0 < result_u.e_coll <= 1):
                raise AssertionError((row.group_id, suffix, "CEI range"))
            rows.append(
                {
                    "group_id": row.group_id,
                    "wave": suffix,
                    "cei_g": result_t.e_coll,
                    "cei_g_untempered": result_u.e_coll,
                    "cei_n_viol": result_t.n_viol,
                    "cei_n_viol_untempered": result_u.n_viol,
                }
            )

    output = pd.DataFrame(rows)
    if output.duplicated(["group_id", "wave"]).any():
        raise AssertionError("Duplicate group-wave CEI rows")
    output.to_csv(args.output, index=False)


if __name__ == "__main__":
    main()
