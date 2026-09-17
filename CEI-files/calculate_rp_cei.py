"""Calculate CEI from the RP experiment data."""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

from cei import run_panel


HERE = Path(__file__).resolve().parent
DEFAULT_DATA_DIR = HERE.parent / "Replication_Package" / "data"
DEFAULT_OUTPUT_DIR = HERE / "results"


def build_cei_input(data_dir: Path) -> pd.DataFrame:
    panel = pd.read_stata(
        data_dir / "panel_individual.dta", convert_categoricals=False
    )
    roster = panel[["group_id", "post", "id", "person"]]

    base = pd.read_stata(
        data_dir / "base_raw.dta", convert_categoricals=False
    ).assign(post=0)
    end = pd.read_stata(
        data_dir / "end_raw.dta", convert_categoricals=False
    ).assign(post=1)
    raw = pd.concat([base, end], ignore_index=True)
    raw = raw.dropna(
        subset=["coord_x", "coord_y", "intercept_x", "intercept_y"]
    )
    raw = raw[(raw["intercept_x"] > 0) & (raw["intercept_y"] > 0)]

    individual = raw[raw["round_number"].between(1, 18)].merge(
        roster,
        on=["group_id", "post", "id"],
        how="inner",
        validate="many_to_one",
    )
    individual["role"] = individual["person"].map({1: "i", 2: "j"})

    pairwaves = roster[["group_id", "post"]].drop_duplicates()
    group = raw[
        raw["round_number"].between(19, 36) & raw["mover"].eq(1)
    ].merge(
        pairwaves,
        on=["group_id", "post"],
        how="inner",
        validate="many_to_one",
    )
    group["round_number"] -= 18
    group["role"] = "g"

    columns = [
        "group_id",
        "post",
        "role",
        "round_number",
        "intercept_x",
        "intercept_y",
        "coord_x",
        "coord_y",
    ]
    data = pd.concat([individual[columns], group[columns]], ignore_index=True)
    data = data.rename(
        columns={
            "group_id": "pair_id",
            "post": "wave",
            "round_number": "round",
            "coord_x": "x_r",
            "coord_y": "x_b",
        }
    )
    data["p_r"] = 1 / data.pop("intercept_x")
    data["p_b"] = 1 / data.pop("intercept_y")
    data = data[
        ["pair_id", "wave", "role", "round", "p_r", "p_b", "x_r", "x_b"]
    ].sort_values(["pair_id", "wave", "role", "round"])

    key = ["pair_id", "wave", "role", "round"]
    if data.duplicated(key).any():
        raise ValueError("Duplicate role-round choices found.")
    counts = data.groupby(["pair_id", "wave", "role"]).size()
    if len(counts) != 3 * len(pairwaves) or not counts.eq(18).all():
        raise ValueError("Expected 18 choices for every role in every pair-wave.")
    return data.reset_index(drop=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data-dir", type=Path, default=DEFAULT_DATA_DIR)
    parser.add_argument("--output-dir", type=Path, default=DEFAULT_OUTPUT_DIR)
    args = parser.parse_args()

    data = build_cei_input(args.data_dir)
    pairwave, rounds = run_panel(data)

    args.output_dir.mkdir(parents=True, exist_ok=True)
    pairwave.to_csv(args.output_dir / "cei_pairwave.csv", index=False)
    rounds.to_csv(args.output_dir / "cei_rounds.csv", index=False)

    summary = pairwave.groupby(["spec", "wave"]).agg(
        n=("pair_id", "size"),
        mean_cei=("e_coll", "mean"),
        pass_cei=("pass_coll", "mean"),
        mean_ccei=("ccei_g", "mean"),
        pass_ccei=("pass_unitary", "mean"),
    )
    print(summary.round(3))


if __name__ == "__main__":
    main()
