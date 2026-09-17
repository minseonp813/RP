"""Build the reusable target-pair by donor-group Ihat matrix."""

from __future__ import annotations

import json
import math
import os
import shutil
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd

try:
    from joblib import Parallel, delayed
except ImportError:  # Standard-library fallback for lean Jupyter environments.
    Parallel = None
    delayed = None

def _revealed_relations(p: np.ndarray, x: np.ndarray, e: float):
    costs = p.T @ x
    expenditure = np.diag(costs)
    return (e * expenditure)[:, None] >= costs, (e * expenditure)[:, None] > costs

def _transitive_closure(relation: np.ndarray) -> np.ndarray:
    closure = relation.copy()
    for k in range(closure.shape[0]):
        closure |= closure[:, [k]] & closure[[k], :]
    return closure

def _cross_garp(p: np.ndarray, x: np.ndarray, side: np.ndarray, e: float) -> int:
    direct, strict = _revealed_relations(p, x, e)
    closure = _transitive_closure(direct)
    strict_on_cycle = strict & closure.T
    for row in np.flatnonzero(strict_on_cycle.any(axis=1)):
        component = (closure[row, :] & closure[:, row]).copy()
        component[row] = True
        component |= strict_on_cycle[row, :]
        if np.any(side[component] == 0) and np.any(side[component] == 1):
            return 0
    return 1

def _ex_cross(p: np.ndarray, x: np.ndarray, side: np.ndarray, tol: float = 1e-6):
    if _cross_garp(p, x, side, 1.0):
        return 1.0
    low, high, estar = 0.0, 1.0, 0.0
    while high - low > tol:
        midpoint = (low + high) / 2
        if _cross_garp(p, x, side, midpoint):
            estar = low = midpoint
        else:
            high = midpoint
    costs = p.T @ x
    expenditure = np.diag(costs)
    ratios = costs / expenditure[:, None]
    mask = (~np.eye(len(side), dtype=bool)) & (ratios < 1 - 1e-12)
    largest_strict_ratio = float(np.max(ratios[mask], initial=0.0))
    return 1.0 if estar > largest_strict_ratio + 1e-9 else estar

def ex_from_arrays(
    ix_ind, iy_ind, cx_ind, cy_ind, ix_group, iy_group, cx_group, cy_group,
):
    p = np.vstack((
        1 / np.r_[ix_ind, ix_group],
        1 / np.r_[iy_ind, iy_group],
    ))
    x = np.vstack((np.r_[cx_ind, cx_group], np.r_[cy_ind, cy_group]))
    side = np.r_[
        np.zeros(len(ix_ind), dtype=np.int8),
        np.ones(len(ix_group), dtype=np.int8),
    ]
    return _ex_cross(p, x, side)

def ihat_from_ex(ex_i, ex_j, ex_ij, tol: float = 1e-6):
    cost_i, cost_j, cost_ij = 1 - ex_i, 1 - ex_j, 1 - ex_ij
    if not np.isfinite(cost_ij) or cost_ij <= tol:
        return np.nan, np.nan
    ihat_i = 0.5 + (cost_i - cost_j) / (2 * cost_ij)
    return ihat_i, 1 - ihat_i

@dataclass
class PlaceboConfig:
    data_dir: Path
    output_dir: Path
    n_jobs: int = 1
    backend: str = "threading"
    target_chunk_size: int = 8
    max_target_pairwaves: int | None = None
    max_donors_per_wave: int | None = None
    overwrite_chunks: bool = False
    cross_tol: float = 1e-6

    def resolved(self) -> "PlaceboConfig":
        cfg = PlaceboConfig(**asdict(self))
        cfg.data_dir = Path(cfg.data_dir).expanduser().resolve()
        cfg.output_dir = Path(cfg.output_dir).expanduser().resolve()
        return cfg

def _numeric_key(value):
    """Stable scalar key for Stata numeric IDs read as int or float."""
    if pd.isna(value):
        raise ValueError("Missing identifier encountered.")
    number = float(value)
    return int(number) if number.is_integer() else number

def _choice_arrays(frame: pd.DataFrame) -> tuple[np.ndarray, ...]:
    frame = frame.sort_values("round_number")
    return tuple(
        frame[col].to_numpy(dtype=float)
        for col in ("intercept_x", "intercept_y", "coord_x", "coord_y")
    )

def _load_inputs(cfg: PlaceboConfig):
    required = ["panel_individual.dta", "base_raw.dta", "end_raw.dta"]
    missing = [name for name in required if not (cfg.data_dir / name).is_file()]
    if missing:
        raise FileNotFoundError(f"Missing input files in {cfg.data_dir}: {missing}")

    panel = pd.read_stata(
        cfg.data_dir / "panel_individual.dta", convert_categoricals=False
    )
    needed_panel = {
        "group_id", "class", "post", "id", "partner_id", "Ihat_ig",
        "ccei_i", "ccei_j",
    }
    absent = needed_panel - set(panel.columns)
    if absent:
        raise ValueError(f"panel_individual.dta lacks columns: {sorted(absent)}")

    base = pd.read_stata(cfg.data_dir / "base_raw.dta", convert_categoricals=False)
    end = pd.read_stata(cfg.data_dir / "end_raw.dta", convert_categoricals=False)
    base["post"] = 0
    end["post"] = 1
    raw = pd.concat([base, end], ignore_index=True)
    raw = raw.loc[
        raw["round_number"].between(1, 36)
        & raw[["coord_x", "coord_y", "intercept_x", "intercept_y"]].notna().all(axis=1)
        & raw["intercept_x"].ne(0)
        & raw["intercept_y"].ne(0)
    ].copy()
    raw["group_key"] = raw["group_id"].map(_numeric_key)
    raw["id_key"] = raw["id"].map(_numeric_key)
    raw["post"] = raw["post"].astype(int)

    roster_rows = []
    for (group_id, post), block in panel.groupby(["group_id", "post"], sort=True):
        members = block.sort_values("id")
        if len(members) != 2 or members["id"].nunique() != 2:
            raise ValueError(f"Expected two members for group={group_id}, post={post}.")
        roster_rows.append({
            "target_group_id": _numeric_key(group_id),
            "post": int(post),
            "target_class": _numeric_key(members["class"].iloc[0]),
            "member1_id": _numeric_key(members["id"].iloc[0]),
            "member2_id": _numeric_key(members["id"].iloc[1]),
        })
    roster = pd.DataFrame(roster_rows).sort_values(
        ["post", "target_group_id"]
    ).reset_index(drop=True)
    roster["target_index"] = np.arange(len(roster), dtype=int)

    individual = {}
    ind_raw = raw.loc[raw["round_number"].between(1, 18)]
    ind_blocks = {
        key: block
        for key, block in ind_raw.groupby(["group_key", "post", "id_key"], sort=False)
    }
    for row in roster.itertuples(index=False):
        for member_id in (row.member1_id, row.member2_id):
            block = ind_blocks.get((row.target_group_id, row.post, member_id))
            if block is None:
                block = ind_raw.iloc[0:0]
            if len(block) != 18:
                raise ValueError(
                    f"Expected 18 individual choices: group={row.target_group_id}, "
                    f"post={row.post}, id={member_id}; found {len(block)}."
                )
            individual[(row.target_group_id, row.post, member_id)] = _choice_arrays(block)

    groups = {}
    group_raw = raw.loc[raw["round_number"].between(19, 36) & raw["mover"].eq(1)]
    group_blocks = {
        key: block
        for key, block in group_raw.groupby(["group_key", "post"], sort=False)
    }
    for row in roster.itertuples(index=False):
        block = group_blocks.get((row.target_group_id, row.post))
        if block is None:
            block = group_raw.iloc[0:0]
        if len(block) != 18:
            raise ValueError(
                f"Expected 18 group choices: group={row.target_group_id}, "
                f"post={row.post}; found {len(block)}."
            )
        groups[(row.target_group_id, row.post)] = _choice_arrays(block)

    panel_keys = panel.copy()
    for col in ("group_id", "id", "partner_id", "class"):
        panel_keys[col] = panel_keys[col].map(_numeric_key)
    panel_keys["post"] = panel_keys["post"].astype(int)
    return panel_keys, roster, individual, groups

def _target_donor_rows(target, donor_rows, individual, groups, tol: float):
    key1 = (target.target_group_id, target.post, target.member1_id)
    key2 = (target.target_group_id, target.post, target.member2_id)
    ix1, iy1, x1, y1 = individual[key1]
    ix2, iy2, x2, y2 = individual[key2]

    output = []
    for donor in donor_rows:
        dix, diy, dx, dy = groups[(donor.target_group_id, donor.post)]
        ex1 = ex_from_arrays(ix1, iy1, x1, y1, dix, diy, dx, dy)
        ex2 = ex_from_arrays(ix2, iy2, x2, y2, dix, diy, dx, dy)
        ex12 = ex_from_arrays(
            np.r_[ix1, ix2], np.r_[iy1, iy2],
            np.r_[x1, x2], np.r_[y1, y2],
            dix, diy, dx, dy,
        )
        ih1, ih2 = ihat_from_ex(ex1, ex2, ex12, tol=tol)
        output.append({
            "target_group_id": target.target_group_id,
            "post": target.post,
            "target_class": target.target_class,
            "member1_id": target.member1_id,
            "member2_id": target.member2_id,
            "donor_group_id": donor.target_group_id,
            "donor_class": donor.target_class,
            "is_own": int(donor.target_group_id == target.target_group_id),
            "same_class": int(donor.target_class == target.target_class),
            "cost1": 1.0 - ex1,
            "cost2": 1.0 - ex2,
            "cost12": 1.0 - ex12,
            "Ihat1_donor": ih1,
            "Ihat2_donor": ih2,
            "degenerate": int(not np.isfinite(ih1)),
        })
    return output

def _chunks(items: list, size: int) -> Iterable[list]:
    for start in range(0, len(items), size):
        yield items[start:start + size]

def _run_jobs(jobs, individual, groups, cfg):
    if cfg.n_jobs == 1:
        return [
            _target_donor_rows(target, donors, individual, groups, cfg.cross_tol)
            for target, donors in jobs
        ]
    if Parallel is not None:
        return Parallel(n_jobs=cfg.n_jobs, backend=cfg.backend, verbose=5)(
            delayed(_target_donor_rows)(
                target, donors, individual, groups, cfg.cross_tol
            )
            for target, donors in jobs
        )
    workers = os.cpu_count() if cfg.n_jobs == -1 else max(1, cfg.n_jobs)
    print(f"joblib is unavailable; using {workers} standard-library threads.")
    with ThreadPoolExecutor(max_workers=workers) as executor:
        futures = [
            executor.submit(
                _target_donor_rows,
                target, donors, individual, groups, cfg.cross_tol,
            )
            for target, donors in jobs
        ]
        return [future.result() for future in futures]

def build_donor_matrix(config: PlaceboConfig) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Compute or resume the expensive target-pair by donor-pair matrix."""
    cfg = config.resolved()
    cfg.output_dir.mkdir(parents=True, exist_ok=True)
    chunk_dir = cfg.output_dir / "donor_matrix_chunks"
    if cfg.overwrite_chunks and chunk_dir.exists():
        shutil.rmtree(chunk_dir)
    chunk_dir.mkdir(parents=True, exist_ok=True)

    panel, roster, individual, groups = _load_inputs(cfg)
    targets = list(roster.itertuples(index=False))
    if cfg.max_target_pairwaves is not None:
        targets = targets[:cfg.max_target_pairwaves]

    metadata = asdict(cfg)
    metadata["data_dir"] = str(cfg.data_dir)
    metadata["output_dir"] = str(cfg.output_dir)
    metadata["n_pairwaves_available"] = len(roster)
    metadata["n_pairwaves_requested"] = len(targets)
    metadata["started_at"] = time.strftime("%Y-%m-%d %H:%M:%S")
    (cfg.output_dir / "placebo_normalized_config.json").write_text(
        json.dumps(metadata, indent=2), encoding="utf-8"
    )

    chunk_paths = []
    for chunk_no, target_chunk in enumerate(_chunks(targets, cfg.target_chunk_size)):
        chunk_path = chunk_dir / f"donor_matrix_chunk_{chunk_no:04d}.csv"
        chunk_paths.append(chunk_path)
        if chunk_path.exists() and not cfg.overwrite_chunks:
            print(f"[resume] {chunk_path.name}")
            continue

        jobs = []
        for target in target_chunk:
            donors = [r for r in roster.itertuples(index=False) if r.post == target.post]
            if cfg.max_donors_per_wave is not None:
                own = [r for r in donors if r.target_group_id == target.target_group_id]
                other = [r for r in donors if r.target_group_id != target.target_group_id]
                donors = own + other[:cfg.max_donors_per_wave]
            jobs.append((target, donors))

        results = _run_jobs(jobs, individual, groups, cfg)
        chunk_frame = pd.DataFrame([row for block in results for row in block])
        chunk_frame.to_csv(chunk_path, index=False)
        print(f"[saved] {chunk_path.name}: {len(chunk_frame):,} rows")

    donor_matrix = pd.concat(
        (pd.read_csv(path) for path in chunk_paths), ignore_index=True
    )
    matrix_path = cfg.output_dir / "placebo_donor_matrix.csv"
    donor_matrix.to_csv(matrix_path, index=False)

    _validate_diagonal(donor_matrix, panel, cfg.cross_tol)
    print(f"Donor matrix saved: {matrix_path} ({len(donor_matrix):,} rows)")
    return donor_matrix, panel

def _validate_diagonal(matrix: pd.DataFrame, panel: pd.DataFrame, tol: float) -> None:
    diagonal = matrix.loc[matrix["is_own"].eq(1)].copy()
    if diagonal.empty:
        raise AssertionError("The donor matrix contains no diagonal entries.")
    long1 = diagonal[["target_group_id", "post", "member1_id", "Ihat1_donor"]].rename(
        columns={"member1_id": "id", "Ihat1_donor": "Ihat_check"}
    )
    long2 = diagonal[["target_group_id", "post", "member2_id", "Ihat2_donor"]].rename(
        columns={"member2_id": "id", "Ihat2_donor": "Ihat_check"}
    )
    check = pd.concat([long1, long2], ignore_index=True).merge(
        panel[["group_id", "post", "id", "Ihat_ig"]],
        left_on=["target_group_id", "post", "id"],
        right_on=["group_id", "post", "id"], how="left", validate="one_to_one",
    )
    both = check[["Ihat_check", "Ihat_ig"]].notna().all(axis=1)
    max_error = (check.loc[both, "Ihat_check"] - check.loc[both, "Ihat_ig"]).abs().max()
    missing_match = check["Ihat_check"].isna().eq(check["Ihat_ig"].isna()).all()
    diagonal_tol = 1e-4
    if not missing_match or (pd.notna(max_error) and max_error > diagonal_tol):
        raise AssertionError(
            f"Diagonal does not reproduce stored Ihat_ig: max error={max_error}, "
            f"missing-pattern match={missing_match}."
        )
    print(f"Diagonal validation passed; max |computed-stored| = {max_error:.3g}")

def _pool_summary(block: pd.DataFrame, prefix: str) -> dict:
    if block.empty:
        return {
            f"M_{prefix}_imp": np.nan, f"M_{prefix}_drop": np.nan,
            f"n_{prefix}": 0, f"nvalid_{prefix}": 0,
            f"degfrac_{prefix}": np.nan,
        }
    valid = block["Ihat_member"].notna()
    return {
        f"M_{prefix}_imp": block["Ihat_member"].fillna(0.5).mean(),
        f"M_{prefix}_drop": block.loc[valid, "Ihat_member"].mean(),
        f"n_{prefix}": len(block),
        f"nvalid_{prefix}": int(valid.sum()),
        f"degfrac_{prefix}": float((~valid).mean()),
    }

def aggregate_placebo(config: PlaceboConfig, donor_matrix=None, panel=None) -> pd.DataFrame:
    """Create one observation per member-pair-wave and save CSV and Stata files."""
    cfg = config.resolved()
    if donor_matrix is None or panel is None:
        panel, _, _, _ = _load_inputs(cfg)
        donor_matrix = pd.read_csv(cfg.output_dir / "placebo_donor_matrix.csv")

    rows = []
    for (group_id, post), block in donor_matrix.groupby(
        ["target_group_id", "post"], sort=True
    ):
        own = block.loc[block["is_own"].eq(1)]
        if len(own) != 1:
            raise AssertionError(f"Expected one diagonal row for {group_id}, wave {post}.")
        own = own.iloc[0]
        donors = block.loc[block["is_own"].eq(0)].copy()

        for member_no in (1, 2):
            other_no = 3 - member_no
            member_id = own[f"member{member_no}_id"]
            partner_id = own[f"member{other_no}_id"]
            member_donors = donors.copy()
            member_donors["Ihat_member"] = member_donors[f"Ihat{member_no}_donor"]
            member_donors["cost_member"] = member_donors[f"cost{member_no}"]
            all_pool = member_donors
            cls_pool = member_donors.loc[member_donors["same_class"].eq(1)]

            record = {
                "group_id": group_id, "post": int(post),
                "class": own["target_class"], "id": member_id,
                "partner_id": partner_id,
                "cost_own": own[f"cost{member_no}"],
                "costN_own": own["cost12"],
                "I_actual": own[f"Ihat{member_no}_donor"],
            }
            record.update(_pool_summary(all_pool, "all"))
            record.update(_pool_summary(cls_pool, "cls"))

            own_cost = record["cost_own"]
            record["P_all"] = (
                float((own_cost <= all_pool["cost_member"]).mean())
                if len(all_pool) else np.nan
            )
            record["P_cls"] = (
                float((own_cost <= cls_pool["cost_member"]).mean())
                if len(cls_pool) else np.nan
            )
            for pool in ("all", "cls"):
                for rule in ("imp", "drop"):
                    record[f"Istar_{pool}_{rule}"] = (
                        record["I_actual"] - record[f"M_{pool}_{rule}"]
                    )
            rows.append(record)

    analysis = pd.DataFrame(rows)
    for pool in ("all", "cls"):
        partner = analysis[["group_id", "post", "id", f"P_{pool}"]].rename(
            columns={"id": "partner_id", f"P_{pool}": f"P_partner_{pool}"}
        )
        analysis = analysis.merge(
            partner, on=["group_id", "post", "partner_id"], how="left",
            validate="one_to_one",
        )
        analysis[f"Pdiff_{pool}"] = analysis[f"P_{pool}"] - analysis[f"P_partner_{pool}"]

    for pool in ("all", "cls"):
        for rule in ("imp", "drop"):
            col = f"Istar_{pool}_{rule}"
            sums = analysis.groupby(["group_id", "post"])[col].sum(min_count=2)
            finite = sums.dropna()
            error = finite.abs().max() if len(finite) else np.nan
            if pd.notna(error) and error > 1e-10:
                raise AssertionError(f"{col} fails pair adding-up: max error={error}")
            print(f"{col}: max pair sum error = {error:.3g}")

    csv_path = cfg.output_dir / "placebo_normalized_member_wave.csv"
    dta_path = cfg.output_dir / "placebo_normalized_member_wave.dta"
    analysis.to_csv(csv_path, index=False)
    stata = analysis.copy()
    stata.columns = [str(c)[:32] for c in stata.columns]
    stata = stata.rename(columns={"class": "_class"})
    stata.to_stata(dta_path, write_index=False, version=118)
    print(f"Analysis data saved: {csv_path}")
    print(f"Stata data saved:    {dta_path}")
    return analysis

def run_placebo_normalization(config: PlaceboConfig) -> pd.DataFrame:
    matrix, panel = build_donor_matrix(config)
    return aggregate_placebo(config, matrix, panel)

if __name__ == "__main__":
    package_dir = Path(__file__).resolve().parent
    config = PlaceboConfig(
        data_dir=package_dir / "data",
        output_dir=package_dir / "results" / "placebo_normalized",
        n_jobs=int(os.getenv("PLACEBO_MATRIX_WORKERS", "1")),
        overwrite_chunks=os.getenv("PLACEBO_OVERWRITE_CHUNKS", "0") == "1",
    )
    run_placebo_normalization(config)
