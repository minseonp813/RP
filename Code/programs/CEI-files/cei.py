"""
cei.py -- Collective Efficiency Index (CEI) for pair choices.

Reference implementation of the test in Section 4 ("The proposed test:
collective rationality with individually stable preferences") of the internal
note "Testing Collective (rather than Unitary) Rationality of Group Choices".

Setting
-------
Pair g = (i, j).  In each wave we observe three datasets of K = 18
observations (p^k, x^k), with p in R^2_{++}, x in R^2_{+}, and income
normalized to p^k . x^k = 1:

    D_i, D_j : members' individual choices
    D_g      : the pair's collective choices (common allocation, so all
               consumption is public and fully observed).

Definitions (Section 4 of the note)
-----------------------------------
* Direct relation at efficiency e:  x^k R^0_e x^l  iff  p^k . x^l <= e.
  (Strict version P^0_e uses "<".)  These are exactly the relations used in
  the paper's Section 3 (Definition 1).
* CCEI e*_m of dataset D_m: sup{e : D_m has no e-violation}.
* Objector set A_m(t) for member m and group observation t: the set of
  member-m individual observations s such that x^s_m is revealed preferred to
  x^t through D_m, i.e. there is a chain
        x^s_m R^0_e x^{k_1}_m R^0_e ... R^0_e x^{k_n}_m R^0_e x^t
  in which every step uses member m's own direct relation R^0_e (the last
  step is the cross comparison p^{k_n}_m . x^t <= e).  "Tempered" chains use
  e = e*_m (the member's own CCEI); "untempered" chains use e = 1.
* Observation-level index
        e_t = min( 1, min_{s in A_i(t), s' in A_j(t)} p^t . (x^s_i v x^{s'}_j) )
  with v the componentwise maximum (join), and e_t = 1 if either objector
  set is empty.  A violation of collective rationality (under the maintained
  hypothesis of stable individual preferences) is e_t < 1.
* Pair-wave index  e^coll_g = min_t e_t ; we also report mean_t e_t and the
  number of violating rounds.

Also implemented
----------------
* Test 0 (FOSD floor): a group choice that buys strictly more of the more
  expensive security is Pareto dominated for any FOSD-respecting members; the
  index e^FOSD_t is the cost of the swapped bundle (x_b, x_r), capped at 1.
* Unitary benchmark: group CCEI, exact (no bisection), for the CEI-vs-CCEI
  decomposition in Exhibit A.
* Power simulations against random behaviour: Bronars (uniform budget shares)
  and bootstrap (budget shares resampled from an observed pool), keeping the
  actual individual data and replacing group choices only.
* Protocol simulator (dictatorship by either member, fixed compromise,
  random dictator, alternating turns) for CRRA expected-utility members, used
  by validate_cei.py to check that all protocols pass Test 1 while the
  varying-weight protocols fail unitary GARP.

Data interface
--------------
A long pandas DataFrame with columns

    pair_id, wave, role, round, p_r, p_b, x_r, x_b

where role in {"i", "j", "g"}.  Prices may be given unnormalized (e.g. as
KRW prices or as axis intercepts, see `from_intercepts`); `normalize_income`
rescales each observation so that p . x = 1.

Numerical tolerance: all affordability comparisons use an absolute tolerance
TOL (default 1e-9) so that an alternative that costs exactly the group's
expenditure is *not* counted as affordable with slack.

Author: draft for the coauthor team, September 2026.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass, field
from typing import Dict, Iterable, List, Optional, Sequence, Tuple

import numpy as np
import pandas as pd

TOL = 1e-9
ROLES = ("i", "j", "g")

# ---------------------------------------------------------------------------
# 1. Basic revealed-preference machinery
# ---------------------------------------------------------------------------


def normalize_income(P: np.ndarray, X: np.ndarray) -> np.ndarray:
    """Rescale prices so that p^k . x^k = 1 for every observation k."""
    P = np.asarray(P, dtype=float)
    X = np.asarray(X, dtype=float)
    inc = np.einsum("kd,kd->k", P, X)
    if np.any(inc <= 0):
        raise ValueError("Non-positive expenditure; check prices/choices.")
    return P / inc[:, None]


def from_intercepts(int_r: np.ndarray, int_b: np.ndarray) -> np.ndarray:
    """Prices from axis intercepts (m/p_r, m/p_b) with income m normalized
    to 1: p_s = 1 / intercept_s."""
    return np.column_stack([1.0 / np.asarray(int_r, float),
                            1.0 / np.asarray(int_b, float)])


def cost_matrix(P: np.ndarray, X: np.ndarray) -> np.ndarray:
    """C[k, l] = p^k . x^l  (cost of bundle l at prices of observation k)."""
    return np.asarray(P, float) @ np.asarray(X, float).T


def direct_relation(C: np.ndarray, e: float = 1.0, strict: bool = False) -> np.ndarray:
    """Boolean matrix of R^0_e (or P^0_e if strict): R[k, l] = 1 iff
    p^k . x^l <= e  (resp. < e)."""
    if strict:
        return C < e - TOL
    return C <= e + TOL


def transitive_closure(R: np.ndarray) -> np.ndarray:
    """Warshall's algorithm.  Returns the reflexive-transitive closure of the
    boolean relation R (K x K)."""
    R = R.astype(bool).copy()
    K = R.shape[0]
    R |= np.eye(K, dtype=bool)
    for k in range(K):
        R |= R[:, [k]] & R[[k], :]
    return R


def has_e_violation(C: np.ndarray, e: float) -> bool:
    """True iff the dataset with cost matrix C admits an e-violation:
    a chain x^{k1} R_e ... R_e x^{kn} with x^{kn} P_e x^{k1}.  Equivalently,
    some (k, l) with k ->* l under R_e and p^l . x^k < e."""
    R = transitive_closure(direct_relation(C, e))
    P = direct_relation(C, e, strict=True)
    return bool(np.any(R & P.T))


def ccei(P: np.ndarray, X: np.ndarray) -> float:
    """Exact Afriat CCEI  e* = sup{e in (0,1] : no e-violation}.

    Violations can only be created (never destroyed) as e rises, so e* is a
    break point of the step function e -> "has violation".  The break points
    are the entries of the cost matrix that are <= 1.  We scan the midpoints
    between consecutive candidate values; the first midpoint at which a
    violation exists identifies e* as the candidate just below it.  If no
    midpoint yields a violation, e* = 1 (GARP holds on (c_max, 1] and the
    sup is 1 whether or not GARP holds at e = 1 exactly)."""
    C = cost_matrix(P, X)
    cand = np.unique(np.concatenate([C[C <= 1 + TOL].ravel(), [1.0]]))
    cand = cand[cand > 0]
    mids = 0.5 * (cand[:-1] + cand[1:])
    if mids.size == 0 or not has_e_violation(C, mids[-1]):
        return 1.0
    # monotonicity: violation at mids[k] implies violation at mids[k'] > k.
    lo, hi = 0, mids.size - 1          # hi has a violation
    while lo < hi:
        mid = (lo + hi) // 2
        if has_e_violation(C, mids[mid]):
            hi = mid
        else:
            lo = mid + 1
    return float(cand[hi])


def garp(P: np.ndarray, X: np.ndarray) -> bool:
    """Standard GARP (e = 1)."""
    return not has_e_violation(cost_matrix(P, X), 1.0)


# ---------------------------------------------------------------------------
# 2. Objector sets and the collective efficiency index
# ---------------------------------------------------------------------------


def objector_sets(P_m: np.ndarray, X_m: np.ndarray, P_g: np.ndarray,
                  X_g: np.ndarray, e_m: float = 1.0) -> np.ndarray:
    """A[s, t] = 1 iff member observation s is in the objector set A_m(t):
    there is a chain s ->* k of member-m relations R^0_{e_m} within D_m, and
    the cross step p^k_m . x^t <= e_m.  Returns a (K_m x K_g) boolean array.

    All steps (within-member and the final cross step) use the same
    efficiency e_m; e_m = 1 gives untempered chains, e_m = CCEI_m gives the
    primary (tempered) specification of the note."""
    C_mm = cost_matrix(P_m, X_m)
    R = transitive_closure(direct_relation(C_mm, e_m))          # s ->* k
    cross = direct_relation(cost_matrix(P_m, X_g), e_m)          # k -> t
    return (R.astype(int) @ cross.astype(int)) > 0


def join_cost_min(P_g_t: np.ndarray, X_i: np.ndarray, X_j: np.ndarray,
                  A_i_t: np.ndarray, A_j_t: np.ndarray
                  ) -> Tuple[float, Optional[Tuple[int, int]]]:
    """min_{s in A_i(t), s' in A_j(t)} p^t . (x^s_i v x^{s'}_j) and the
    argmin pair (s, s').  Returns (inf, None) if either set is empty."""
    Si = np.flatnonzero(A_i_t)
    Sj = np.flatnonzero(A_j_t)
    if Si.size == 0 or Sj.size == 0:
        return np.inf, None
    join = np.maximum(X_i[Si][:, None, :], X_j[Sj][None, :, :])   # |Si|x|Sj|x2
    cost = join @ P_g_t                                              # |Si|x|Sj|
    a, b = np.unravel_index(np.argmin(cost), cost.shape)
    return float(cost[a, b]), (int(Si[a]), int(Sj[b]))


def fosd_floor(P_g: np.ndarray, X_g: np.ndarray) -> np.ndarray:
    """Test 0.  For each group round, the cost of the state-swapped bundle
    (x_b, x_r), capped at 1.  With equal state probabilities the swap has the
    same payoff distribution, so a swap cost < 1 means the group choice is
    Pareto dominated for any FOSD-respecting members (it bought strictly
    more of the more expensive security)."""
    swap = X_g[:, ::-1]
    c = np.einsum("kd,kd->k", P_g, swap)
    return np.minimum(1.0, c)


@dataclass
class CEIResult:
    """Container for one pair-wave."""
    e_t: np.ndarray                     # observation-level index (K_g,)
    argmin: List[Optional[Tuple[int, int]]]  # binding (s, s') per round
    n_obj_i: np.ndarray                 # |A_i(t)|
    n_obj_j: np.ndarray                 # |A_j(t)|
    e_i: float                          # efficiency used for member i chains
    e_j: float
    fosd_t: np.ndarray                  # Test 0 index per round
    ccei_g: float                       # unitary benchmark (group CCEI)
    ccei_i: float
    ccei_j: float

    @property
    def e_coll(self) -> float:
        return float(self.e_t.min())

    @property
    def e_mean(self) -> float:
        return float(self.e_t.mean())

    @property
    def n_viol(self) -> int:
        return int(np.sum(self.e_t < 1 - TOL))

    def summary(self) -> Dict[str, float]:
        return dict(
            e_coll=self.e_coll, e_mean=self.e_mean, n_viol=self.n_viol,
            pass_coll=float(self.e_coll >= 1 - TOL),
            ccei_g=self.ccei_g, pass_unitary=float(self.ccei_g >= 1 - TOL),
            ccei_i=self.ccei_i, ccei_j=self.ccei_j, e_i=self.e_i, e_j=self.e_j,
            fosd_min=float(self.fosd_t.min()),
            n_fosd_viol=int(np.sum(self.fosd_t < 1 - TOL)),
            mean_obj_i=float(self.n_obj_i.mean()),
            mean_obj_j=float(self.n_obj_j.mean()),
            mean_obj_product=float((self.n_obj_i * self.n_obj_j).mean()),
            frac_rounds_both_nonempty=float(
                np.mean((self.n_obj_i > 0) & (self.n_obj_j > 0))),
        )


def cei_pair_wave(P_i, X_i, P_j, X_j, P_g, X_g, tempered: bool = True,
                  e_i: Optional[float] = None, e_j: Optional[float] = None,
                  ccei_cache: Optional[Dict[str, float]] = None) -> CEIResult:
    """Compute the collective efficiency index for one pair-wave.

    Parameters
    ----------
    P_m, X_m : (K x 2) arrays, income normalized to one.
    tempered : if True (primary specification) build objector sets with
               e_m = CCEI of member m; if False use e_m = 1 (upper bound on
               violations).  `e_i`, `e_j` override the tempering level.
    ccei_cache : optional dict with keys "i", "j", "g" to skip recomputation.
    """
    cache = ccei_cache or {}
    ccei_i = cache.get("i", ccei(P_i, X_i))
    ccei_j = cache.get("j", ccei(P_j, X_j))
    ccei_g = cache.get("g", ccei(P_g, X_g))
    if e_i is None:
        e_i = ccei_i if tempered else 1.0
    if e_j is None:
        e_j = ccei_j if tempered else 1.0

    A_i = objector_sets(P_i, X_i, P_g, X_g, e_i)   # K_i x K_g
    A_j = objector_sets(P_j, X_j, P_g, X_g, e_j)   # K_j x K_g

    K_g = X_g.shape[0]
    e_t = np.ones(K_g)
    arg: List[Optional[Tuple[int, int]]] = [None] * K_g
    for t in range(K_g):
        c, pair = join_cost_min(P_g[t], X_i, X_j, A_i[:, t], A_j[:, t])
        if pair is not None and c < 1 - TOL:
            e_t[t] = c
            arg[t] = pair
    return CEIResult(e_t=e_t, argmin=arg,
                     n_obj_i=A_i.sum(0), n_obj_j=A_j.sum(0),
                     e_i=e_i, e_j=e_j, fosd_t=fosd_floor(P_g, X_g),
                     ccei_g=ccei_g, ccei_i=ccei_i, ccei_j=ccei_j)


# ---------------------------------------------------------------------------
# 3. Batch driver over a long-format panel
# ---------------------------------------------------------------------------

REQUIRED_COLS = ["pair_id", "wave", "role", "round", "p_r", "p_b", "x_r", "x_b"]


def _arrays(df: pd.DataFrame, role: str) -> Tuple[np.ndarray, np.ndarray]:
    d = df[df["role"] == role].sort_values("round")
    P = d[["p_r", "p_b"]].to_numpy(float)
    X = d[["x_r", "x_b"]].to_numpy(float)
    return normalize_income(P, X), X


def run_panel(df: pd.DataFrame, tempered: bool = True,
              both: bool = True) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """Compute CEI for every (pair_id, wave) in a long DataFrame.

    Returns (pairwave, rounds): a pair-wave level table and a round-level
    table.  If `both` is True the tempered and untempered specifications are
    both computed (column `spec`)."""
    missing = [c for c in REQUIRED_COLS if c not in df.columns]
    if missing:
        raise ValueError(f"missing columns: {missing}")
    specs = [("tempered", True), ("untempered", False)] if both else \
            [("tempered" if tempered else "untempered", tempered)]
    pw_rows, rd_rows = [], []
    for (pid, w), d in df.groupby(["pair_id", "wave"], sort=True):
        if not all((d["role"] == r).any() for r in ROLES):
            continue
        P_i, X_i = _arrays(d, "i")
        P_j, X_j = _arrays(d, "j")
        P_g, X_g = _arrays(d, "g")
        cache = {"i": ccei(P_i, X_i), "j": ccei(P_j, X_j), "g": ccei(P_g, X_g)}
        for name, temp in specs:
            res = cei_pair_wave(P_i, X_i, P_j, X_j, P_g, X_g,
                                tempered=temp, ccei_cache=cache)
            pw_rows.append(dict(pair_id=pid, wave=w, spec=name, **res.summary()))
            for t in range(len(res.e_t)):
                s, sp = res.argmin[t] if res.argmin[t] else (np.nan, np.nan)
                rd_rows.append(dict(pair_id=pid, wave=w, spec=name, round=t + 1,
                                    e_t=res.e_t[t], fosd_t=res.fosd_t[t],
                                    n_obj_i=res.n_obj_i[t], n_obj_j=res.n_obj_j[t],
                                    bind_s_i=s, bind_s_j=sp))
    return pd.DataFrame(pw_rows), pd.DataFrame(rd_rows)


# ---------------------------------------------------------------------------
# 4. Power against random behaviour (Section 6.1 of the note)
# ---------------------------------------------------------------------------


def random_group_choices(P_g: np.ndarray, rng: np.random.Generator,
                         pool_shares: Optional[np.ndarray] = None) -> np.ndarray:
    """Random bundles on the actual group budgets.  Budget share of the r
    security w ~ U[0,1] (Bronars 1987) or resampled from `pool_shares`
    (bootstrap benchmark of Harbaugh et al. 2001 / Andreoni-Miller 2002).
    Returns X with p . x = 1."""
    K = P_g.shape[0]
    if pool_shares is None:
        w = rng.uniform(size=K)
    else:
        w = rng.choice(np.asarray(pool_shares, float), size=K, replace=True)
    return np.column_stack([w / P_g[:, 0], (1 - w) / P_g[:, 1]])


def budget_shares(P: np.ndarray, X: np.ndarray) -> np.ndarray:
    """Expenditure share of the r security, p_r x_r / (p . x)."""
    return P[:, 0] * X[:, 0] / np.einsum("kd,kd->k", P, X)


def power_random(P_i, X_i, P_j, X_j, P_g, n_sim: int = 200,
                 tempered: bool = True, pool_shares=None,
                 seed: int = 0) -> pd.DataFrame:
    """Keep the actual individual data; replace group choices by random
    bundles on the actual group budgets; return the CEI distribution."""
    rng = np.random.default_rng(seed)
    cache = {"i": ccei(P_i, X_i), "j": ccei(P_j, X_j)}
    out = []
    for b in range(n_sim):
        Xr = random_group_choices(P_g, rng, pool_shares)
        res = cei_pair_wave(P_i, X_i, P_j, X_j, P_g, Xr, tempered=tempered,
                            ccei_cache=cache)
        out.append(dict(sim=b, e_coll=res.e_coll, e_mean=res.e_mean,
                        n_viol=res.n_viol, ccei_g=res.ccei_g))
    return pd.DataFrame(out)


# ---------------------------------------------------------------------------
# 5. Protocol simulator with CRRA expected-utility members (Section 6.2)
# ---------------------------------------------------------------------------


def crra_demand(P: np.ndarray, rho: float) -> np.ndarray:
    """Optimal bundle on each budget for u(x) = x^{1-rho}/(1-rho), equal
    state probabilities, income 1.  FOC: x_r/x_b = (p_b/p_r)^{1/rho}."""
    pr, pb = P[:, 0], P[:, 1]
    ratio = (pb / pr) ** (1.0 / rho)          # x_r / x_b
    x_b = 1.0 / (pr * ratio + pb)
    return np.column_stack([ratio * x_b, x_b])


def fit_crra(P: np.ndarray, X: np.ndarray, grid=None) -> float:
    """Simple placeholder calibration: rho minimizing squared error in budget
    shares over a grid.  Replace with the paper's CRRA estimation routine
    when merging with the Comment 1 codebase."""
    grid = np.geomspace(0.05, 20, 400) if grid is None else grid
    w_obs = budget_shares(P, X)
    err = [np.sum((budget_shares(P, crra_demand(P, r)) - w_obs) ** 2) for r in grid]
    return float(grid[int(np.argmin(err))])


def simulate_protocol(P_g: np.ndarray, rho_i: float, rho_j: float,
                      protocol: str, rng: Optional[np.random.Generator] = None,
                      noise_sd: float = 0.0) -> np.ndarray:
    """Group choices under a deliberation protocol with CRRA members.
    protocol in {"dict_i", "dict_j", "compromise", "random_dictator",
    "alternate"}.  `compromise` is the 50/50 point on the budget line between
    the two members' optimal bundles (a fixed-weight *point* rule, which is
    Pareto efficient with both goods strictly normal but need not be a
    fixed Pareto-weight rule).  Optional Gaussian noise in budget share."""
    rng = np.random.default_rng(0) if rng is None else rng
    Xi, Xj = crra_demand(P_g, rho_i), crra_demand(P_g, rho_j)
    K = P_g.shape[0]
    if protocol == "dict_i":
        X = Xi
    elif protocol == "dict_j":
        X = Xj
    elif protocol == "compromise":
        X = 0.5 * (Xi + Xj)
    elif protocol == "random_dictator":
        pick = rng.integers(0, 2, size=K)
        X = np.where(pick[:, None] == 0, Xi, Xj)
    elif protocol == "alternate":
        pick = np.arange(K) % 2
        X = np.where(pick[:, None] == 0, Xi, Xj)
    else:
        raise ValueError(protocol)
    if noise_sd > 0:
        w = np.clip(budget_shares(P_g, X) + rng.normal(0, noise_sd, K), 0, 1)
        X = np.column_stack([w / P_g[:, 0], (1 - w) / P_g[:, 1]])
    return X


def draw_budgets(K: int, rng: np.random.Generator, lo: float = 300,
                 hi: float = 3000, min_max: float = 1500) -> np.ndarray:
    """Budgets as in the experiment: intercepts U[lo, hi] with at least one
    intercept >= min_max.  Returns prices with income normalized to one."""
    out = []
    while len(out) < K:
        a, b = rng.uniform(lo, hi, size=2)
        if max(a, b) >= min_max:
            out.append((a, b))
    ab = np.array(out)
    return from_intercepts(ab[:, 0], ab[:, 1])


# ---------------------------------------------------------------------------
# 6. Command-line entry point
# ---------------------------------------------------------------------------


def main(argv: Optional[Sequence[str]] = None) -> None:
    ap = argparse.ArgumentParser(description="Collective efficiency index.")
    ap.add_argument("input", help="long CSV with columns " + ",".join(REQUIRED_COLS))
    ap.add_argument("--out", default="cei_pairwave.csv")
    ap.add_argument("--rounds-out", default="cei_rounds.csv")
    ap.add_argument("--power", type=int, default=0,
                    help="number of random-behaviour simulations per pair-wave (0 = skip)")
    ap.add_argument("--power-out", default="cei_power.csv")
    args = ap.parse_args(argv)

    df = pd.read_csv(args.input)
    pw, rd = run_panel(df)
    pw.to_csv(args.out, index=False)
    rd.to_csv(args.rounds_out, index=False)
    print(pw.groupby("spec")[["e_coll", "e_mean", "n_viol", "pass_coll",
                              "ccei_g", "pass_unitary"]].mean().round(3))

    if args.power > 0:
        gpool = df[df["role"] == "g"]
        pool = budget_shares(gpool[["p_r", "p_b"]].to_numpy(float),
                             gpool[["x_r", "x_b"]].to_numpy(float))
        rows = []
        for (pid, w), d in df.groupby(["pair_id", "wave"]):
            if not all((d["role"] == r).any() for r in ROLES):
                continue
            P_i, X_i = _arrays(d, "i"); P_j, X_j = _arrays(d, "j"); P_g, _ = _arrays(d, "g")
            for bench, ps in (("bronars", None), ("bootstrap", pool)):
                sim = power_random(P_i, X_i, P_j, X_j, P_g, n_sim=args.power,
                                   pool_shares=ps, seed=hash((pid, w)) % 2**32)
                rows.append(dict(pair_id=pid, wave=w, benchmark=bench,
                                 mean_e_coll=sim["e_coll"].mean(),
                                 pass_rate=(sim["e_coll"] >= 1 - TOL).mean()))
        pd.DataFrame(rows).to_csv(args.power_out, index=False)


if __name__ == "__main__":
    main()
