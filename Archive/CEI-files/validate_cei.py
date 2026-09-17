"""
validate_cei.py -- checks for cei.py.

  A. Known-answer test: the single-observation example in the paper's
     Appendix (converse of Proposition 2(iii)).  Expected: CCEI of D^{ig} = 1,
     CCEI of D^{jg} = CCEI of D^{Ng} = 7/8; the group choice is not Pareto
     dominated (both members object but the join is unaffordable).
  B. Hand-built violation: both members reveal a preference for bundles
     whose join costs 0.85 of the group's budget.  Expected e_t = 0.85.
  C. Cross-checks on random data: (i) exact CCEI equals bisection CCEI,
     (ii) vectorized join equals an explicit double loop, (iii) tempering
     weakly raises e_t round by round, (iv) e_t = 1 whenever an objector set
     is empty.
  D. Protocol lineup with CRRA members (note, Section 6.2): every protocol
     is Pareto efficient round by round, so Test 1 must pass; random
     dictator and alternation should typically fail unitary GARP.
  E. Power against random behaviour (Bronars and bootstrap).
  F. Vacuity check (Lemma 1): Test 1 without tempering is trivially passed
     when members' data are replaced by the corner "personalized" datasets
     that rationalize anything -- i.e. the test's content comes entirely
     from the individual data.

Run:  python validate_cei.py
"""

import numpy as np
import pandas as pd

import cei

np.set_printoptions(precision=4, suppress=True)
rng = np.random.default_rng(20260903)
report = []


def log(s=""):
    print(s)
    report.append(s)


def bisection_ccei(P, X, tol=1e-10):
    C = cei.cost_matrix(P, X)
    lo, hi = 0.0, 1.0
    if not cei.has_e_violation(C, 1.0 - 1e-12):
        return 1.0
    while hi - lo > tol:
        mid = 0.5 * (lo + hi)
        if cei.has_e_violation(C, mid):
            hi = mid
        else:
            lo = mid
    return lo


def loop_join(P_g_t, X_i, X_j, A_i_t, A_j_t):
    best = np.inf
    for s in np.flatnonzero(A_i_t):
        for sp in np.flatnonzero(A_j_t):
            best = min(best, float(P_g_t @ np.maximum(X_i[s], X_j[sp])))
    return best


# ---------------------------------------------------------------- A
log("A. Known-answer test (paper Appendix example)")
xg, xj, xi = np.array([[5, .5]]), np.array([[2, 1.5]]), np.array([[5.1, .6]])
pg, pj, pi = np.array([[1/6, 1/3]]), np.array([[1/8, 1/2]]), np.array([[1/6, 1/4]])
c_ig = cei.ccei(np.vstack([pi, pg]), np.vstack([xi, xg]))
c_jg = cei.ccei(np.vstack([pj, pg]), np.vstack([xj, xg]))
c_Ng = cei.ccei(np.vstack([pi, pj, pg]), np.vstack([xi, xj, xg]))
log(f"   CCEI(D_ig) = {c_ig:.6f}  (expected 1)")
log(f"   CCEI(D_jg) = {c_jg:.6f}  (expected 7/8 = {7/8:.6f})")
log(f"   CCEI(D_Ng) = {c_Ng:.6f}  (expected 7/8)")
assert abs(c_ig - 1) < 1e-9 and abs(c_jg - 7/8) < 1e-9 and abs(c_Ng - 7/8) < 1e-9
res = cei.cei_pair_wave(pi, xi, pj, xj, pg, xg, tempered=False)
log(f"   |A_i|={res.n_obj_i[0]}, |A_j|={res.n_obj_j[0]}, join cost = "
    f"{pg[0] @ np.maximum(xi[0], xj[0]):.4f} > 1  ->  e_t = {res.e_t[0]:.4f}")
assert res.n_obj_i[0] == 1 and res.n_obj_j[0] == 1 and res.e_t[0] == 1
log("   PASS\n")

# ---------------------------------------------------------------- B
log("B. Hand-built violation")
pg, xg = np.array([[.25, .25]]), np.array([[3.8, .2]])
pi = np.array([[.2, (1 - .2 * 1.6) / 1.2]]); xi = np.array([[1.6, 1.2]])
pj = np.array([[.2, (1 - .2 * 1.2) / 1.8]]); xj = np.array([[1.2, 1.8]])
res = cei.cei_pair_wave(pi, xi, pj, xj, pg, xg, tempered=True)
log(f"   p_i.x_g = {pi[0]@xg[0]:.4f}, p_j.x_g = {pj[0]@xg[0]:.4f}  (both <= 1: both object)")
log(f"   join = {np.maximum(xi[0], xj[0])}, cost at p_g = {pg[0]@np.maximum(xi[0], xj[0]):.4f}")
log(f"   e_t = {res.e_t[0]:.4f} (expected 0.85); binding (s, s') = {res.argmin[0]}")
assert abs(res.e_t[0] - .85) < 1e-9
log("   PASS\n")

# ---------------------------------------------------------------- C
log("C. Cross-checks on random data (150 synthetic pair-waves, K = 18)")
n_ok = 0
max_dev = 0.0
for b in range(150):
    P = [cei.draw_budgets(18, rng) for _ in range(3)]
    # members: noisy CRRA; group: noisy mixture
    rho = rng.uniform(0.3, 4, size=2)
    X_i = cei.simulate_protocol(P[0], rho[0], rho[0], "dict_i", rng, noise_sd=0.15)
    X_j = cei.simulate_protocol(P[1], rho[1], rho[1], "dict_i", rng, noise_sd=0.15)
    X_g = cei.simulate_protocol(P[2], rho[0], rho[1], "random_dictator", rng, noise_sd=0.15)
    for (Pm, Xm) in ((P[0], X_i), (P[1], X_j), (P[2], X_g)):
        max_dev = max(max_dev, abs(cei.ccei(Pm, Xm) - bisection_ccei(Pm, Xm)))
    r_t = cei.cei_pair_wave(P[0], X_i, P[1], X_j, P[2], X_g, tempered=True)
    r_u = cei.cei_pair_wave(P[0], X_i, P[1], X_j, P[2], X_g, tempered=False)
    A_i = cei.objector_sets(P[0], X_i, P[2], X_g, r_t.e_i)
    A_j = cei.objector_sets(P[1], X_j, P[2], X_g, r_t.e_j)
    for t in range(18):
        lj = min(1.0, loop_join(P[2][t], X_i, X_j, A_i[:, t], A_j[:, t]))
        assert abs(lj - r_t.e_t[t]) < 1e-9, (lj, r_t.e_t[t])
        if r_t.n_obj_i[t] == 0 or r_t.n_obj_j[t] == 0:
            assert r_t.e_t[t] == 1
    assert np.all(r_t.e_t >= r_u.e_t - 1e-12)
    n_ok += 1
log(f"   {n_ok} pair-waves checked; max |exact CCEI - bisection CCEI| = {max_dev:.2e}")
log("   vectorized join == loop join; tempered e_t >= untempered e_t; empty objector set -> e_t = 1")
log("   PASS\n")

# ---------------------------------------------------------------- D
log("D. Protocol lineup, CRRA members rho_i = 0.5, rho_j = 3.0, 200 replications")
protocols = ["dict_i", "dict_j", "compromise", "random_dictator", "alternate"]
rows = []
for b in range(200):
    P_i, P_j, P_g = (cei.draw_budgets(18, rng) for _ in range(3))
    X_i, X_j = cei.crra_demand(P_i, 0.5), cei.crra_demand(P_j, 3.0)
    for pr in protocols:
        X_g = cei.simulate_protocol(P_g, 0.5, 3.0, pr, rng)
        r = cei.cei_pair_wave(P_i, X_i, P_j, X_j, P_g, X_g, tempered=True)
        rows.append(dict(protocol=pr, pass_coll=r.e_coll >= 1 - 1e-9,
                         e_coll=r.e_coll, pass_unitary=r.ccei_g >= 1 - 1e-9,
                         ccei_g=r.ccei_g))
tab = pd.DataFrame(rows).groupby("protocol", sort=False).mean()
log(tab.round(3).to_string())
assert tab.loc[["dict_i", "dict_j", "compromise", "random_dictator", "alternate"], "pass_coll"].min() == 1.0
log("   All protocols pass Test 1 (e_coll = 1); varying-weight protocols fail unitary GARP.  PASS\n")

# noisy members: tempering vs untempered on the same lineup
log("D'. Same lineup with noisy members (budget-share noise sd = 0.10): tempered vs untempered pass rates")
rows = []
for b in range(200):
    P_i, P_j, P_g = (cei.draw_budgets(18, rng) for _ in range(3))
    X_i = cei.simulate_protocol(P_i, 0.5, 0.5, "dict_i", rng, noise_sd=0.10)
    X_j = cei.simulate_protocol(P_j, 3.0, 3.0, "dict_i", rng, noise_sd=0.10)
    for pr in ["dict_i", "alternate"]:
        X_g = cei.simulate_protocol(P_g, 0.5, 3.0, pr, rng)
        for temp in (True, False):
            r = cei.cei_pair_wave(P_i, X_i, P_j, X_j, P_g, X_g, tempered=temp)
            rows.append(dict(protocol=pr, spec="tempered" if temp else "untempered",
                             pass_coll=r.e_coll >= 1 - 1e-9, e_coll=r.e_coll,
                             ccei_i=r.ccei_i, ccei_j=r.ccei_j))
log(pd.DataFrame(rows).groupby(["protocol", "spec"]).mean().round(3).to_string())
log()

# ---------------------------------------------------------------- E
log("E. Power against random behaviour (rational CRRA members, 60 pair-waves x 40 draws)")
rows = []
pool = None
for b in range(60):
    P_i, P_j, P_g = (cei.draw_budgets(18, rng) for _ in range(3))
    rho = rng.uniform(0.3, 4, size=2)
    X_i, X_j = cei.crra_demand(P_i, rho[0]), cei.crra_demand(P_j, rho[1])
    X_g = cei.simulate_protocol(P_g, rho[0], rho[1], "alternate", rng)
    actual = cei.cei_pair_wave(P_i, X_i, P_j, X_j, P_g, X_g)
    pool = cei.budget_shares(P_g, X_g) if pool is None else np.concatenate([pool, cei.budget_shares(P_g, X_g)])
    for bench, ps in (("bronars", None), ("bootstrap", pool)):
        sim = cei.power_random(P_i, X_i, P_j, X_j, P_g, n_sim=40, pool_shares=ps, seed=b)
        rows.append(dict(benchmark=bench, actual_e_coll=actual.e_coll,
                         random_e_coll=sim["e_coll"].mean(),
                         random_pass=(sim["e_coll"] >= 1 - 1e-9).mean(),
                         rho_gap=abs(rho[0] - rho[1])))
E = pd.DataFrame(rows)
log(E.groupby("benchmark")[["actual_e_coll", "random_e_coll", "random_pass"]].mean().round(3).to_string())
E["gap_bin"] = pd.cut(E["rho_gap"], [0, .5, 1.5, 4])
log("   Random-behaviour pass rate by |rho_i - rho_j| (pass rate of random behaviour RISES with disagreement: power falls as the Pareto interval widens):")
log(E.groupby(["benchmark", "gap_bin"], observed=True)["random_pass"].mean().round(3).to_string())
log()

# ---------------------------------------------------------------- F
log("F. Vacuity check (Lemma 1): corner 'members' u_i = x_r, u_j = x_b never object jointly")
P_g = cei.draw_budgets(18, rng)
X_g = cei.random_group_choices(P_g, rng)
P_i = cei.draw_budgets(18, rng); X_i = np.column_stack([1 / P_i[:, 0], np.zeros(18)])
P_j = cei.draw_budgets(18, rng); X_j = np.column_stack([np.zeros(18), 1 / P_j[:, 1]])
r = cei.cei_pair_wave(P_i, X_i, P_j, X_j, P_g, X_g, tempered=False)
log(f"   random group choices, corner members: e_coll = {r.e_coll:.4f}, unitary CCEI = {r.ccei_g:.4f}")
log("   (joins of corner objectors cost >= 1 by construction, so no violation is possible)")
assert r.e_coll == 1.0
log("   PASS")

with open("validation_report.txt", "w") as f:
    f.write("\n".join(report))
