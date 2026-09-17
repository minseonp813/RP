#!/usr/bin/env python3
"""Randomised differential test of the R library, part 1: generate datasets and compute the
three cross costs by INDEPENDENT brute force in exact arithmetic.

The reference implementations here deliberately avoid every shortcut the R code takes:
  * c_Sg is obtained by scanning efficiency levels and testing for a cross e-violation with the
    strongly-connected-component criterion -- it does NOT use the weakest-link lemma.
  * c^HM_Sg is obtained by trying every subset R in increasing size -- no hitting set, no
    constraint generation, no branch and bound.
  * c^MP_Sg is obtained by enumerating every simple cycle -- no Dinkelbach, no Lawler partition.

Data are integers in the same shape the package reads: budget axis intercepts (ix, iy) and a
chosen bundle (cx, cy).  With p = (1/ix, 1/iy),
    p^u . x^v = (cx_v*iy_u + cy_v*ix_u) / (ix_u*iy_u),
so with E[u][v] = cx_v*iy_u + cy_v*ix_u the comparison p^u.x^v <= e*(p^u.x^u) is the exact
integer/rational comparison E[u][v] <= e*E[u][u].
"""
import json, random, sys, os
from fractions import Fraction as F
from itertools import permutations, combinations


def build(ix, iy, cx, cy):
    n = len(ix)
    E = [[cx[v] * iy[u] + cy[v] * ix[u] for v in range(n)] for u in range(n)]
    den = [ix[u] * iy[u] for u in range(n)]
    return E, den, n


def edges(E, n, e=F(1)):
    """weak[u][v] : p^u.x^v <= e p^u.x^u ;  strict[u][v] : < ."""
    W = [[False] * n for _ in range(n)]
    S = [[False] * n for _ in range(n)]
    for u in range(n):
        thr = e * E[u][u]
        for v in range(n):
            if u == v:
                continue
            W[u][v] = F(E[u][v]) <= thr
            S[u][v] = F(E[u][v]) < thr
    return W, S


def sccs(W, n, keep):
    """Tarjan, restricted to `keep` (a set of node indices)."""
    idx = {}
    low = {}
    on = {}
    st = []
    out = []
    ctr = [0]

    def go(v):
        # iterative Tarjan to avoid recursion limits
        work = [(v, iter([w for w in keep if W[v][w]]))]
        idx[v] = low[v] = ctr[0]; ctr[0] += 1; st.append(v); on[v] = True
        while work:
            u, it = work[-1]
            advanced = False
            for w in it:
                if w not in idx:
                    idx[w] = low[w] = ctr[0]; ctr[0] += 1; st.append(w); on[w] = True
                    work.append((w, iter([z for z in keep if W[w][z]])))
                    advanced = True
                    break
                elif on.get(w):
                    low[u] = min(low[u], idx[w])
            if advanced:
                continue
            work.pop()
            if work:
                low[work[-1][0]] = min(low[work[-1][0]], low[u])
            if low[u] == idx[u]:
                comp = []
                while True:
                    w = st.pop(); on[w] = False; comp.append(w)
                    if w == u:
                        break
                out.append(comp)
    for v in keep:
        if v not in idx:
            go(v)
    return out


def has_cross_violation(E, n, side, keep, e=F(1)):
    """A cross e-violation exists iff some strongly connected component of R^0_e restricted to
    `keep` meets both sides and contains a strict edge internal to it."""
    W, S = edges(E, n, e)
    for comp in sccs(W, n, keep):
        cs = {side[v] for v in comp}
        if cs != {"I", "G"}:
            continue
        cset = set(comp)
        for u in comp:
            for v in comp:
                if u != v and S[u][v] and v in cset:
                    return True
    return False


def c_ccei(E, den, n, side, keep):
    """1 - sup{e : D admits no cross e-violation}.

    CAREFUL.  E_Sg = {e : no cross e-violation} is an interval (0, theta) or (0, theta]: the
    relations R^0_e only grow with e, so once a violation appears it persists.  Its supremum
    theta need NOT belong to it -- at e = theta the maximizing violation is still a chain of
    weak comparisons and keeps a strict step unless its per-step shares happen to be constant.
    Taking a maximum over any finite grid of levels therefore UNDERSTATES the supremum and
    overstates the cost.  Instead we locate theta as the breakpoint at which the status flips:
    the relations change only at the finitely many ratios (p^u.x^v)/(p^u.x^u), so the answer is
    constant on each open interval between consecutive ratios and one midpoint test per interval
    settles it."""
    keep = set(keep)
    if not has_cross_violation(E, n, side, keep, F(1)):
        return F(0)
    lv = {F(1)}
    for u in keep:
        for v in keep:
            if E[u][u] > 0:
                r = F(E[u][v], E[u][u])
                if 0 < r <= 1:
                    lv.add(r)
    lv = sorted(lv)
    prev = F(0)
    for b in lv:
        mid = (prev + b) / 2
        if mid > 0 and has_cross_violation(E, n, side, keep, mid):
            return 1 - prev          # clean up to prev, violating immediately above it
        prev = b
    return 1 - prev                  # violating only at e = 1 itself, so the supremum is 1


def c_hm(E, den, n, side, keep):
    """Smallest |R| with no cross violation left, by trying every subset in increasing size."""
    keep = list(keep)
    if not has_cross_violation(E, n, side, set(keep)):
        return 0
    for r in range(1, len(keep) + 1):
        for R in combinations(keep, r):
            if not has_cross_violation(E, n, side, set(keep) - set(R)):
                return r
    return len(keep)


def c_mp(E, den, n, side, keep):
    """Largest money pump over SIMPLE cross cycles, by complete enumeration."""
    keep = sorted(keep)
    W, S = edges(E, n, F(1))
    m = [F(E[u][u], den[u]) for u in range(n)]
    best = F(0)
    bestc = None
    for r in range(2, len(keep) + 1):
        for cyc in permutations(keep, r):
            if cyc[0] != min(cyc):
                continue
            if not all(W[cyc[a]][cyc[(a + 1) % r]] for a in range(r)):
                continue
            if {side[v] for v in cyc} != {"I", "G"}:
                continue
            if not any(S[cyc[a]][cyc[(a + 1) % r]] for a in range(r)):
                continue
            # paper's definition: p.x = 1 for every observation, so MPI is the UNWEIGHTED mean
            # of the per-step shares 1 - p^a.x^{a+1}/p^a.x^a (the shares are scale-free)
            val = sum(1 - F(E[cyc[a]][cyc[(a + 1) % r]], E[cyc[a]][cyc[a]]) for a in range(r)) / r
            if val > best:
                best, bestc = val, cyc
    return best, bestc


def gen(rng, n, mode):
    """Random budgets and choices.  `mode` shapes how adversarial the instance is."""
    ix = [rng.randint(200, 3000) for _ in range(n)]
    iy = [rng.randint(200, 3000) for _ in range(n)]
    cx, cy = [], []
    for k in range(n):
        if mode == "online":            # choice exactly on the budget line (integer-rounded)
            t = F(rng.randint(1, 99), 100)
            a = int(t * ix[k])
            b = int((1 - t) * iy[k])
        elif mode == "interior":        # strictly inside, creates many weak edges
            a = rng.randint(0, ix[k] - 1)
            b = rng.randint(0, max(0, int(iy[k] * (1 - a / ix[k]))))
        elif mode == "tight":           # near-identical budgets, choices spread along the line
            ix[k] = rng.randint(950, 1050)
            iy[k] = rng.randint(950, 1050)
            t = F(rng.randint(5, 95), 100)
            a, b = int(t * ix[k]), int((1 - t) * iy[k])
        else:                           # "corner": near an axis, makes lopsided prices
            if rng.random() < 0.5:
                a, b = rng.randint(int(0.7 * ix[k]), ix[k]), rng.randint(0, int(0.2 * iy[k]))
            else:
                a, b = rng.randint(0, int(0.2 * ix[k])), rng.randint(int(0.7 * iy[k]), iy[k])
        cx.append(max(1, a)); cy.append(max(1, b))
    return ix, iy, cx, cy


def main():
    seed = int(sys.argv[1]) if len(sys.argv) > 1 else 0
    ntrial = int(sys.argv[2]) if len(sys.argv) > 2 else 60
    rng = random.Random(seed)
    cases = []
    for t in range(ntrial):
        n = rng.choice([6, 7, 8, 8, 9])
        mode = rng.choice(["tight", "tight", "tight", "online", "interior", "corner"])
        ix, iy, cx, cy = gen(rng, n, mode)
        # split into an individual side and a group side, both non-empty
        ng = rng.randint(1, n - 1)
        side = ["I"] * (n - ng) + ["G"] * ng
        E, den, _ = build(ix, iy, cx, cy)
        keep = set(range(n))
        mp, cyc = c_mp(E, den, n, side, keep)
        cases.append(dict(
            id=t, n=n, mode=mode, ix=ix, iy=iy, cx=cx, cy=cy, side=side,
            ccei=str(c_ccei(E, den, n, side, keep)),
            hm=c_hm(E, den, n, side, keep),
            mpi=str(mp),
            mpi_float=float(mp),
            ccei_float=float(c_ccei(E, den, n, side, keep)),
            mpi_cycle=list(cyc) if cyc else None))
    out = os.path.join(os.path.dirname(os.path.abspath(__file__)), "difftest_cases.json")
    json.dump(cases, open(out, "w"))
    nz = sum(1 for c in cases if c["hm"] > 0)
    print("generated %d cases (%d with a cross violation) -> %s" % (len(cases), nz, out))
    print("  HM values seen:", sorted({c["hm"] for c in cases}))
    print("  cases with MPI > 0:", sum(1 for c in cases if c["mpi_float"] > 0))


if __name__ == "__main__":
    main()
