"""Write example_data.csv in the long format expected by cei.py (synthetic)."""
import numpy as np, pandas as pd, cei
rng = np.random.default_rng(1)
rows = []
protos = ["dict_i", "dict_j", "compromise", "random_dictator", "alternate"]
for pid in range(1, 21):
    for wave in (1, 2):
        rho = rng.uniform(0.3, 4, size=2)
        for role, r in (("i", rho[0]), ("j", rho[1])):
            P = cei.draw_budgets(18, rng)
            X = cei.simulate_protocol(P, r, r, "dict_i", rng, noise_sd=rng.uniform(0, .2))
            rows += [dict(pair_id=pid, wave=wave, role=role, round=k+1, p_r=P[k,0], p_b=P[k,1], x_r=X[k,0], x_b=X[k,1]) for k in range(18)]
        P = cei.draw_budgets(18, rng)
        X = cei.simulate_protocol(P, rho[0], rho[1], protos[pid % 5], rng, noise_sd=rng.uniform(0, .15))
        rows += [dict(pair_id=pid, wave=wave, role="g", round=k+1, p_r=P[k,0], p_b=P[k,1], x_r=X[k,0], x_b=X[k,1]) for k in range(18)]
pd.DataFrame(rows).to_csv("example_data.csv", index=False)
