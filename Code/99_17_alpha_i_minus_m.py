"""Paired I versus I-M experiments using the existing alpha simulation notebook.

Run from any directory: python3 Code/99_17_alpha_i_minus_m.py
Requires NumPy, pandas, Matplotlib, joblib, and a C++ compiler. SciPy is only
needed if the original notebook's empirical calibration is re-estimated.
"""

from __future__ import annotations

import argparse
import ctypes
import hashlib
import json
import os
import subprocess
import sys
import tempfile
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from types import ModuleType

os.environ.setdefault("MPLCONFIGDIR", str(Path(tempfile.gettempdir()) / "rp-alpha-mpl"))
os.environ.setdefault("MPLBACKEND", "Agg")

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

CODE = Path(__file__).resolve().parent
SAVED = CODE / "results" / "alpha_weighted_simulation"
DEFAULT_OUTPUT = CODE / "results" / "alpha_i_minus_m"
ALPHAS = np.round(np.arange(0, 1.01, 0.1), 1)


def notebook_functions():
    notebook = json.loads((CODE / "99_17_alpha_weighted_simulation.ipynb").read_text())
    module = ModuleType("rp_alpha_notebook_functions")
    sys.modules[module.__name__] = module
    exec(compile("".join(notebook["cells"][2]["source"]), "alpha_notebook", "exec"), module.__dict__)
    return module


def compiled_costs():
    source = CODE / "programs" / "alpha_simulation_costs.cpp"
    fingerprint = hashlib.sha256(source.read_bytes()).hexdigest()[:16]
    suffix = ".dylib" if sys.platform == "darwin" else ".so"
    library = Path(tempfile.gettempdir()) / f"rp_alpha_costs_{fingerprint}{suffix}"
    if not library.exists():
        subprocess.run(["c++", "-O3", "-std=c++17", "-shared", "-fPIC", str(source), "-o", str(library)], check=True)
    lib = ctypes.CDLL(str(library))
    pointer = np.ctypeslib.ndpointer(dtype=np.float64, flags="C_CONTIGUOUS")
    lib.alpha_cross_cost.argtypes = [pointer, ctypes.c_int, ctypes.c_int]
    lib.alpha_cross_cost.restype = ctypes.c_double
    lib.alpha_batch_indices.argtypes = [pointer, ctypes.c_int, pointer, ctypes.c_int, ctypes.c_int, pointer]
    lib.alpha_batch_indices.restype = None
    return lib


def choice_array(data):
    return np.ascontiguousarray(np.column_stack([data[k] for k in ("intercept_x", "intercept_y", "coord_x", "coord_y")]))


def indices(lib, members, groups):
    groups = np.ascontiguousarray(groups, dtype=float)
    values = np.empty(len(groups), dtype=float)
    lib.alpha_batch_indices(members, len(members) // 2, groups, groups.shape[1], len(groups), values)
    return values


def scenarios(fn, design):
    rows = []
    if design in ("all", "fixed"):
        rows.append(dict(setting="fixed_rho", design="fixed", heterogeneity="fixed",
                         noise_scenario="30_30", group_precision="30", rho_i=0.3,
                         rho_j=0.7, gamma_i=30., gamma_j=30., gamma_g=30.,
                         n_budgets=50, task_id=10006))
    if design in ("all", "calibrated"):
        risks = pd.read_csv(SAVED / "rho_heterogeneity_scenarios.csv")
        gammas = pd.read_csv(SAVED / "gamma_calibration_selected.csv")
        # Use the original alpha=.5 seed for the target pair; keep it fixed
        # across alpha. This reproduces the cached midpoint choices exactly.
        for h, risk in enumerate(risks.itertuples(index=False)):
            for k, noise in enumerate(("high_high", "high_low", "low_low")):
                ti, tj = fn.parse_noise_scenario(noise)
                for g, precision in enumerate(("low", "median", "high")):
                    rows.append(dict(setting=f"{risk.heterogeneity}_{noise}_group_{precision}",
                                     design="calibrated", heterogeneity=risk.heterogeneity,
                                     noise_scenario=noise, group_precision=precision,
                                     rho_i=risk.rho_i, rho_j=risk.rho_j,
                                     gamma_i=fn.gamma_lookup(gammas,"raw",risk.heterogeneity,"i",ti),
                                     gamma_j=fn.gamma_lookup(gammas,"raw",risk.heterogeneity,"j",tj),
                                     gamma_g=fn.gamma_lookup(gammas,"raw",risk.heterogeneity,"g",precision),
                                     n_budgets=18, task_id=(h*9+k*3+g)*11+6))
    return rows


def make_donors(fn, pool, setting, cfg, reference, donor_reps, scenario_id):
    groups, alpha_values = [], []
    for alpha_index, alpha in enumerate(ALPHAS):
        for rep in range(donor_reps):
            rng = fn.seed_rng(cfg.master_seed, 9100, scenario_id, alpha_index, rep)
            ix, iy = fn.sample_budget_arrays(pool, cfg.n_budgets, False, rng)
            groups.append(choice_array(fn.simulate_collective_dataset(
                ix, iy, setting["rho_i"], setting["rho_j"], alpha,
                setting["gamma_g"], "raw", reference, cfg, rng)))
            alpha_values.append(alpha)
    return np.ascontiguousarray(groups), np.array(alpha_values)


def run_setting(fn, lib, pool, setting, args, scenario_id):
    cfg = fn.SimulationConfig(n_budgets=setting["n_budgets"], simulation_replications=args.replications,
                              master_seed=args.seed, n_jobs=args.jobs, verbose=0)
    reference = 957.5 if setting["design"] == "calibrated" else 0.9575
    donors, donor_alpha = make_donors(fn, pool, setting, cfg, reference, args.donor_reps, scenario_id)
    half_mask = np.tile(np.arange(args.donor_reps) % 2 == 0, len(ALPHAS))
    low_mask, high_mask = donor_alpha <= .3, donor_alpha >= .7

    def one_pair(rep):
        rng = fn.seed_rng(cfg.master_seed, 9000, setting["task_id"], rep)
        i = fn.simulate_single_agent_dataset(pool, setting["rho_i"], setting["gamma_i"], "raw", reference, cfg, rng)
        j = fn.simulate_single_agent_dataset(pool, setting["rho_j"], setting["gamma_j"], "raw", reference, cfg, rng)
        members = np.ascontiguousarray(np.concatenate([choice_array(i), choice_array(j)]))
        ix, iy = fn.sample_budget_arrays(pool, cfg.n_budgets, False, rng)
        choice_state = rng.bit_generator.state
        own_groups = []
        for alpha in ALPHAS:
            rng.bit_generator.state = choice_state
            own_groups.append(choice_array(fn.simulate_collective_dataset(
                ix, iy, setting["rho_i"], setting["rho_j"], alpha,
                setting["gamma_g"], "raw", reference, cfg, rng)))
        own_groups = np.ascontiguousarray(own_groups)
        actual = indices(lib, members, own_groups)
        donor_values = indices(lib, members, donors)
        imputed = np.where(np.isfinite(donor_values), donor_values, .5)
        mean_m = imputed.mean()
        ccei_i = 1-lib.alpha_cross_cost(choice_array(i), cfg.n_budgets, 0)
        ccei_j = 1-lib.alpha_cross_cost(choice_array(j), cfg.n_budgets, 0)
        rows = []
        for a, alpha in enumerate(ALPHAS):
            distance = actual[a]
            ccei_g = 1-lib.alpha_cross_cost(own_groups[a], cfg.n_budgets, 0)
            rows.append(dict(**setting, replication=rep, true_alpha=alpha,
                             I=distance, M=mean_m, I_minus_M=distance-mean_m,
                             raw_alignment=1-distance, literal_alignment=1-distance+mean_m,
                             centered_alignment=.5-distance+mean_m,
                             M_half=imputed[half_mask].mean(),
                             M_low_alpha=imputed[low_mask].mean(), M_high_alpha=imputed[high_mask].mean(),
                             M_drop=np.nanmean(donor_values) if np.isfinite(donor_values).any() else np.nan,
                             donor_undefined_pct=100*np.isnan(donor_values).mean(),
                             donor_mc_se=imputed.std(ddof=1)/np.sqrt(len(imputed)),
                             ccei_i=ccei_i,ccei_j=ccei_j,ccei_g=ccei_g))
        return rows, members, own_groups, donor_values

    started = time.monotonic()
    with ThreadPoolExecutor(max_workers=args.jobs) as executor:
        result = list(executor.map(one_pair, range(args.replications)))
    output = args.output_dir
    cache_dir = output / "choices"
    cache_dir.mkdir(exist_ok=True)
    np.savez_compressed(cache_dir / f"{setting['setting']}.npz", donors=donors, donor_alpha=donor_alpha,
                        members=np.stack([r[1] for r in result]), own_groups=np.stack([r[2] for r in result]),
                        donor_indices=np.stack([r[3] for r in result]), alpha_grid=ALPHAS)
    frame = pd.DataFrame([row for r in result for row in r[0]])
    print(f"{setting['setting']}: {len(frame)} rows, {time.monotonic()-started:.1f}s",flush=True)
    return frame


def summarize(results):
    rows=[]
    for (setting, alpha), sub in results.groupby(["setting","true_alpha"],sort=False):
        paired=sub.dropna(subset=["I"])
        for metric in ("raw_alignment","literal_alignment","centered_alignment","I_minus_M"):
            values=paired[metric]
            row=dict(setting=setting,true_alpha=alpha,metric=metric,n_runs=len(sub),n_defined=len(paired),
                     undefined_pct=100*(len(sub)-len(paired))/len(sub),mean=values.mean(),
                     p05=values.quantile(.05),p10=values.quantile(.10),p90=values.quantile(.90),p95=values.quantile(.95),
                     sd=values.std(),mean_M=paired.M.mean(),mean_ccei_i=sub.ccei_i.mean(),
                     mean_ccei_j=sub.ccei_j.mean(),mean_ccei_g=sub.ccei_g.mean())
            rows.append(row)
    return pd.DataFrame(rows)


def plot_band(ax, summary, metric, label, color):
    sub=summary.loc[summary.metric.eq(metric)].sort_values("true_alpha")
    x=sub.true_alpha.to_numpy()
    ax.fill_between(x,sub.p05.to_numpy(),sub.p95.to_numpy(),color=color,alpha=.10)
    ax.fill_between(x,sub.p10.to_numpy(),sub.p90.to_numpy(),color=color,alpha=.19)
    ax.plot(x,sub["mean"],"o-",color=color,label=label,markersize=4)


def plots(summary, settings, output):
    figures=output / "figures"
    figures.mkdir(exist_ok=True)
    for setting in settings:
        sub=summary.loc[summary.setting.eq(setting["setting"])]
        fig,axes=plt.subplots(1,3,figsize=(15,5),layout="constrained")
        plot_band(axes[0],sub,"raw_alignment","1-I","#1755a6")
        plot_band(axes[0],sub,"literal_alignment","1-(I-M)","#bc5a14")
        axes[0].axhline(1,color="gray",ls=":",lw=1,label="Adjusted neutrality")
        axes[0].set(title="Literal replacement",ylabel="Alignment score",ylim=(-.05,2.05))
        plot_band(axes[1],sub,"raw_alignment","1-I","#1755a6")
        plot_band(axes[1],sub,"centered_alignment","0.5-(I-M)","#bc5a14")
        axes[1].plot(ALPHAS,ALPHAS,"r--",lw=1,label="y = alpha (reference)")
        axes[1].axhline(.5,color="gray",ls=":",lw=1)
        axes[1].set(title="Same neutral center",ylabel="Centered alignment",ylim=(-.55,1.55))
        m=sub.loc[sub.metric.eq("raw_alignment")]
        axes[2].plot(m.true_alpha,m.mean_M,"o-",color="#278257",label="Mean M, defined own I")
        axes[2].axhline(.5,color="gray",ls=":",lw=1,label="M = 0.5")
        axes[2].set(title="Unrelated-group benchmark",ylabel="M",ylim=(-.02,1.02))
        for ax in axes:
            ax.set(xlabel="True alpha (weight on member i)",xticks=ALPHAS)
            ax.tick_params(axis="x",labelsize=8)
            ax.grid(alpha=.25)
            ax.legend(fontsize=8)
        undef=m.undefined_pct
        individual_label = setting['noise_scenario'].replace('_','-').title()
        if setting['design'] == 'fixed': individual_label = 'gamma = 30 for both members'
        fig.suptitle(f"rho = ({setting['rho_i']:.3f}, {setting['rho_j']:.3f}) | "
                     f"Individual precision: {individual_label} | Group: {setting['group_precision'].title()}\n"
                     f"Bands: 5-95% and 10-90% of defined own indices; undefined own I: {undef.min():.0f}-{undef.max():.0f}%",fontsize=12)
        fig.savefig(figures/f"{setting['setting']}.png",dpi=180)
        plt.close(fig)
    for precision in ("low","median","high"):
        selected=[s for s in settings if s["design"]=="calibrated" and s["group_precision"]==precision]
        if not selected:
            continue
        fig,axes=plt.subplots(3,3,figsize=(12,11),layout="constrained",sharex=True,sharey=True)
        for setting in selected:
            r=("small","medium","large").index(setting["heterogeneity"])
            c=("high_high","high_low","low_low").index(setting["noise_scenario"])
            ax=axes[r,c]
            sub=summary.loc[summary.setting.eq(setting["setting"])]
            plot_band(ax,sub,"raw_alignment","1-I","#1755a6")
            plot_band(ax,sub,"centered_alignment","0.5-(I-M)","#bc5a14")
            ax.plot(ALPHAS,ALPHAS,"r--",lw=.8)
            ax.axhline(.5,color="gray",ls=":",lw=.7)
            undefined=sub.loc[sub.metric.eq('raw_alignment'),'undefined_pct']
            ax.set(title=f"{setting['heterogeneity'].title()} gap | {setting['noise_scenario'].replace('_','-').title()}\n"
                         f"Undefined I: {undefined.min():.0f}-{undefined.max():.0f}%",ylim=(-.55,1.55))
            ax.title.set_fontsize(10)
            ax.grid(alpha=.2)
            if r==2: ax.set_xlabel("True alpha")
            if c==0: ax.set_ylabel("Centered alignment")
        axes[0,0].legend(fontsize=8)
        fig.suptitle(f"Group precision: {precision.title()} | Original and donor-adjusted alignment\n"
                     "Bands: 5-95% and 10-90%; means and bands use defined own indices",fontsize=13)
        fig.savefig(figures/f"risk_variants_group_{precision}.png",dpi=180)
        plt.close(fig)


def diagnostics_and_report(results, summary, settings, output):
    config=json.loads((output/'config.json').read_text())
    n_pairs=config['replications_per_alpha']
    n_donors=config['n_donors']
    n_half=11*((config['donors_per_alpha']+1)//2)
    diagnostics=[]
    for setting in settings:
        sub=results.loc[results.setting.eq(setting['setting'])]
        defined=sub.dropna(subset=['I'])
        means=defined.groupby('true_alpha')[['raw_alignment','centered_alignment']].mean()
        bands=summary.loc[summary.setting.eq(setting['setting'])]
        paired=sub.drop_duplicates('replication')
        row=dict(**setting,mean_M=paired.M.mean(),
                 mean_shift_centered=defined.centered_alignment.mean()-defined.raw_alignment.mean(),
                 undefined_pct=100*sub.I.isna().mean(),
                 donor_undefined_pct=paired.donor_undefined_pct.mean(),
                 mean_abs_M_half_difference=(paired.M-paired.M_half).abs().mean(),
                 max_abs_M_half_difference=(paired.M-paired.M_half).abs().max(),
                 mean_M_low_alpha=paired.M_low_alpha.mean(),mean_M_high_alpha=paired.M_high_alpha.mean(),
                 mean_abs_drop_impute_difference=(paired.M-paired.M_drop).abs().mean(),
                 pct_centered_outside_01=100*((defined.centered_alignment<0)|(defined.centered_alignment>1)).mean())
        for metric in ('raw_alignment','centered_alignment'):
            for alpha,name in ((0.,'alpha0'),(.5,'alpha05'),(1.,'alpha1')):
                row[f'{metric}_{name}']=means.loc[alpha,metric]
            row[f'{metric}_endpoint_span']=means.loc[1.,metric]-means.loc[0.,metric]
            metric_bands=bands.loc[bands.metric.eq(metric)]
            row[f'{metric}_mean_p90_p10_width']=(metric_bands.p90-metric_bands.p10).mean()
            # These distances to alpha are descriptive, not calibration claims.
            row[f'{metric}_rmse_to_alpha']=np.sqrt(np.mean((defined[metric]-defined.true_alpha)**2))
        diagnostics.append(row)
    table=pd.DataFrame(diagnostics)
    table.to_csv(output/'setting_diagnostics.csv',index=False)
    fixed=table.loc[table.design.eq('fixed')]
    calibrated=table.loc[table.design.eq('calibrated')]
    lines=[
        'I VERSUS I-M: MONTE CARLO COMPARISON',
        'Date: 2026-10-10', '',
        'Main result',
        'The literal replacement is 1-(I-M)=1-I+M, centered at 1. It is not on the original alpha scale.',
        'The supplementary score 0.5-(I-M)=1-I+(M-0.5) retains a neutral center of 0.5.',
        'I = 0.5 + (c_ig-c_jg)/(2*c_Ng), with c_Sg = 1 - cross-consistency threshold.',
        'M averages separately normalized donor-specific indices, keeping both target members fixed.',
        'Both adjusted scores describe own-group alignment relative to unrelated groups; neither is an estimated Pareto weight.',
        'Each target pair has a fixed M across alpha, so its centered curve is its original curve plus a constant.',
        'Mean-curve differences can also reflect changing sets of defined actual indices. The adjustment changes cross-pair spread.', '',
        'Choices made',
        f'Ran {len(settings)} settings and {len(results):,} target-alpha observations.',
        'All 27 existing cases combine three empirical CRRA gaps, High-High/High-Low/Low-Low individual precision, and Low/Median/High group precision.',
        'Saved CRRA pairs and gamma calibrations are reused; no empirical re-estimation was performed.',
        f"Each setting has {n_pairs} target pairs and an independent panel of {n_donors} unrelated groups ({config['donors_per_alpha']} at each of the 11 alpha values).",
        'The full donor panel and target individual choices stay fixed across target alpha. Both members use the same donor groups.',
        'For calibrated cases, target choices at alpha=0.5 use the original notebook RNG keys; other alphas use the same budgets and uniform draws.',
        'M is the mean of donor-specific normalized indices, with undefined donor indices imputed to 0.5; undefined own indices remain missing.',
        'The synthetic donor population is scenario-specific and balanced over alpha, rather than the empirical manuscript donor pool.',
        'Its purpose is a controlled Monte Carlo comparison. Conclusions about levels depend on this donor population.', '',
        'Reconstructed supplied figure',
        'The older figure executable, choice arrays, and random seed were not found. Its image and caption settings were found.',
        f'Reconstruction: rho=(0.3,0.7), gamma=(30,30,30), 50 budgets, 200 allocations, {n_pairs} pairs.',
        'Following simulation.tex, archived legacy rational-sample budgets are converted from KRW to dollar units by dividing by 1000.',
        'Expected utility uses equal state probabilities (0.5). Independent budget samples follow the later notebook.',
        'These conventions are explicit reconstruction choices, so the blue curve is not an exact reproduction of the supplied Monte Carlo draw.',
    ]
    if not fixed.empty:
        f=fixed.iloc[0]
        lines.extend([f'Mean M: {f.mean_M:.4f}; centered mean shift: {f.mean_shift_centered:+.4f}.',
                      f'At alpha 0/0.5/1, original means: {f.raw_alignment_alpha0:.3f}, {f.raw_alignment_alpha05:.3f}, {f.raw_alignment_alpha1:.3f}.',
                      f'Centered adjusted means: {f.centered_alignment_alpha0:.3f}, {f.centered_alignment_alpha05:.3f}, {f.centered_alignment_alpha1:.3f}.'])
    lines.extend(['','Representative calibrated cases (group precision High)',
                  'Risk gap / Individual precision     Mean M    Original at .5    Centered at .5    Original span    Centered span'])
    for row in table.loc[table.design.eq('calibrated') & table.group_precision.eq('high')].itertuples(index=False):
        lines.append(f'{row.heterogeneity:6} / {row.noise_scenario:10}    {row.mean_M:7.3f}       {row.raw_alignment_alpha05:7.3f}          {row.centered_alignment_alpha05:7.3f}           {row.raw_alignment_endpoint_span:7.3f}          {row.centered_alignment_endpoint_span:7.3f}')
    if not calibrated.empty:
        small_precise=summary.loc[summary.setting.eq('small_high_high_group_high') & summary.metric.eq('raw_alignment')]
        lines.extend(['',
            f'Across calibrated cases, M ranges from {calibrated.mean_M.min():.3f} to {calibrated.mean_M.max():.3f}.',
            f'The centered adjustment reduces the mean 10-90% band width in {(calibrated.centered_alignment_mean_p90_p10_width < calibrated.raw_alignment_mean_p90_p10_width).sum()} of 27 cases.',
            'Raw CRRA utility scales differ across risk parameters. Large-gap curves can saturate near 1 even at low positive alpha.',
            'Subtracting the donor benchmark changes the level but does not restore alpha sensitivity in saturated or nearly flat curves.',
            'Low-Low individual precision produces nearly flat curves; adjustment can narrow the bands without adding alpha sensitivity.',
            'Group precision is calibrated at alpha=0.5; the group CCEI need not equal its target at other alpha values.',
            'The figures show full theoretical score ranges; negative centered values are valid relative-alignment outcomes. No scores are clipped.',
            f"Own-index missingness ranges from {calibrated.undefined_pct.min():.1f}% to {calibrated.undefined_pct.max():.1f}% across calibrated settings.",
            f'Small gap/High-High members/High group precision has {small_precise.undefined_pct.min():.0f}-{small_precise.undefined_pct.max():.0f}% undefined own indices per alpha in this run.',
            'Its plotted means are conditional on defined own indices; subtraction cannot restore missing actual indices.',
            '', 'Donor and numerical checks',
            f'Average absolute M change when using the balanced {n_half}-donor half panel instead of {n_donors}: {table.mean_abs_M_half_difference.min():.4f}-{table.mean_abs_M_half_difference.max():.4f} across settings.',
            'Low-alpha and high-alpha donor averages are saved to show sensitivity to the donor population, not as preferred alternatives.',
            f'The mean difference between these donor populations ranges from {(table.mean_M_low_alpha-table.mean_M_high_alpha).min():+.3f} to {(table.mean_M_low_alpha-table.mean_M_high_alpha).max():+.3f}.',
            f'Average donor missingness across settings ranges from {table.donor_undefined_pct.min():.1f}% to {table.donor_undefined_pct.max():.1f}%.',
            f'The average absolute change in M from dropping rather than imputing undefined donors ranges from {table.mean_abs_drop_impute_difference.min():.4f} to {table.mean_abs_drop_impute_difference.max():.4f}.',
            'Missing-donor treatment matters particularly for Small gap/High-High members/High group precision; the main results follow production imputation.',
            'Cross costs use sorted expenditure-ratio thresholds and the production mixed-SCC/strict-edge criterion.',
            'Floating comparisons have relative tolerance 1e-12; actual indices are undefined when total cost <=1e-9.',
            'See validation.json for independent checks and midpoint replication tolerances.'])
    lines.extend(['','Files',
                  'simulation_results.csv: paired results, own I, M, direct I-M, both alignment transformations, donor sensitivities, CCEIs.',
                  'simulation_summary.csv: means, 5/10/90/95 percentiles, defined counts and CCEIs for each metric and alpha.',
                  'setting_diagnostics.csv: per-case shifts, endpoint spans, spread, donor-panel stability and missingness.',
                  'choices/*.npz: target, own-group and donor choices, and donor-specific indices for reproducible inspection.',
                  'figures/fixed_rho.png: literal replacement, centered comparison, and M for reconstructed caption settings.',
                  'figures/risk_variants_group_{low,median,high}.png: all 27 calibrated cases.',
                  'figures/*.png: individual three-panel comparison for each case.',
                  'config.json: seeds, parameter values, currency/budget conventions and input hashes.',
                  'Original code: Code/99_17_alpha_weighted_simulation.ipynb.',
                  'New driver: Code/99_17_alpha_i_minus_m.py; native cross-cost helper: Code/programs/alpha_simulation_costs.cpp.',
                  '', 'Rerun', 'python3 Code/99_17_alpha_i_minus_m.py --jobs 6', ''])
    (output/'report.txt').write_text('\n'.join(lines))
    return table


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--design",choices=["all","fixed","calibrated"],default="all")
    parser.add_argument("--replications",type=int,default=100)
    parser.add_argument("--donor-reps",type=int,default=20,help="Donors per each of 11 alpha values")
    parser.add_argument("--seed",type=int,default=20260823)
    parser.add_argument("--jobs",type=int,default=min(6,os.cpu_count() or 1))
    parser.add_argument("--output-dir",type=Path,default=DEFAULT_OUTPUT)
    args=parser.parse_args()
    if min(args.replications,args.donor_reps,args.jobs)<1: parser.error("Counts must be positive")
    args.output_dir.mkdir(parents=True,exist_ok=True)
    fn,lib=notebook_functions(),compiled_costs()
    raw=[fn.prepare_raw(CODE/"data"/"base_raw.dta",0),fn.prepare_raw(CODE/"data"/"end_raw.dta",1)]
    calibrated_pool=fn.canonical_budget_pool(raw)
    legacy=CODE.parent/"Archive"/"Code"/"Archive"/"Replication Package_BH"/"data"/"baseline_raw_129groups.csv"
    fixed_pool=fn.legacy_rational_budget_pool(legacy)
    fixed_pool[["intercept_x","intercept_y"]]/=1000.
    settings=scenarios(fn,args.design)
    manifest=dict(seed=args.seed,replications_per_alpha=args.replications,donors_per_alpha=args.donor_reps,
                  n_donors=11*args.donor_reps,alpha_grid=ALPHAS.tolist(),settings=settings,
                  donor_design="Independent scenario-specific panel, equal mass over all 11 alpha values; held fixed across target alpha",
                  target_design="Fixed target individual choices and common group budgets/uniform draws across alpha; original cached alpha=.5 seeds",
                  donor_undefined_rule="Impute donor I=0.5; undefined actual I remains missing",
                  ccei_method="Sorted floating expenditure-ratio search, mixed SCC with strict edge, zero cost threshold 1e-9",
                  fixed_reconstruction="Caption settings; archived legacy rational budget pool / 1000 (dollar units), independent budgets; expected utility uses equal state probability 0.5",
                  fixed_provenance_limit="Original executable and random seed absent; reconstruction is not an exact replication of original image",
                  calibrated_currency="Original KRW units, raw CRRA; saved risk pairs and precision calibration reused",
                  calibrated_pool_rows=len(calibrated_pool),fixed_pool_rows=len(fixed_pool),
                  code_sha256={str(p.relative_to(CODE.parent)):hashlib.sha256(p.read_bytes()).hexdigest() for p in
                               [Path(__file__),CODE/"programs"/"alpha_simulation_costs.cpp"]},
                  input_sha256={str(p.relative_to(CODE.parent)):hashlib.sha256(p.read_bytes()).hexdigest() for p in
                                [CODE/"99_17_alpha_weighted_simulation.ipynb",CODE/"data"/"base_raw.dta",CODE/"data"/"end_raw.dta",
                                 SAVED/"rho_heterogeneity_scenarios.csv",SAVED/"gamma_calibration_selected.csv",legacy]})
    (args.output_dir/"config.json").write_text(json.dumps(manifest,indent=2)+"\n")
    frames=[]
    for k,setting in enumerate(settings):
        pool=fixed_pool if setting["design"]=="fixed" else calibrated_pool
        frames.append(run_setting(fn,lib,pool,setting,args,k+1))
    results=pd.concat(frames,ignore_index=True)
    results.to_csv(args.output_dir/"simulation_results.csv",index=False)
    summary=summarize(results)
    summary.to_csv(args.output_dir/"simulation_summary.csv",index=False)
    plots(summary,settings,args.output_dir)
    diagnostics_and_report(results,summary,settings,args.output_dir)
    print(f"Saved {len(results)} paired simulation rows and {len(settings)} setting comparisons to {args.output_dir}",flush=True)


if __name__=="__main__":
    main()
