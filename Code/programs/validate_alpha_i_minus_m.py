"""Independent checks of saved paired alpha simulations; writes a JSON audit."""
import argparse
import importlib.util
import json
import os
import sys
import tempfile
from pathlib import Path

os.environ.setdefault('MPLCONFIGDIR', str(Path(tempfile.gettempdir()) / 'rp-alpha-mpl'))
os.environ.setdefault('MPLBACKEND', 'Agg')
import numpy as np
import pandas as pd

code = Path(__file__).resolve().parents[1]
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--output-dir', type=Path, default=code/'results/alpha_i_minus_m')
parser.add_argument('--json-output', type=Path, help='Defaults to validation.json inside the selected output directory')
args = parser.parse_args()
if args.json_output is None:
    args.json_output = args.output_dir / 'validation.json'
spec = importlib.util.spec_from_file_location('rp_alpha_driver_validation', code / '99_17_alpha_i_minus_m.py')
driver = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = driver
spec.loader.exec_module(driver)
fn, lib = driver.notebook_functions(), driver.compiled_costs()
config = json.loads((args.output_dir / 'config.json').read_text())
rows = pd.read_csv(args.output_dir / 'simulation_results.csv')
cached = pd.read_csv(driver.SAVED / 'alpha_simulation_results.csv')
summary = pd.read_csv(args.output_dir / 'simulation_summary.csv')
checks = []
statistics = {}

def check(condition, message):
    assert bool(condition), message
    checks.append(message)

def close(a, b, atol=1e-12):
    return np.allclose(np.asarray(a, dtype=float), np.asarray(b, dtype=float), atol=atol, rtol=0, equal_nan=True)

def dict_data(a):
    return dict(zip(('intercept_x','intercept_y','coord_x','coord_y'), a.T))

def cost(a, split):
    return lib.alpha_cross_cost(np.ascontiguousarray(a), len(a), split)

check(not rows.duplicated(['setting','replication','true_alpha']).any(), 'unique setting/replication/alpha keys')
check(len(rows) == len(config['settings']) * config['replications_per_alpha'] * 11, 'complete configured row count')
for column, expected in [('I_minus_M', rows.I-rows.M), ('raw_alignment', 1-rows.I),
                         ('literal_alignment',1-rows.I+rows.M), ('centered_alignment',.5-rows.I+rows.M)]:
    check(close(rows[column], expected), f'{column} algebra holds without clipping')
for column, lower, upper in [('I',0,1),('M',0,1),('I_minus_M',-1,1),('literal_alignment',0,2),('centered_alignment',-.5,1.5)]:
    values = rows[column].dropna()
    check(values.between(lower-1e-12,upper+1e-12).all(), f'{column} within theoretical bounds')
check(rows.groupby(['setting','replication']).M.nunique().eq(1).all(), 'M fixed within target pair across all alpha values')
check(rows.groupby(['setting','replication']).ccei_i.nunique().eq(1).all() and rows.groupby(['setting','replication']).ccei_j.nunique().eq(1).all(), 'individual datasets fixed across alpha')

parameters = {s['setting']:s for s in driver.scenarios(fn,'all')}
for setting in config['settings']:
    expected = parameters[setting['setting']]
    for key in ('rho_i','rho_j','gamma_i','gamma_j','gamma_g','n_budgets','task_id'):
        check(close(setting[key],expected[key]), f"saved parameter reused: {setting['setting']} / {key}")

raw = [fn.prepare_raw(code/'data/base_raw.dta',0), fn.prepare_raw(code/'data/end_raw.dta',1)]
pool = fn.canonical_budget_pool(raw)
reference = json.loads((driver.SAVED/'utility_reference.json').read_text())['reference_consumption']
check(reference == 957.5, 'saved utility reference matches driver calibration')
check(config['calibrated_pool_rows'] == len(pool), 'budget pool reconstructs with same row count')
midpoint_max_error = 0.
midpoint_nearzero = []
midpoint_failures = []
midpoint_sensitive = []
midpoint_all_max_error = 0.
cost_max_error = 0.
monotonic_max_error = 0.
pair_symmetry_max_error = 0.
convergence = []
choice_identity_checks = 0
for setting in config['settings']:
    name=setting['setting']
    frame=rows.loc[rows.setting.eq(name)].sort_values(['replication','true_alpha'])
    cache=np.load(args.output_dir/'choices'/f'{name}.npz')
    donors=cache['donors']; members=cache['members']; own=cache['own_groups']; di=cache['donor_indices']
    nr=config['replications_per_alpha']; ng=setting['n_budgets']; nd=config['n_donors']
    check(members.shape==(nr,2*ng,4) and own.shape==(nr,11,ng,4) and donors.shape==(nd,ng,4), f'{name}: saved choice array dimensions')
    check(close(cache['alpha_grid'],driver.ALPHAS) and len(cache['donor_alpha'])==nd, f'{name}: complete alpha grids')
    check(np.all(np.unique(cache['donor_alpha'],return_counts=True)[1]==config['donors_per_alpha']), f'{name}: alpha-balanced unrelated donor pool')
    imputed=np.where(np.isfinite(di),di,.5)
    mean=imputed.mean(axis=1)
    check(close(frame.M.to_numpy().reshape(nr,11),np.repeat(mean[:,None],11,axis=1)), f'{name}: saved M equals imputed donor mean')
    half=np.tile(np.arange(config['donors_per_alpha'])%2==0,11)
    halfmean=imputed[:,half].mean(axis=1)
    check(close(frame.M_half.to_numpy().reshape(nr,11),np.repeat(halfmean[:,None],11,axis=1)), f'{name}: nested donor half benchmark')
    nvalid=np.isfinite(di).sum(axis=1)
    dropped=np.divide(np.where(np.isfinite(di),di,0).sum(axis=1),nvalid,out=np.full(nr,np.nan),where=nvalid>0)
    check(close(frame.M_drop.to_numpy().reshape(nr,11),np.repeat(dropped[:,None],11,axis=1)), f'{name}: excluded-undefined donor mean')
    undefined_pct=100*np.isnan(di).mean(axis=1)
    check(close(frame.donor_undefined_pct.to_numpy().reshape(nr,11),np.repeat(undefined_pct[:,None],11,axis=1)), f'{name}: undefined donor imputation accounting')
    convergence.append(dict(setting=name,mean_shift=float(np.mean(halfmean-mean)),mean_abs_shift=float(np.mean(abs(halfmean-mean))),max_abs_shift=float(np.max(abs(halfmean-mean)))))
    # Recompute one whole target's own and donor indices, then swap member labels.
    ni=driver.indices(lib,members[0],own[0])
    check(close(ni,frame.loc[frame.replication.eq(0)].I), f'{name}: native recomputation of all own alpha indices')
    check(close(driver.indices(lib,members[0],donors),di[0]), f'{name}: native recomputation of entire donor pool')
    swapped=np.ascontiguousarray(np.concatenate([members[0,ng:],members[0,:ng]]))
    sj=driver.indices(lib,swapped,own[0]); sjd=driver.indices(lib,swapped,donors)
    check(close(sj,1-ni) and close(sjd,1-di[0]), f'{name}: own and donor member-swap symmetry')
    check(abs(np.where(np.isfinite(sjd),sjd,.5).mean()+mean[0]-1)<1e-12, f'{name}: placebo benchmarks sum to one under member swap')
    pair_symmetry_max_error=max(pair_symmetry_max_error,float(np.nanmax(abs(sj+ni-1))) if np.isfinite(ni).any() else 0)
    # Compare three selected own choices with the original notebook's independent bisection.
    for alpha_index in (0,5,10):
        g=own[0,alpha_index]
        arrays=[np.concatenate([members[0,:ng],g]),np.concatenate([members[0,ng:],g]),np.concatenate([members[0],g])]
        costs=[]
        for a,split in zip(arrays,(ng,ng,2*ng)):
            new=cost(a,split)
            old=1-fn.ex_from_datasets(dict_data(a[:split]),dict_data(a[split:]))
            cost_max_error=max(cost_max_error,abs(new-old))
            check(abs(new-old)<1.1e-6, f'{name}: alpha {alpha_index/10:g}, native/notebook cross cost agreement')
            costs.append(new)
        monotonic_max_error=max(monotonic_max_error,max(costs[:2])-costs[2])
        check(max(costs[:2]) <= costs[2]+1e-12, f'{name}: coalition cross costs monotone')
        if costs[2]<=1e-9:
            check(np.isnan(ni[alpha_index]), f'{name}: zero-total actual cost remains undefined')
        else:
            check(abs(ni[alpha_index]-(.5+(costs[0]-costs[1])/(2*costs[2])))<1e-12, f'{name}: native index uses unmodified Shapley formula')
    if setting['design']=='calibrated':
        cfg=fn.SimulationConfig(master_seed=config['seed'],n_budgets=ng,simulation_replications=nr)
        rng=fn.seed_rng(cfg.master_seed,9000,setting['task_id'],0)
        i=fn.simulate_single_agent_dataset(pool,setting['rho_i'],setting['gamma_i'],'raw',reference,cfg,rng)
        j=fn.simulate_single_agent_dataset(pool,setting['rho_j'],setting['gamma_j'],'raw',reference,cfg,rng)
        ix,iy=fn.sample_budget_arrays(pool,ng,False,rng)
        g=fn.simulate_collective_dataset(ix,iy,setting['rho_i'],setting['rho_j'],.5,setting['gamma_g'],'raw',reference,cfg,rng)
        check(np.array_equal(np.concatenate([driver.choice_array(i),driver.choice_array(j)]),members[0]) and np.array_equal(driver.choice_array(g),own[0,5]), f'{name}: midpoint choices regenerate from original seed')
        choice_identity_checks+=1
        original=cached.loc[cached.heterogeneity.eq(setting['heterogeneity']) & cached.noise_scenario.eq(setting['noise_scenario']) & cached.group_precision.eq(setting['group_precision']) & cached.true_alpha.eq(.5)].set_index('replication')
        newframe=frame.loc[frame.true_alpha.eq(.5)].set_index('replication')
        common=original.index.intersection(newframe.index)
        for rep in common:
            old=original.loc[rep]; new=newframe.loc[rep]
            # Use exact native pooled cost to classify denominator-sensitive comparisons.
            cn=cost(np.concatenate([members[rep],own[rep,5]]),2*ng)
            ci=cost(np.concatenate([members[rep,:ng],own[rep,5]]),ng)
            cj=cost(np.concatenate([members[rep,ng:],own[rep,5]]),ng)
            check(max(abs(ci-old.c_ig),abs(cj-old.c_jg),abs(cn-old.c_Ng))<1.1e-6, f'{name}: midpoint cached costs agree, rep {rep}')
            if cn<=1e-3:
                midpoint_nearzero.append(dict(setting=name,replication=int(rep),c_Ng=float(cn),old_I=float(old.Ihat_ig) if np.isfinite(old.Ihat_ig) else None,new_I=float(new.I) if np.isfinite(new.I) else None))
            if np.isfinite(old.Ihat_ig) and np.isfinite(new.I):
                error=abs(old.Ihat_ig-new.I);midpoint_all_max_error=max(midpoint_all_max_error,error)
                bound=(abs(ci-old.c_ig)+abs(cj-old.c_jg))/(2*cn)+abs(old.c_ig-old.c_jg)*abs(cn-old.c_Ng)/(2*cn*old.c_Ng)
                check(error<=bound+1e-12, f'{name}: cached-I error obeys exact cost-sensitivity bound, rep {rep}')
                if error>2e-5:
                    record=dict(setting=name,replication=int(rep),error=float(error),c_Ng=float(cn),sensitivity_bound=float(bound))
                    if bound>=2e-5: midpoint_sensitive.append(record)
                    else: midpoint_failures.append(record)
                else: midpoint_max_error=max(midpoint_max_error,error)
        check(close(newframe.loc[common].ccei_i,original.loc[common].ccei_i,atol=1.1e-9) and close(newframe.loc[common].ccei_j,original.loc[common].ccei_j,atol=1.1e-9) and close(newframe.loc[common].ccei_g,original.loc[common].ccei_g,atol=1.1e-9), f'{name}: midpoint CCEIs reproduce saved calibrations')
    print(f'Validated {name}',flush=True)
check(not midpoint_failures, f'cached midpoint index agreement within 2e-5 outside small denominators: {midpoint_failures}')
check(close(driver.summarize(rows)['mean'],summary['mean']), 'summary means reproduce from row data')
# Identical observations yield no cost: actual missing, donor neutral after imputation.
fixture=np.array([[2.,2.,1.,1.]]*4)
fi=driver.indices(lib,np.ascontiguousarray(fixture[:2]),np.ascontiguousarray(fixture[2:][None,:,:]))
check(np.isnan(fi[0]) and np.where(np.isfinite(fi),fi,.5)[0]==.5, 'identical-choice fixture distinguishes undefined actual from imputed donor')
# Hand-constructed GARP fixtures distinguish cross violations from within-only cycles.
internal=np.array([[.5,1.,1.,0.],[1.,.5,0.,1.],[100.,100.,100.,100.]])
check(abs(cost(internal,0)-.5)<1e-12 and cost(internal,2)==0, 'within-only strict cycle contributes no cross cost')
cross=np.array([[.5,1.,1.,0.],[1.,.5,0.,1.]])
check(abs(cost(cross,1)-.5)<1e-12, 'strict cross cycle has known exact threshold one half')
ties=np.array([[1.,1.,1.,0.],[1.,1.,0.,1.]])
check(cost(ties,1)==0, 'pure expenditure-tie mixed cycle contributes no positive cross cost')
statistics.update(rows=len(rows),settings=len(config['settings']),checks=len(checks),choice_regeneration_settings=choice_identity_checks,
                  max_notebook_cost_error=cost_max_error,max_cached_midpoint_index_error_without_amplified_cases=midpoint_max_error,
                  nearzero_midpoint_cases=midpoint_nearzero,denominator_sensitive_midpoint_cases=midpoint_sensitive,max_cached_midpoint_index_error_all_finite=midpoint_all_max_error,max_pair_symmetry_error=pair_symmetry_max_error,
                  max_monotonicity_error=monotonic_max_error,
                  donor_convergence=convergence,
                  observed_ranges={key:[float(rows[key].min()),float(rows[key].max())] for key in ['I','M','I_minus_M','centered_alignment','literal_alignment']})
args.json_output.write_text(json.dumps(dict(status='PASS',statistics=statistics,checks=checks),indent=2,allow_nan=False)+'\n')
print(json.dumps(dict(status='PASS',statistics={k:v for k,v in statistics.items() if k not in ('nearzero_midpoint_cases','denominator_sensitive_midpoint_cases','donor_convergence')}),indent=2),flush=True)
print(f'Audit: {args.json_output}',flush=True)
