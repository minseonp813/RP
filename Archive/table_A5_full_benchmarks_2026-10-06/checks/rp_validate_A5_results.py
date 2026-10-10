import csv, math
from pathlib import Path
root=Path('/Users/minseonp/Library/CloudStorage/Dropbox/RP')
new=list(csv.DictReader((root/'Code/results/tables/table_bargaining_alternatives_M.csv').open()))
assert len(new)==18 and len({(r['measure'],int(r['column'])) for r in new})==18
for r in new:
    assert int(r['N'])==(2604 if r['measure']=='ra' else 2512)
    assert int(r['clusters'])==64
    assert all(math.isfinite(float(r[x])) for x in ['beta','se','p','beta_M','se_M','p_M','outcome_sd','focal_sd','standardized','r2'])
    assert float(r['se'])>0 and 0<=float(r['p'])<=1 and 0<=float(r['r2'])<=1
old=list(csv.DictReader((root/'Archive/table_A5_full_benchmarks_2026-10-06/Code/IminusM_review/outputs/tables/main_comparison.csv').open()))
old=[r for r in old if r['measure']=='ra' and r['outcome_model']=='adjusted']
assert len(old)==6
for r in [r for r in new if r['measure']=='ra']:
    col=int(r['column'])
    focal='HighCCEI_both_high' if col<=3 else 'ccei_gap_ij'
    spec=(col-1)%3+1
    o=next(o for o in old if o['focal']==focal and int(o['specification'])==spec)
    for x in ['beta','se','p','beta_M','se_M','p_M','outcome_sd','focal_sd','standardized','r2']:
        assert abs(float(r[x])-float(o[x]))<1e-7,(col,x,r[x],o[x])
print('PASS: all 18 fits have the intended sample, 64 class clusters and finite estimates; all six RA fits reproduce the retained full-benchmark results.')
for r in new:
    if r['measure']!='ra' and int(r['column']) in [3,6]:
        print(r['measure'],r['column'],'beta',r['beta'],'se',r['se'],'p',r['p'],'standardized',r['standardized'],'binary effect / outcome SD',float(r['beta'])/float(r['outcome_sd']))
