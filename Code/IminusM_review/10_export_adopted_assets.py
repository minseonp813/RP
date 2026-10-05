"""Export the chosen review results to separate manuscript assets (run from any directory)."""
import csv
import shutil
from pathlib import Path

root = Path(__file__).resolve().parents[2]
out = root / 'Code/IminusM_review/outputs'
tables = root / 'Overleaf/tables_2025'
figures = root / 'Overleaf/figures_2025'

def read(name):
    return list(csv.DictReader((out/'tables'/name).open()))
def stars(p):
    p = float(p)
    return '**' if p < .01 else '*' if p < .05 else '+' if p < .1 else ''
def coefficient(r, key='beta', p='p'):
    return f"{float(r[key]):.3f}" + (r'\sym{' + stars(r[p]) + '}' if stars(r[p]) else '')
def row(label, values):
    return label + ' & ' + ' & '.join(values) + r' \\' + '\n'

for file in (out/'adopted').glob('*.tex'):
    shutil.copyfile(file, tables/file.name)
for suffix in ['bar','cdf']:
    name=f'ccei_IminusM_by_higher_ccei_{suffix}.png'
    shutil.copyfile(out/'figures'/name, figures/name)

# Existing adjusted HM/MaxMPI/RA review estimates, including benchmark coefficients.
source=read('alternative_measure_comparison.csv')+read('main_comparison.csv')
text=''
for measure,panel,title in [('hm','A','HM-based distance (50-donor benchmark)'),('maxmpi','B','MaxMPI-based distance (20-donor benchmark)'),('ra','C','Risk-aversion distance (651-donor benchmark)')]:
    d=[r for r in source if r['measure']==measure and r['outcome_model']=='adjusted']
    d=sorted(d,key=lambda r:(not r['focal'].startswith('High'),int(r['specification'])))
    assert len(d)==6
    text+=rf'\multicolumn{{7}}{{l}}{{\emph{{Panel {panel}: {title}}}}} \\'+'\n'
    for label,sel in [('Higher rationality',range(3)),('Rationality difference',range(3,6))]:
        text+=row(label,[coefficient(r) if i in sel else '' for i,r in enumerate(d)])
        text+=row('',[f"({float(r['se']):.3f})" if i in sel else '' for i,r in enumerate(d)])
    text+=row('$M_{ig}$',[coefficient(r,'beta_M','p_M') for r in d])
    text+=row('',[f"({float(r['se_M']):.3f})" for r in d])
    text+=row('N',[r['N'] for r in d])+row('R-squared',[f"{float(r['r2']):.3f}" for r in d])+r'\midrule'+'\n'
text+=row('Fixed effects',['Class','Class','Individual']*2)
text+=row('Individual, friendship, and choice-share controls',['',r'\checkmark',r'\checkmark']*2)+r'\bottomrule'+'\n'
(tables/'table_bargaining_alternatives_M.tex').write_text(text)

# Preserve the existing correlation table layout and update the I-M row/column.
source=read('baseline_correlations_IminusM.csv')
rows=['CCEI','Risk attitude','Placebo-adjusted distance','Out-degree','In-degree','Male','Height','Math score','RAT score','Outgoing','Openness','Agreeableness','Conscientiousness','Emotional stability']
text=r'\begin{tabular}{lccccc}\toprule'+'\n'+row('', ['CCEI','Risk attitude','$I-M$','Out-degree','In-degree'])+r'\midrule'+'\n'
for i,label in enumerate(rows):
    cells=[]
    for j,col in enumerate(rows[:5]):
        matches=[r for r in source if r['row']==label and r['column']==col]
        cells.append(coefficient(matches[0],'correlation') if j<i else '')
    text+=row('$I-M$' if label=='Placebo-adjusted distance' else label,cells)
text+=r'\bottomrule\end{tabular}'+'\n'
(tables/'table_correlation_IminusM.tex').write_text(text)
print('Exported adopted tables and Figure 2 assets.')
