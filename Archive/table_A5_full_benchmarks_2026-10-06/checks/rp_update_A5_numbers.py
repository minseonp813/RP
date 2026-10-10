from pathlib import Path
import csv,re
root=Path('/Users/minseonp/Library/CloudStorage/Dropbox/RP')
rows=list(csv.DictReader((root/'Code/results/tables/table_bargaining_alternatives_M.csv').open()))
assert len(rows)==18
bykey={(r['measure'],int(r['column'])):r for r in rows}
assert len(bykey)==18
hm=bykey['hm',3];mpi=bykey['maxmpi',3]
hm_gap=bykey['hm',6];mpi_gap=bykey['maxmpi',6]
assert all(float(r['beta'])<0 for r in [hm,mpi,hm_gap,mpi_gap])
max_p=max(float(r['p']) for r in [hm,mpi,hm_gap,mpi_gap])
level=next((n for n in [1,5,10] if max_p<n/100),None)
assert level is not None,'The existing significance statement needs a wording decision.'
p=root/'Overleaf/main_v3.tex'
s=p.read_text()
paragraph=next(line for line in s.splitlines() if line.startswith(r'Appendix \autoref{tab:pref_agg_alternatives} confirms'))
replacements={
'$-0.083$':f"${float(hm['beta']):.3f}$",
'$-0.103$':f"${float(mpi['beta']):.3f}$",
r'34.3\%':f"{abs(float(hm['beta']))/float(hm['outcome_sd'])*100:.1f}"+r'\%',
r'35.3\%':f"{abs(float(mpi['beta']))/float(mpi['outcome_sd'])*100:.1f}"+r'\%',
'$0.168$':f"${abs(float(hm_gap['standardized'])):.3f}$",
'$0.259$':f"${abs(float(mpi_gap['standardized'])):.3f}$",
r'1\% level':str(level)+r'\% level',
}
updated=paragraph
for old,new in replacements.items():
    assert updated.count(old)==1,(old,updated)
    updated=updated.replace(old,new)
# The main-text wording must remain byte-for-byte identical apart from numerals.
assert re.sub(r'\d+(?:\.\d+)?','NUMBER',paragraph)==re.sub(r'\d+(?:\.\d+)?','NUMBER',updated)
p.write_text(s.replace(paragraph,updated))
print('Updated only Table A5 numbers in the existing main-text paragraph:')
print(updated)
print('Table A5 notes still describe the historical pilot; wording retained at user request.')
