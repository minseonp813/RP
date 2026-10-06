* Shared respondent-wave input for local risk-survey explorations; run from Code.
use "data/panel_individual.dta", clear
isid id post
merge m:1 group_id post using "data/panel_group_new_indices.dta", ///
    keepusing(ceiv_g CEIV_lower CEIV_upper) assert(match) nogen
assert _N == 2608
assert !missing(ccei_g, ceiv_g)
bysort group_id post: assert _N == 2
bysort group_id post: assert ccei_g[1] == ccei_g[2] & ceiv_g[1] == ceiv_g[2]
bysort group_id post: assert abs(Ihat_ig[1]+Ihat_ig[2]-1) < 1e-7 if !missing(Ihat_ig)
egen long class_fe = group(class)
foreach q in cooperation similar whose {
    local categories = cond("`q'" == "cooperation",5,4)
    assert inrange(risk_`q'_i,1,`categories') | missing(risk_`q'_i)
}
