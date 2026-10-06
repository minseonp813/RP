* Run from Code. Section 6.3 figures and appendix table, with validated CEIV inputs.
clear all
set more off
adopath ++ "programs"
local out "results/new_indices/collective_rationality_summary"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace

* Rebuild the CEIV input directly; no exploratory dofile is needed.
local data_dir "`c(pwd)'/data"
import excel "../CEI_CEIV_CEIC_statistics.xlsx", sheet("Group results") firstrow allstring clear
keep group_id post member_i_id member_j_id CEI CEIV CEIC ccei_i ccei_j ccei_group CEIC_lower CEIC_upper CEIV_lower CEIV_upper CEIC_status CEIV_status CEIC_precision_met CEIV_precision_met
rename (ccei_i ccei_j ccei_group CEI CEIV CEIC) (xlsx_ccei_i xlsx_ccei_j xlsx_ccei_g xlsx_cei ceiv_g ceic_g)
destring post xlsx_* ceiv_g ceic_g CEIC_lower CEIC_upper CEIV_lower CEIV_upper CEIC_precision_met CEIV_precision_met, replace
isid group_id post
assert _N==1304
assert member_i_id!=member_j_id
assert !missing(group_id,member_i_id,member_j_id)
assert !missing(ceiv_g,ceic_g)
assert inrange(ceiv_g,0,1) & inrange(ceic_g,0,1)
assert CEIV_precision_met==1
assert CEIC_lower<=ceic_g & ceic_g<=CEIC_upper
assert CEIV_lower<=ceiv_g & ceiv_g<=CEIV_upper
tempfile indices
save `indices'

use "`data_dir'/panel_individual.dta", clear
isid id post
local original_n = _N
merge m:1 group_id post using `indices', assert(match) nogen
assert _N==`original_n'
assert (id==member_i_id & partner_id==member_j_id) | (id==member_j_id & partner_id==member_i_id)
assert abs(ccei_i-xlsx_ccei_i)<1e-7 if id==member_i_id
assert abs(ccei_i-xlsx_ccei_j)<1e-7 if id==member_j_id
assert abs(ccei_j-xlsx_ccei_j)<1e-7 if partner_id==member_j_id
assert abs(ccei_j-xlsx_ccei_i)<1e-7 if partner_id==member_i_id
assert abs(ccei_g-xlsx_ccei_g)<1e-7
assert abs(cei_g-xlsx_cei)<1e-7
bysort group_id post: assert _N==2

use "`data_dir'/panel_group.dta", clear
isid group_id post
merge 1:1 group_id post using `indices', assert(match) nogen
assert id_mover==member_i_id & id_nonmover==member_j_id
save "`data_dir'/panel_group_new_indices.dta", replace

do "programs/prepare_collective_sample.do" "`c(pwd)'/data"
merge 1:1 group_id post using "data/panel_group_new_indices.dta", ///
    keepusing(ceiv_g CEIV_lower CEIV_upper) assert(match) nogen
gen double ccei_min = ccei_max-ccei_dist
assert _N==1304
assert abs(ccei_min-min(ccei_i,ccei_j))<1e-7
bysort group_id: assert _N==2 & class_fe==class_fe[1]
foreach x in ccei_g ceiv_g ccei_max ccei_min $t5_group $t5_friend $t5_share {
    assert !missing(`x')
}

* The median pools all individual observations across both waves.
preserve
    keep ccei_max ccei_min
    gen long observation = _n
    rename (ccei_max ccei_min) (individual_ccei1 individual_ccei2)
    reshape long individual_ccei, i(observation) j(member)
    quietly summarize individual_ccei, detail
    local median = r(p50)
    assert individual_ccei!=`median'
restore
gen byte max_high = ccei_max>`median'
gen byte min_high = ccei_min>`median'
gen byte pair_category = max_high+min_high
assert min_high<=max_high
gen byte low_high = pair_category==1
gen byte high_high = pair_category==2
gen double ccei_max_01 = 10*ccei_max
gen double ccei_min_01 = 10*ccei_min
gen byte joint_category = 1+(ccei_g>=1-1e-9)+2*(ceiv_g>=1-1e-9)
assert (ceiv_g>=1-1e-9)==(CEIV_lower>=1-1e-9)
assert (ceiv_g>=1-1e-9)==(CEIV_upper>=1-1e-9)
gen double pooled_median = `median'
save "`out'/analysis_sample.dta", replace

tempname coefficients diagnostics
tempfile coefficient_results model_results
postfile `coefficients' str4 outcome byte column str12 term ///
    double estimate double se double p using `coefficient_results'
postfile `diagnostics' str4 outcome byte column double equality_p double r2 ///
    long n long clusters long df_m using `model_results'
foreach outcome in ccei ceiv {
    forvalues column=1/6 {
        local specification = mod(`column'-1,3)+1
        local regressors "low_high high_high"
        if `column'>3 local regressors "ccei_max ccei_dist"
        local controls ""
        if `specification'>1 local controls "$t5_group $t5_friend $t5_share"
        local fe class_fe
        if `specification'==3 local fe pair_fe
        reghdfe `outcome'_g `regressors' `controls', ///
            absorb(`fe') vce(cluster class_fe)
        assert e(N)==1304 & e(N_clust)==64
        estimates save "`out'/table5_`outcome'_`column'.ster", replace
        foreach term in `regressors' {
            quietly lincom `term'
            post `coefficients' ("`outcome'") (`column') ("`term'") ///
                (r(estimate)) (r(se)) (r(p))
        }
        if `column'<=3 {
            quietly lincom high_high-low_high
            local comparison_p = r(p)
            post `coefficients' ("`outcome'") (`column') ("hh_minus_lh") ///
                (r(estimate)) (r(se)) (r(p))
        }
        else {
            * Preserve the test of equal member-specific slopes under max/gap.
            quietly test ccei_max+2*ccei_dist=0
            local comparison_p = r(p)
        }
        post `diagnostics' ("`outcome'") (`column') (`comparison_p') (e(r2)) (e(N)) (e(N_clust)) (e(df_m))
    }
}
postclose `coefficients'
postclose `diagnostics'
preserve
    use `coefficient_results', clear
    export delimited using "`out'/table5_coefficients.csv", replace
    use `model_results', clear
    export delimited using "`out'/table5_diagnostics.csv", replace
restore

* Figure 6 extension: four joint CCEI/CEIV outcomes, full controls and class effects.
collective_multinomial_ame joint_category, controls($t5_group $t5_friend $t5_share) base(1)
assert r(n)==1304 & r(pairs)==652 & r(clusters)==64
matrix effects = r(effects)
estimates save "`out'/figure6.ster", replace
* Independently evaluate derivatives of fitted category probabilities.
forvalues member=1/2 {
    local x ccei_max_01
    if `member'==2 local x ccei_min_01
    gen double original_x = `x'
    replace `x' = original_x+1e-5
    forvalues category=1/4 {
        predict double hi`category', pr outcome(`category')
    }
    replace `x' = original_x-1e-5
    forvalues category=1/4 {
        predict double lo`category', pr outcome(`category')
        gen double derivative = (hi`category'-lo`category')/(2e-5)
        quietly summarize derivative, meanonly
        assert abs(r(mean)-effects[`category',4*(`member'-1)+1])<1e-8
        drop derivative hi`category' lo`category'
    }
    replace `x' = original_x
    drop original_x
}
tempname ames
tempfile ame_results
postfile `ames' byte outcome str7 member double estimate double se double low ///
    double high double share long n long pairs long clusters using `ame_results'
forvalues category=1/4 {
    forvalues member=1/2 {
        local name = cond(`member'==1,"maximum","minimum")
        local start = 4*(`member'-1)
        post `ames' (`category') ("`name'") ///
            (effects[`category',`start'+1]) (effects[`category',`start'+2]) ///
            (effects[`category',`start'+3]) (effects[`category',`start'+4]) ///
            (effects[`category',9]) (1304) (652) (64)
    }
}
postclose `ames'
use `ame_results', clear
export delimited using "`out'/figure6_ame.csv", replace
di as result "SUCCESS: all twelve table models and the joint-outcome model passed sample and inference checks."
log close
