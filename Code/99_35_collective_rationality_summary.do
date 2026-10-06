* Run from Code. Review-only expansion of the category figure, Table 5, and Figure 6.
clear all
set more off
adopath ++ "programs"
local out "results/new_indices/collective_rationality_summary"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace

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
