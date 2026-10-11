* Run from Code. Figure 7 / Table A7 sensitivity; current paper outputs are retained.
* Optional argument: class_wave. That run also assigns median ties High as a check.
args median_scope
if "`median_scope'"=="" local median_scope "wave"
assert inlist("`median_scope'","wave","class_wave")
clear all
set more off
adopath ++ "programs"
local out "results/collective_quality/within_`median_scope'_median"
local strata "post"
local cutoff "wave_median"
local definitions "pooled wave"
if "`median_scope'"=="class_wave" {
    local strata "class post"
    local cutoff "class_wave_median"
    local definitions "pooled class_wave class_ties"
}
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace
use "data/collective_analysis_sample.dta", clear
isid group_id post
assert _N==1304
rename pair_category pooled_pair_category

* Count each member once in the classification cell.
preserve
    keep group_id class post ccei_max ccei_min
    rename (ccei_max ccei_min) (individual_ccei1 individual_ccei2)
    reshape long individual_ccei, i(group_id post) j(member)
    bysort `strata': egen double `cutoff' = median(individual_ccei)
    gen byte median_tie = individual_ccei==`cutoff'
    bysort `strata': assert mod(_N,2)==0
    collapse (first) `cutoff' (sum) median_ties=median_tie ///
        (count) individuals=individual_ccei, by(`strata')
    export delimited using "`out'/classification_cutoffs.csv", replace
    tempfile medians
    save `medians'
restore
merge m:1 `strata' using `medians', assert(match) nogen
gen byte pair_category = (ccei_min>`cutoff')+(ccei_max>`cutoff')
gen byte ties_high_pair_category = (ccei_min>=`cutoff')+(ccei_max>=`cutoff')
replace max_high = ccei_max>`cutoff'
replace min_high = ccei_min>`cutoff'
replace low_low = pair_category==0
replace high_high = pair_category==2
save "`out'/analysis_sample.dta", replace

local student "mathscore_max mathscore_dist height_max height_dist male_diff outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_max conscientious_dist stable_max stable_dist mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local friendship "inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist friend"
local shares "corner_share_max corner_share_dist mid_share_max mid_share_dist"
tempname coefficients diagnostics
tempfile coefficient_results model_results
postfile `coefficients' str10 definition str4 outcome byte column str12 term ///
    double estimate double se double p using `coefficient_results'
postfile `diagnostics' str10 definition str4 outcome byte column double r2 ///
    long n clusters using `model_results'
foreach definition in `definitions' {
    local category "pooled_pair_category"
    if "`definition'"!="pooled" local category "pair_category"
    if "`definition'"=="class_ties" local category "ties_high_pair_category"
    replace low_low = `category'==0
    replace high_high = `category'==2
    foreach outcome in ccei ceiv {
        forvalues column=1/3 {
            local controls ""
            if `column'>1 local controls "`student' `friendship' `shares'"
            local fe class_fe
            if `column'==3 local fe pair_fe
            quietly reghdfe `outcome'_g low_low high_high `controls', ///
                absorb(`fe') vce(cluster class_fe)
            assert e(N)==1304 & e(N_clust)==64
            foreach term in low_low high_high {
                quietly lincom `term'
                post `coefficients' ("`definition'") ("`outcome'") (`column') ///
                    ("`term'") (r(estimate)) (r(se)) (r(p))
            }
            post `diagnostics' ("`definition'") ("`outcome'") (`column') ///
                (e(r2)) (e(N)) (e(N_clust))
        }
    }
}
postclose `coefficients'
postclose `diagnostics'
use `coefficient_results', clear
export delimited using "`out'/table_coefficients.csv", replace
use `model_results', clear
export delimited using "`out'/table_diagnostics.csv", replace
di as result "SUCCESS: all models use the same 1,304 pair-waves and 64 classes."
log close
