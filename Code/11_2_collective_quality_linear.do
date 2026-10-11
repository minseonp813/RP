* Figure 8: linear probability models with class and pair fixed effects.
* Run from Code. Four category indicators use the same sample and regressors.
clear all
set more off
adopath ++ "programs"
local out "results/collective_quality"
cap mkdir "`out'"
capture log close
cap mkdir "Logs"
log using "Logs/11_2_collective_quality_linear.log", text replace
do "programs/prepare_collective_sample.do" "`c(pwd)'/data"
use "data/collective_analysis_sample.dta", clear
isid group_id post
assert _N==1304
local full_controls "$t5_group $t5_friend $t5_share"
tempname ames
tempfile ame_results
postfile `ames' str13 specification byte outcome str7 member double estimate se low high ///
    double share long n pairs clusters using `ame_results'
foreach specification in class_fe full_controls pair_fe {
    local controls "`full_controls'"
    if "`specification'"=="class_fe" local controls ""
    local fe class_fe
    if "`specification'"=="pair_fe" local fe pair_fe
    collective_linear_ame joint_category, controls(`controls') fe(`fe')
    assert r(n)==1304 & r(pairs)==652 & r(clusters)==64
    matrix effects = r(effects)
    forvalues category=1/4 {
        forvalues member=1/2 {
            local name = cond(`member'==1,"maximum","minimum")
            local start = 4*(`member'-1)
            post `ames' ("`specification'") (`category') ("`name'") ///
                (effects[`category',`start'+1]) (effects[`category',`start'+2]) ///
                (effects[`category',`start'+3]) (effects[`category',`start'+4]) ///
                (effects[`category',9]) (1304) (652) (64)
        }
    }
}
postclose `ames'
use `ame_results', clear
export delimited using "`out'/joint_outcome_linear_ame.csv", replace
di as result "SUCCESS: all twelve linear probability models passed common-sample checks."
log close
