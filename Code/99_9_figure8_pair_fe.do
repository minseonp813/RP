* Figure 8 pair-FE review only; does not update the draft or its adopted estimates.
* Run from Code.
clear all
set more off
adopath ++ "programs"
local out "results/collective_quality/figure8_pair_fe_review"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace

do "programs/prepare_collective_sample.do" "`c(pwd)'/data"
use "data/collective_analysis_sample.dta", clear
isid group_id post
bysort pair_fe: assert _N==2
assert _N==1304
local controls "$t5_group $t5_friend $t5_share"

bysort pair_fe: gen byte changes_category = joint_category[1]!=joint_category[2]
egen byte pair_tag = tag(pair_fe)
count if pair_tag & changes_category
di as result "Pairs changing joint outcome: " r(N)

* Unconditional multinomial logit with pair dummies: exploratory with only two waves.
collective_multinomial_ame joint_category, controls(`controls') fe(pair_fe) base(1) iterations(30)
assert r(n)==1304 & r(pairs)==652 & r(clusters)==64
matrix effects = r(effects)
estimates save "`out'/pair_fe_multinomial.ster", replace
tempname ames
tempfile ame_results
postfile `ames' str13 specification byte outcome str7 member double estimate se low high ///
    double share long n pairs clusters using `ame_results'
forvalues category=1/4 {
    forvalues member=1/2 {
        local name = cond(`member'==1,"maximum","minimum")
        local start = 4*(`member'-1)
        post `ames' ("pair_fe") (`category') ("`name'") ///
            (effects[`category',`start'+1]) (effects[`category',`start'+2]) ///
            (effects[`category',`start'+3]) (effects[`category',`start'+4]) ///
            (effects[`category',9]) (1304) (652) (64)
    }
}
postclose `ames'
import delimited using "results/collective_quality/joint_outcome_ame.csv", clear
tempfile class_ames
save `class_ames'
use `ame_results', clear
append using `class_ames'
export delimited using "`out'/multinomial_ame.csv", replace
log close
