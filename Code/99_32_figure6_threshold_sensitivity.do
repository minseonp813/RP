* Run from Code. Relax group outcome cutoffs in the Figure 6 joint model.
clear all
set more off
args mode
if "`mode'"=="" local mode ccei_only
assert inlist("`mode'","ccei_only","both")
adopath ++ "programs"
local out "results/new_indices/threshold_sensitivity"
cap mkdir "`out'"
capture log close
log using "`out'/`mode'_analysis.log", text replace
* Reuse the established control lists; estimate on the saved Figure 6 sample.
do "programs/prepare_collective_sample.do" "`c(pwd)'/data"
use "results/new_indices/analysis_sample.dta", clear
isid group_id post
assert _N==1304
local controls "$t5_group $t5_friend $t5_share"
foreach x in `controls' ccei_min ccei_max ccei_g ceiv_g {
    assert !missing(`x')
}
gen double ccei_max_01 = 10*ccei_max
gen double ccei_min_01 = 10*ccei_min
gen byte joint_category = .
tempname handle
tempfile effects
postfile `handle' str9 mode double epsilon byte outcome str7 member ///
    double estimate se low high share long n pairs clusters using `effects'
foreach epsilon in .001 .01 .05 {
    local ceiv_epsilon = 1e-9
    if "`mode'"=="both" local ceiv_epsilon = `epsilon'
    replace joint_category = 1+(ccei_g>=1-`epsilon')+2*(ceiv_g>=1-`ceiv_epsilon')
    tabulate joint_category
    collective_multinomial_ame joint_category, controls(`controls') base(1)
    tempname ame
    matrix `ame' = r(effects)
    local n = r(n)
    local pairs = r(pairs)
    local clusters = r(clusters)
    assert `n'==1304 & `pairs'==652 & `clusters'==64
    forvalues category=1/4 {
        forvalues member=1/2 {
            local name = cond(`member'==1,"maximum","minimum")
            local start = 4*(`member'-1)
            post `handle' ("`mode'") (`epsilon') (`category') ("`name'") ///
                (`ame'[`category',`start'+1]) (`ame'[`category',`start'+2]) ///
                (`ame'[`category',`start'+3]) (`ame'[`category',`start'+4]) ///
                (`ame'[`category',9]) (`n') (`pairs') (`clusters')
        }
    }
}
postclose `handle'
import delimited "results/new_indices/figure6_a_ame.csv", clear
gen str9 mode = "`mode'"
gen double epsilon = 0
append using `effects'
isid epsilon outcome member
assert _N==32
sort epsilon outcome member
export delimited using "`out'/`mode'_ame.csv", replace
di as result "SUCCESS: identical samples, converged models, and adding-up checks passed."
log close
