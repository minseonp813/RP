* Run from Code. Optional full_gender adds male-male alongside mixed-sex indicators.
clear all
set more off
args variant
assert inlist("`variant'","","full_gender")
adopath ++ "programs"
local out "results/new_indices/non_male_male"
if "`variant'"=="full_gender" local out "`out'_full_gender"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace
do "programs/prepare_collective_sample.do" "`c(pwd)'/data"
local controls "$t5_group $t5_friend $t5_share"
use "results/new_indices/analysis_sample.dta", clear
isid group_id post
assert _N==1304
assert inlist(male_i,0,1) & inlist(male_j,0,1)
gen byte male_male = male_i==1 & male_j==1
if "`variant'"=="full_gender" local controls "`controls' male_male"
bysort group_id (post): assert male_male==male_male[1]
tabulate male_male post
gen byte joint_category = 1+(ccei_g>=1-1e-9)+2*(ceiv_g>=1-1e-9)
gen double ccei_max_01 = 10*ccei_max
gen double ccei_min_01 = 10*ccei_min

tempname handle
tempfile effects
postfile `handle' str13 sample byte outcome str7 member ///
    double estimate se low high share long n pairs clusters using `effects'
foreach sample in full non_male_male {
    local condition ""
    if "`sample'"=="non_male_male" local condition "if !male_male"
    tabulate joint_category `condition'
    collective_multinomial_ame joint_category `condition', controls(`controls') base(1)
    tempname ame
    matrix `ame' = r(effects)
    local n = r(n)
    local pairs = r(pairs)
    local clusters = r(clusters)
    if "`sample'"=="full" assert `n'==1304 & `pairs'==652 & `clusters'==64
    if "`sample'"=="non_male_male" assert `n'==654 & `pairs'==327 & `clusters'==40
    estimates save "`out'/figure6_`sample'.ster", replace

    * Independently verify AMEs as central differences of fitted probabilities.
    foreach member in maximum minimum {
        local x ccei_max_01
        local column = 1
        if "`member'"=="minimum" {
            local x ccei_min_01
            local column = 5
        }
        gen double original_x = `x'
        replace `x' = original_x+1e-5
        forvalues category=1/4 {
            predict double hi`category' if e(sample), pr outcome(`category')
        }
        replace `x' = original_x-1e-5
        forvalues category=1/4 {
            predict double lo`category' if e(sample), pr outcome(`category')
            gen double derivative = (hi`category'-lo`category')/(2e-5) if e(sample)
            quietly summarize derivative, meanonly
            assert abs(r(mean)-`ame'[`category',`column'])<1e-6
            drop derivative hi`category' lo`category'
        }
        replace `x' = original_x
        drop original_x
    }
    forvalues category=1/4 {
        forvalues member=1/2 {
            local name = cond(`member'==1,"maximum","minimum")
            local start = 4*(`member'-1)
            post `handle' ("`sample'") (`category') ("`name'") ///
                (`ame'[`category',`start'+1]) (`ame'[`category',`start'+2]) ///
                (`ame'[`category',`start'+3]) (`ame'[`category',`start'+4]) ///
                (`ame'[`category',9]) (`n') (`pairs') (`clusters')
        }
    }
}
postclose `handle'
use `effects', clear
isid sample outcome member
assert _N==16
export delimited using "`out'/figure6_ame.csv", replace
di as result "SUCCESS: both models converged, samples checked, and AMEs match numerical derivatives."
log close
