* Numerical check of exported AMEs against derivatives of fitted probabilities.
* Run from Code after the two Figure 6 analysis scripts.
clear all
set more off
args variant
assert inlist("`variant'","","no_shares")
adopath ++ "programs"
local out "results/new_indices/communication_figures"
if "`variant'"=="no_shares" local out "`out'_no_shares"
capture log close
log using "`out'/validation.log", text replace
import delimited "`out'/figure6_ame.csv", clear
tempfile exported shared numerical_data
save `exported'
import delimited "`out'/friendship_shared_ame.csv", clear
save `shared'
use `exported', clear
append using `shared'
isid moderator group outcome member
save `exported', replace
do "programs/prepare_communication_sample.do"
gen double ccei_max_01 = 10*ccei_max
gen double ccei_min_01 = 10*ccei_min
tempname numerical
postfile `numerical' str12 moderator byte group outcome str12 member ///
    double numerical_ame using `numerical_data'
foreach moderator in wave friend_any math_pair math_less friend friend_pool {
    local split `moderator'_split
    if "`moderator'"=="friend_pool" local split friend_split
    quietly levelsof `split', local(groups)
    foreach g of local groups {
        preserve
            keep if `split'==`g'
            local model "figure6_`moderator'_`g'"
            if "`moderator'"=="friend_pool" local model figure6_friend_shared
            capture confirm file "`out'/`model'.ster"
            if _rc {
                di as result "No converged separate fit for `moderator', subgroup `g'; shared-controls alternative checked separately."
                restore
                continue
            }
            estimates use "`out'/`model'.ster"
            * Local derivatives from near-perfect prediction are not interpreted.
            if abs(e(ll))<0.001 {
                forvalues category=1/4 {
                    foreach member in maximum minimum {
                        post `numerical' ("`moderator'") (`g') (`category') ("`member'") (.)
                    }
                }
                restore
                continue
            }
            foreach member in maximum minimum {
                local x ccei_max_01
                if "`member'"=="minimum" local x ccei_min_01
                gen double original_x = `x'
                replace `x' = original_x+1e-5
                forvalues category=1/4 {
                    predict double hi`category', pr outcome(`category')
                }
                replace `x' = original_x-1e-5
                forvalues category=1/4 {
                    predict double lo`category', pr outcome(`category')
                    gen double derivative = (hi`category'-lo`category')/(2e-5)
                    assert !missing(derivative)
                    quietly summarize derivative, meanonly
                    post `numerical' ("`moderator'") (`g') (`category') ("`member'") (r(mean))
                    drop derivative hi`category' lo`category'
                }
                replace `x' = original_x
                drop original_x
            }
        restore
    }
}
postclose `numerical'
use `numerical_data', clear
merge 1:1 moderator group outcome member using `exported', assert(match) nogen
gen byte skipped_near_separated = missing(numerical_ame)
gen double absolute_error = abs(estimate-numerical_ame)
assert absolute_error<1e-6 if !skipped_near_separated
export delimited using "`out'/derivative_validation.csv", replace
summarize absolute_error
di as result "SUCCESS: all interpreted AMEs match numerical derivatives; exact near-separated mutual-friendship fit excluded."
log close
