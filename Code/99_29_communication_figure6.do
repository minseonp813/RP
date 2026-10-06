* Run from Code. Figure 6 separately by the communication-proxy subgroups.
clear all
set more off
args variant
assert inlist("`variant'","","no_shares")
adopath ++ "programs"
local out "results/new_indices/communication_figures"
if "`variant'"=="no_shares" local out "`out'_no_shares"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace
do "programs/prepare_communication_sample.do"
gen byte ceiv_endpoint = ceiv_g>=1-1e-9
assert ceiv_endpoint == (CEIV_lower>=1-1e-9)
assert ceiv_endpoint == (CEIV_upper>=1-1e-9)
gen byte joint_category = 1+(ccei_g>=1-1e-9)+2*ceiv_endpoint
gen double ccei_max_01 = 10*ccei_max
gen double ccei_min_01 = 10*ccei_min
local controls "$t5_group $t5_friend $t5_share"
if "`variant'"=="no_shares" local controls "$t5_group $t5_friend"

tempname effects status
tempfile effect_data status_data
postfile `effects' str10 moderator byte group outcome str12 member ///
    double estimate se low high share long n pairs clusters using `effect_data'
postfile `status' str10 moderator byte group base attempts success ///
    double ll long eligible n pairs clusters using `status_data'
foreach moderator in wave friend_any math_pair math_less friend {
    local split `moderator'_split
    quietly levelsof `split', local(groups)
    foreach g of local groups {
        quietly count if `split'==`g'
        local eligible = r(N)
        di as result "Figure 6: `moderator', subgroup `g', N=`eligible'"
        tabulate joint_category if `split'==`g'
        local success = 0
        local attempt = 0
        foreach base in 1 4 {
            local ++attempt
            capture noisily collective_multinomial_ame joint_category if `split'==`g', ///
                controls(`controls') base(`base') technique("bfgs 20 nr 20") iterations(150)
            if _rc==0 {
                local success = 1
                local chosen_base = `base'
                continue, break
            }
            di as result "Fit attempt `attempt' failed for `moderator' subgroup `g'; return code " _rc
        }
        if `success' {
            tempname ame
            matrix `ame' = r(effects)
            local n = r(n)
            local pairs = r(pairs)
            local clusters = r(clusters)
            local ll = r(ll)
            assert `n'==`eligible'
            estimates save "`out'/figure6_`moderator'_`g'.ster", replace
            forvalues category=1/4 {
                forvalues member=1/2 {
                    local name = cond(`member'==1,"maximum","minimum")
                    local start = 4*(`member'-1)
                    post `effects' ("`moderator'") (`g') (`category') ("`name'") ///
                        (`ame'[`category',`start'+1]) (`ame'[`category',`start'+2]) ///
                        (`ame'[`category',`start'+3]) (`ame'[`category',`start'+4]) ///
                        (`ame'[`category',9]) (`n') (`pairs') (`clusters')
                }
            }
            post `status' ("`moderator'") (`g') (`chosen_base') (`attempt') (1) ///
                (`ll') (`eligible') (`n') (`pairs') (`clusters')
        }
        else post `status' ("`moderator'") (`g') (.) (`attempt') (0) ///
            (.) (`eligible') (.) (.) (.)
    }
}
postclose `effects'
postclose `status'
use `effect_data', clear
export delimited using "`out'/figure6_ame.csv", replace
use `status_data', clear
export delimited using "`out'/model_status.csv", replace
di as result "DONE: inspect model_status.csv for subgroup convergence and AME validation."
log close
