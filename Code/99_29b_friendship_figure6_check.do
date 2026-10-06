* Shared-controls alternative for the near-separated mutual-friendship subgroup.
* Run from Code after 99_29_communication_figure6.do.
clear all
set more off
args variant
assert inlist("`variant'","","no_shares")
adopath ++ "programs"
local out "results/new_indices/communication_figures"
if "`variant'"=="no_shares" local out "`out'_no_shares"
capture log close
log using "`out'/friendship_check.log", text replace
do "programs/prepare_communication_sample.do"
gen byte joint_category = 1+(ccei_g>=1-1e-9)+2*(ceiv_g>=1-1e-9)
gen double ccei_max_01 = 10*ccei_max
gen double ccei_min_01 = 10*ccei_min
local controls "$t5_group $t5_friend $t5_share"
if "`variant'"=="no_shares" local controls "$t5_group $t5_friend"
mlogit joint_category c.ccei_max_01##ib0.friend_split ///
    c.ccei_min_01##ib0.friend_split `controls' i.class_fe, ///
    baseoutcome(1) vce(cluster class_fe) difficult technique(bfgs 20 nr 20) iterate(200)
assert e(converged)==1
assert e(N)==1304
local fit_n = e(N)
local clusters = e(N_clust)
local ll = e(ll)
estimates save "`out'/figure6_friend_shared.ster", replace
tempname effects
tempfile effect_data
postfile `effects' str12 moderator byte group outcome str12 member ///
    double estimate se low high share long n pairs clusters using `effect_data'
forvalues g=0/2 {
    quietly count if friend_split==`g'
    local n = r(N)
    tempvar tag
    egen byte `tag' = tag(group_id) if friend_split==`g'
    quietly count if `tag'==1
    local pairs = r(N)
    forvalues category=1/4 {
        quietly count if friend_split==`g' & joint_category==`category'
        local share = r(N)/`n'
        margins if friend_split==`g', dydx(ccei_max_01 ccei_min_01) predict(outcome(`category'))
        matrix ame = r(table)
        forvalues member=1/2 {
            local name = cond(`member'==1,"maximum","minimum")
            post `effects' ("friend_pool") (`g') (`category') ("`name'") ///
                (ame[1,`member']) (ame[2,`member']) (ame[5,`member']) (ame[6,`member']) ///
                (`share') (`n') (`pairs') (`clusters')
        }
    }
}
postclose `effects'
use `effect_data', clear
assert !missing(estimate,se,low,high)
bysort group member: egen double sum = total(estimate)
assert abs(sum)<1e-8
drop sum
export delimited using "`out'/friendship_shared_ame.csv", replace
di as result "DONE: shared-controls friendship model; fit N=`fit_n', classes=`clusters', ll=`ll'."
log close
