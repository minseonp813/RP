* Run from Code. Exploratory communication-proxy heterogeneity in Table 5(3).
clear all
set more off
args variant
assert inlist("`variant'","","no_shares")
adopath ++ "programs"
local out "results/new_indices/communication_heterogeneity"
if "`variant'"=="no_shares" local out "`out'_no_shares"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace

do "programs/prepare_communication_sample.do"
local controls "$t5_group $t5_friend $t5_share"
if "`variant'"=="no_shares" local controls "$t5_group $t5_friend"

preserve
    collapse (count) pairwaves=ccei_g (sum) ccei_ties=ccei_tied ///
        less_math_missing=math_less_missing_pair, by(post)
    export delimited using "`out'/exclusions.csv", replace
restore
tempname balance
tempfile balance_data
postfile `balance' str9 moderator byte post group double cutoff ///
    long n median_ties using `balance_data'
foreach moderator in wave friend math_pair math_less {
    quietly levelsof `moderator'_split, local(groups)
    forvalues wave=0/1 {
        foreach g of local groups {
            quietly count if post==`wave' & `moderator'_split==`g'
            local n = r(N)
            local cutoff .
            local ties .
            if inlist("`moderator'","math_pair","math_less") {
                local suffix = subinstr("`moderator'","math_","",.)
                quietly summarize median_math_`suffix' if post==`wave', meanonly
                local cutoff = r(mean)
                local score math_pair_mean
                if "`moderator'"=="math_less" local score math_less
                quietly count if post==`wave' & `moderator'_split==`g' & `score'==`cutoff'
                local ties = r(N)
            }
            post `balance' ("`moderator'") (`wave') (`g') (`cutoff') (`n') (`ties')
        }
    }
    collective_split_ols `moderator'_split, name(`moderator') ///
        outdir("`out'") controls(`controls')
    if "`moderator'"!="wave" {
        collective_split_ols `moderator'_split, name(`moderator'_wavefe) ///
            outdir("`out'") controls(`controls' post)
    }
}
* Collapse friendship to the paper's existing any-nomination indicator.
collective_split_ols friend_any_split, name(friend_any) outdir("`out'") controls(`controls')
collective_split_ols friend_any_split, name(friend_any_wavefe) outdir("`out'") controls(`controls' post)
postclose `balance'
preserve
use `balance_data', clear
sort moderator post group
export delimited using "`out'/split_summary.csv", replace
restore
di as result "SUCCESS: four moderator splits, observed-score exclusions, subgroup OLS and formal cross-group/cross-outcome tests validated."
log close
