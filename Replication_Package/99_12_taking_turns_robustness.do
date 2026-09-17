* Taking-turns robustness.

clear all
set more off
set matsize 8000
args expected_reps bootstrap_reps mode definition
if "`expected_reps'" == "" local expected_reps 200
if "`bootstrap_reps'" == "" local bootstrap_reps 999

local code_dir `"`c(pwd)'"'
local replication_dir "`code_dir'"
local data_dir "`replication_dir'/data"
local result_dir "`code_dir'/results/taking_turns_permutation"
local table_dir "`code_dir'/results/tables/taking_turns"

if "`definition'" == "" {
    foreach definition in both_high both_low ccei_diff {
        do "`code_dir'/99_12_taking_turns_robustness.do" `expected_reps' `bootstrap_reps' run `definition'
    }
    exit
}
if "`definition'" == "both_high" {
    local xvar HighCCEI_both_high
    local suffix ""
}
else if "`definition'" == "both_low" {
    local xvar HighCCEI_both_low
    local suffix "_bothlow"
}
else if "`definition'" == "ccei_diff" {
    local xvar ccei_gap_ij
    local suffix "_cceidiff"
}
else {
    di as error "Unknown definition: `definition'"
    exit 198
}
global TT_XVAR "`xvar'"

if "`mode'" != "export" {
capture mkdir "`result_dir'"
capture mkdir "`code_dir'/results/tables"
capture mkdir "`table_dir'"
capture log close ttlog
log using "`result_dir'/taking_turns_robustness`suffix'.log", text replace name(ttlog)

use "`data_dir'/panel_individual.dta", clear

* Merge permutation results.
merge m:1 group_id post using "`result_dir'/taking_turns_pairwave.dta", ///
    keep(master match) nogen keepusing(n_permutations turn_like)

assert n_permutations == `expected_reps' if !missing(n_permutations)
count if !missing(n_permutations)
assert r(N) > 0
assert inlist(turn_like, 0, 1) if !missing(turn_like)

* Prepare controls.

tempfile choice_shares partner_choice_shares baseline_choice_shares
preserve
    use "`data_dir'/base_raw.dta", clear
    keep if game_type == 1
    gen byte post = 0
    gen byte corner_share = (coord_x == 0 | coord_y == 0) ///
        if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    save `baseline_choice_shares'

    use "`data_dir'/end_raw.dta", clear
    keep if game_type == 1
    gen byte post = 1
    gen byte corner_share = (coord_x == 0 | coord_y == 0) ///
        if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    append using `baseline_choice_shares'
    save `choice_shares'
    rename id partner_id
    save `partner_choice_shares'
restore

merge m:1 id post using `choice_shares', keep(master match) nogen
rename corner_share corner_share_i
rename mid_share mid_share_i
merge m:1 partner_id post using `partner_choice_shares', ///
    keep(master match) nogen
rename corner_share corner_share_j
rename mid_share mid_share_j

gen double corner_share_diff = corner_share_i - corner_share_j
gen double mid_share_diff = mid_share_i - mid_share_j
gen byte female_i_male_j = (male_i == 0 & male_j == 1)
gen byte male_i_female_j = (male_i == 1 & male_j == 0)

global tt_group "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff female_i_male_j male_i_female_j"
global tt_group_nogender "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
global tt_friend "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
global tt_missing "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global tt_ra "RA_i RA_diff"
global tt_share "corner_share_i corner_share_diff mid_share_i mid_share_diff"

bysort id: egen n_distance = total(!missing(Ihat_ig))
gen byte balanced_t3 = (n_distance == 2)
drop n_distance
egen long id_fe = group(id)

label var `xvar' "\$Higher CCEI_i\$"

* Estimate full and restricted samples.

eststo clear
eststo full1: reghdfe Ihat_ig `xvar' if balanced_t3, ///
    absorb(class) vce(cluster class)
scalar b_full1 = _b[`xvar']
eststo full2: reghdfe Ihat_ig `xvar' $tt_group $tt_friend $tt_missing ///
    if balanced_t3, absorb(class) vce(cluster class)
scalar b_full2 = _b[`xvar']
eststo full3: reghdfe Ihat_ig `xvar' $tt_group $tt_friend $tt_missing ///
    $tt_ra $tt_share if balanced_t3, absorb(class) vce(cluster class)
scalar b_full3 = _b[`xvar']
eststo full4: reghdfe Ihat_ig `xvar' $tt_group_nogender $tt_friend ///
    $tt_missing $tt_ra $tt_share if balanced_t3, ///
    absorb(id_fe) vce(cluster class)
scalar b_full4 = _b[`xvar']

eststo drop1: reghdfe Ihat_ig `xvar' if balanced_t3 & turn_like != 1, ///
    absorb(class) vce(cluster class)
scalar b_drop1 = _b[`xvar']
eststo drop2: reghdfe Ihat_ig `xvar' $tt_group $tt_friend $tt_missing ///
    if balanced_t3 & turn_like != 1, absorb(class) vce(cluster class)
scalar b_drop2 = _b[`xvar']
eststo drop3: reghdfe Ihat_ig `xvar' $tt_group $tt_friend $tt_missing ///
    $tt_ra $tt_share if balanced_t3 & turn_like != 1, ///
    absorb(class) vce(cluster class)
scalar b_drop3 = _b[`xvar']
eststo drop4: reghdfe Ihat_ig `xvar' $tt_group_nogender $tt_friend ///
    $tt_missing $tt_ra $tt_share if balanced_t3 & turn_like != 1, ///
    absorb(id_fe) vce(cluster class)
scalar b_drop4 = _b[`xvar']

esttab full1 full2 full3 full4 drop1 drop2 drop3 drop4 using ///
    "`table_dir'/table_taking_turns_exclusion`suffix'.tex", replace ///
    b(3) se(3) keep(`xvar') ///
    stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    mgroups("Full sample" "Excluding turn-like pair-waves", ///
        pattern(1 0 0 0 1 0 0 0) span) ///
    mtitles("(1)" "(2)" "(3)" "(4)" "(1)" "(2)" "(3)" "(4)") ///
    star(+ 0.1 * 0.05 ** 0.01) label fragment booktabs

* Compare estimates.

tempname coefficients
postfile `coefficients' byte specification str10 sample double coefficient se p N r2 using ///
    "`result_dir'/taking_turns_all_coefficients`suffix'.dta", replace
foreach sample in full drop {
    forvalues s = 1/4 {
        estimates restore `sample'`s'
        post `coefficients' (`s') ("`sample'") (_b[`xvar']) ///
            (_se[`xvar']) (2*ttail(e(df_r),abs(_b[`xvar']/_se[`xvar']))) (e(N)) (e(r2))
    }
}
postclose `coefficients'

capture program drop tt_difference
program define tt_difference, rclass
    tempname d1 d2 d3 d4
    capture confirm variable boot_class
    if _rc {
        clonevar boot_class = class
    }
    capture drop boot_id_fe
    egen long boot_id_fe = group(boot_class id)

    quietly reghdfe Ihat_ig $TT_XVAR if balanced_t3, absorb(boot_class)
    tempname full1
    scalar `full1' = _b[$TT_XVAR]
    quietly reghdfe Ihat_ig $TT_XVAR if balanced_t3 & turn_like != 1, ///
        absorb(boot_class)
    scalar `d1' = _b[$TT_XVAR] - `full1'

    quietly reghdfe Ihat_ig $TT_XVAR $tt_group $tt_friend $tt_missing ///
        if balanced_t3, absorb(boot_class)
    tempname full2
    scalar `full2' = _b[$TT_XVAR]
    quietly reghdfe Ihat_ig $TT_XVAR $tt_group $tt_friend $tt_missing ///
        if balanced_t3 & turn_like != 1, absorb(boot_class)
    scalar `d2' = _b[$TT_XVAR] - `full2'

    quietly reghdfe Ihat_ig $TT_XVAR $tt_group $tt_friend $tt_missing ///
        $tt_ra $tt_share if balanced_t3, absorb(boot_class)
    tempname full3
    scalar `full3' = _b[$TT_XVAR]
    quietly reghdfe Ihat_ig $TT_XVAR $tt_group $tt_friend $tt_missing ///
        $tt_ra $tt_share if balanced_t3 & turn_like != 1, ///
        absorb(boot_class)
    scalar `d3' = _b[$TT_XVAR] - `full3'

    quietly reghdfe Ihat_ig $TT_XVAR $tt_group_nogender $tt_friend ///
        $tt_missing $tt_ra $tt_share if balanced_t3, absorb(boot_id_fe)
    tempname full4
    scalar `full4' = _b[$TT_XVAR]
    quietly reghdfe Ihat_ig $TT_XVAR $tt_group_nogender $tt_friend ///
        $tt_missing $tt_ra $tt_share if balanced_t3 & turn_like != 1, ///
        absorb(boot_id_fe)
    scalar `d4' = _b[$TT_XVAR] - `full4'
    ereturn clear
    forvalues s = 1/4 {
        return scalar difference`s' = `d`s''
    }
end

set seed 20001
ereturn clear
bootstrap difference1=r(difference1) difference2=r(difference2) ///
    difference3=r(difference3) difference4=r(difference4), ///
    reps(`bootstrap_reps') cluster(class) ///
    idcluster(boot_class) saving("`result_dir'/coefficient_difference_bootstrap`suffix'.dta", replace) ///
    nodots: tt_difference

tempname post_results
postfile `post_results' byte specification double full_coefficient ///
    exclusion_coefficient coefficient_change difference_se difference_p using ///
    "`result_dir'/taking_turns_regression_results`suffix'.dta", replace
forvalues s = 1/4 {
    scalar change`s' = b_drop`s' - b_full`s'
    scalar se`s' = _se[difference`s']
    scalar p`s' = 2 * normal(-abs(change`s' / se`s'))
    post `post_results' (`s') (b_full`s') (b_drop`s') ///
        (change`s') (se`s') (p`s')
}
postclose `post_results'

use "`result_dir'/taking_turns_regression_results`suffix'.dta", clear
export delimited using ///
    "`result_dir'/taking_turns_regression_results`suffix'.csv", replace

list, noobs abbreviate(24)
use "`result_dir'/taking_turns_all_coefficients`suffix'.dta", clear
export delimited using "`result_dir'/taking_turns_all_coefficients`suffix'.csv", replace
log close ttlog
macro drop TT_XVAR
}
