********************************************************************************
* Figure A7: across- and within-individual explained variation.
* Run independently from Code. Uses Table 3 Column (3), including M and no RA.
* Individual effects enter first: overall R2 = across R2 + added within fit.
* Four-block Shapley values then allocate the conditional within-individual R2.
* Output: results/tables/shapley_bargaining_index_M.csv; plotting is in 09.
********************************************************************************
clear
set more off
local code_dir `"`c(pwd)'"'
cap mkdir "Logs"
capture log close distance_shapley
log using "Logs/08_shapley_distance.log", name(distance_shapley) text replace
do "programs/prepare_table3_controls.do"
capture drop M_ccei n_ccei_donors
merge 1:1 id post using "IminusM_review/outputs/data/ccei_ra_candidate_analysis.dta", ///
    keepusing(M_ccei n_ccei_donors) assert(match) nogen
assert n_ccei_donors == 651 & !missing(M_ccei)
keep if balanced_t3
local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"
local characteristics "`individual' `friendship' `missing_controls'"
quietly reghdfe Ihat_ig HighCCEI_both_high M_ccei `characteristics' `shares', ///
    absorb(id_fe) vce(cluster class)
assert e(N) == 2512 & e(N_clust) == 64
scalar A7_total = e(r2)
scalar A7_conditional = e(r2_within)
scalar A7_high_beta = _b[HighCCEI_both_high]

* Demeaning makes every coalition conditional on the same individual effects.
foreach variable in Ihat_ig HighCCEI_both_high M_ccei `characteristics' `shares' {
    assert !missing(`variable')
    bysort id_fe: egen double individual_mean = mean(`variable')
    gen double w_`variable' = `variable' - individual_mean
    drop individual_mean
}
quietly summarize Ihat_ig
scalar A7_total_variance = r(Var)
quietly summarize w_Ihat_ig
scalar A7_across = 1 - r(Var) / A7_total_variance
scalar A7_within_added = A7_total - A7_across
assert abs(A7_within_added - (1 - A7_across) * A7_conditional) < 1e-10
local block1 "w_HighCCEI_both_high"
local block2 "w_M_ccei"
local block3 ""
foreach variable in `characteristics' {
    local block3 "`block3' w_`variable'"
}
local block4 ""
foreach variable in `shares' {
    local block4 "`block4' w_`variable'"
}

* Exact four-block Shapley decomposition: all 16 covariate coalitions.
tempname coalition_r2
matrix `coalition_r2' = J(16, 1, 0)
forvalues mask = 1/15 {
    local rhs ""
    forvalues block = 1/4 {
        if mod(floor(`mask' / 2^(`block' - 1)), 2) {
            local rhs "`rhs' `block`block''"
        }
    }
    quietly regress w_Ihat_ig `rhs'
    assert e(N) == 2512
    matrix `coalition_r2'[`mask' + 1, 1] = e(r2)
}
assert abs(`coalition_r2'[16, 1] - A7_conditional) < 1e-10
assert abs(_b[w_HighCCEI_both_high] - A7_high_beta) < 1e-10
tempfile decomposition
tempname output
postfile `output' str8 panel str40 block double shapley_value double shapley_percent ///
    double total_r2 double within_r2 int N using `decomposition', replace
post `output' ("overall") ("Across individuals") ///
    (A7_across) (100 * A7_across / A7_total) (A7_total) (A7_conditional) (2512)
post `output' ("overall") ("Within individuals") ///
    (A7_within_added) (100 * A7_within_added / A7_total) (A7_total) (A7_conditional) (2512)
local label1 "Higher CCEI"
local label2 "M benchmark"
local label3 "Individual/Friendship"
local label4 "Corner/Midpoint shares"
scalar A7_sum = 0
forvalues block = 1/4 {
    scalar A7_contribution = 0
    forvalues mask = 0/15 {
        if !mod(floor(`mask' / 2^(`block' - 1)), 2) {
            local size = 0
            forvalues other = 1/4 {
                local size = `size' + mod(floor(`mask' / 2^(`other' - 1)), 2)
            }
            * |S|!(3-|S|)!/4!: 1/4 for sizes 0/3, and 1/12 for sizes 1/2.
            local weight = cond(inlist(`size', 0, 3), 1/4, 1/12)
            scalar A7_contribution = A7_contribution + `weight' * ///
                (`coalition_r2'[`mask' + 1 + 2^(`block' - 1), 1] - `coalition_r2'[`mask' + 1, 1])
        }
    }
    scalar A7_sum = A7_sum + A7_contribution
    post `output' ("within") ("`label`block''") ///
        (A7_contribution) (100 * A7_contribution / A7_conditional) ///
        (A7_total) (A7_conditional) (2512)
}
assert abs(A7_sum - A7_conditional) < 1e-10
postclose `output'
use `decomposition', clear
isid panel block
bysort panel: egen double share_sum = total(shapley_percent)
assert abs(share_sum - 100) < 1e-8
drop share_sum
cap mkdir "results/tables"
export delimited using "results/tables/shapley_bargaining_index_M.csv", replace
list panel block shapley_value shapley_percent, noobs abbreviate(25)
di as result "SUCCESS: Figure A7 inputs match Table 3 Column (3); N=2512, clusters=64."
log close distance_shapley

