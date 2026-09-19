clear all
set more off

local review_dir "IminusM_review"
local out_dir "`review_dir'/outputs"
capture log close benchmark_sensitivity
log using "`out_dir'/logs/benchmark_sensitivity.log", name(benchmark_sensitivity) text replace

use "`out_dir'/data/ccei_ra_candidate_analysis.dta", clear
tempfile ccei_pool_counts
preserve
    use "results/placebo_normalized/placebo_normalized_member_wave.dta", clear
    tostring group_id, replace format(%14.0f)
    tostring id, replace format(%7.0f)
    keep group_id post id n_cls
    rename n_cls n_ccei_sameclass_donors
    save `ccei_pool_counts'
restore
merge 1:1 group_id post id using `ccei_pool_counts', assert(match) nogen

gen double M_ccei_outclass = ///
    (n_ccei_donors * M_ccei - n_ccei_sameclass_donors * M_ccei_sameclass) / ///
    (n_ccei_donors - n_ccei_sameclass_donors)
gen double Istar_ccei_outclass = I_ccei - M_ccei_outclass

bysort group_id post: assert abs(M_ccei_outclass[1] + M_ccei_outclass[2] - 1) < 1e-7
bysort group_id post: assert abs(Istar_ccei_outclass[1] + Istar_ccei_outclass[2]) < 1e-7 ///
    if !missing(Istar_ccei_outclass[1], Istar_ccei_outclass[2])

local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local gender "female_i_male_j male_i_female_j"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"

tempfile results
postfile result_file str8 measure str18 benchmark str20 focal byte specification ///
    double beta se p outcome_sd standardized long N double r2 using `results', replace

foreach measure in ccei ra {
    local variants "all_imp all_drop outclass_imp outclass_drop"
    if "`measure'" == "ccei" local variants "all_imp all_drop sameclass outclass_imp"
    local sample "sample_`measure'"
    foreach variant of local variants {
        local outcome "Istar_`measure'"
        if "`measure'" == "ccei" {
            if "`variant'" == "all_drop" local outcome "Istar_ccei_drop"
            if "`variant'" == "sameclass" local outcome "Istar_ccei_sameclass"
            if "`variant'" == "outclass_imp" local outcome "Istar_ccei_outclass"
        }
        else {
            if "`variant'" == "all_drop" local outcome "Istar_ra_drop"
            if "`variant'" == "outclass_imp" local outcome "Istar_ra_outclass"
            if "`variant'" == "outclass_drop" local outcome "Istar_ra_outclass_drop"
        }
        foreach focal in HighCCEI_both_high ccei_gap_ij {
            forvalues specification = 1/3 {
                local controls ""
                local fixed_effect "class"
                local singleton_option ""
                if `specification' == 2 local controls "`individual' `gender' `friendship' `missing_controls' `shares'"
                if `specification' == 3 {
                    local controls "`individual' `friendship' `missing_controls' `shares'"
                    local fixed_effect "id_fe"
                    if "`measure'" == "ra" local singleton_option "keepsingletons"
                }
                quietly reghdfe `outcome' `focal' `controls' if `sample', ///
                    absorb(`fixed_effect') vce(cluster class) `singleton_option'
                quietly summarize `outcome' if e(sample)
                local ysd = r(sd)
                quietly summarize `focal' if e(sample)
                post result_file ("`measure'") ("`variant'") ("`focal'") (`specification') ///
                    (_b[`focal']) (_se[`focal']) ///
                    (2 * ttail(e(df_r), abs(_b[`focal'] / _se[`focal']))) (`ysd') ///
                    (_b[`focal'] * r(sd) / `ysd') (e(N)) (e(r2))
            }
        }
    }
}
postclose result_file
use `results', clear
export delimited using "`out_dir'/tables/benchmark_sensitivity.csv", replace
log close benchmark_sensitivity

