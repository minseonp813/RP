clear all
set more off

local review_dir "IminusM_review"
local out_dir "`review_dir'/outputs"
capture log close hm_stability
log using "`out_dir'/logs/hm_sample_stability.log", name(hm_stability) text replace

local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local gender "female_i_male_j male_i_female_j"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"

tempfile results
postfile result_file str8 measure str12 benchmark str24 focal byte specification double beta se p long N double r2 ///
    using `results', replace
local measures "hm"
capture confirm file "`out_dir'/data/maxmpi_sample_stability.dta"
if !_rc local measures "hm maxmpi"
foreach measure of local measures {
    do "programs/prepare_table3_controls.do"
    tempfile stability
    preserve
        use "`out_dir'/data/`measure'_sample_stability.dta", clear
        tostring group_id, replace format(%14.0f)
        tostring id, replace format(%7.0f)
        save `stability'
    restore
    merge 1:1 group_id post id using `stability', assert(match) nogen
    local actual "Ihat_hm_ig"
    local high "HighHM_both_high"
    local gap "hm_gap_ij"
    if "`measure'" == "maxmpi" {
        local actual "Ihat_maxmpi_ig"
        local high "HighMaxMPI_both_high"
        local gap "maxmpi_gap_ij"
    }
    assert missing(I_actual) == missing(`actual')
    assert abs(I_actual - `actual') < 2e-6 if !missing(`actual')
    bysort id: egen n_measure = total(!missing(`actual'))
    gen byte sample_measure = (n_measure == 2)
    count if sample_measure
    assert r(N) == 2512
    foreach benchmark in half1 half2 fullsample {
        foreach definition in high gap {
            local focal "``definition''"
            forvalues specification = 1/3 {
                local controls ""
                local fixed_effect "class"
                if `specification' == 2 local controls "`individual' `gender' `friendship' `missing_controls' `shares'"
                if `specification' == 3 {
                    local controls "`individual' `friendship' `missing_controls' `shares'"
                    local fixed_effect "id_fe"
                }
                quietly reghdfe Istar_`benchmark' `focal' `controls' if sample_measure, ///
                    absorb(`fixed_effect') vce(cluster class)
                post result_file ("`measure'") ("`benchmark'") ("`focal'") (`specification') ///
                    (_b[`focal']) (_se[`focal']) ///
                    (2 * ttail(e(df_r), abs(_b[`focal'] / _se[`focal']))) (e(N)) (e(r2))
            }
        }
    }
}
postclose result_file
use `results', clear
export delimited using "`out_dir'/tables/alternative_sample_stability.csv", replace
log close hm_stability
