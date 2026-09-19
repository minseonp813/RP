clear all
set more off
set matsize 8000
set maxvar 10000

local review_dir "IminusM_review"
local out_dir "`review_dir'/outputs"
capture log close alternative_review
log using "`out_dir'/logs/alternative_measure_analysis.log", name(alternative_review) text replace

do "programs/prepare_table3_controls.do"

tempfile hm_benchmark maxmpi_benchmark
local measures "hm"
capture confirm file "`out_dir'/benchmarks/maxmpi_sample20/placebo_normalized_member_wave.dta"
if !_rc local measures "hm maxmpi"
foreach measure of local measures {
    local folder "hm_sample50"
    if "`measure'" == "maxmpi" local folder "maxmpi_sample20"
    preserve
        use "`out_dir'/benchmarks/`folder'/placebo_normalized_member_wave.dta", clear
        tostring group_id, replace format(%14.0f)
        tostring id, replace format(%7.0f)
        keep group_id post id I_actual M_all_imp M_all_drop Istar_all_drop n_all nvalid_all degfrac_all
        rename I_actual I_`measure'_cached
        rename M_all_imp M_`measure'
        rename M_all_drop M_`measure'_drop
        rename Istar_all_drop Istar_`measure'_drop
        rename n_all n_`measure'_donors
        rename nvalid_all nvalid_`measure'_donors
        rename degfrac_all degfrac_`measure'
        save ``measure'_benchmark'
    restore
    merge 1:1 group_id post id using ``measure'_benchmark', assert(match) nogen
}

gen double I_hm = Ihat_hm_ig
gen double Istar_hm = I_hm - M_hm
assert missing(I_hm_cached) == missing(I_hm)
assert abs(I_hm_cached - I_hm) < 2e-6 if !missing(I_hm)
assert n_hm_donors == 50
if strpos(" `measures' ", " maxmpi ") {
    gen double I_maxmpi = Ihat_maxmpi_ig
    gen double Istar_maxmpi = I_maxmpi - M_maxmpi
    assert missing(I_maxmpi_cached) == missing(I_maxmpi)
    assert abs(I_maxmpi_cached - I_maxmpi) < 2e-6 if !missing(I_maxmpi)
    assert n_maxmpi_donors == 20
}

bysort group_id post: assert abs(M_hm[1] + M_hm[2] - 1) < 1e-7
bysort group_id post: assert abs(Istar_hm[1] + Istar_hm[2]) < 2e-6 if !missing(Istar_hm[1], Istar_hm[2])
if strpos(" `measures' ", " maxmpi ") {
    bysort group_id post: assert abs(M_maxmpi[1] + M_maxmpi[2] - 1) < 1e-7
    bysort group_id post: assert abs(Istar_maxmpi[1] + Istar_maxmpi[2]) < 2e-6 if !missing(Istar_maxmpi[1], Istar_maxmpi[2])
}

foreach measure of local measures {
    bysort id: egen n_`measure'_actual = total(!missing(I_`measure'))
    gen byte sample_`measure' = (n_`measure'_actual == 2)
    count if sample_`measure'
    assert r(N) == 2512
}

local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local gender "female_i_male_j male_i_female_j"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"

tempfile results
postfile result_file str8 measure str18 outcome_model str24 focal byte specification ///
    double beta se p beta_M se_M p_M p_M_equals_one outcome_sd focal_sd standardized ///
    long N donor_count clusters double r2 using `results', replace

foreach measure of local measures {
    local high "HighHM_both_high"
    local gap "hm_gap_ij"
    if "`measure'" == "maxmpi" {
        local high "HighMaxMPI_both_high"
        local gap "maxmpi_gap_ij"
    }
    foreach outcome_model in raw normalized adjusted normalized_drop adjusted_drop {
        local outcome "I_`measure'"
        if "`outcome_model'" == "normalized" local outcome "Istar_`measure'"
        if "`outcome_model'" == "normalized_drop" local outcome "Istar_`measure'_drop"
        local benchmark "M_`measure'"
        if "`outcome_model'" == "adjusted_drop" local benchmark "M_`measure'_drop"
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
                if inlist("`outcome_model'", "adjusted", "adjusted_drop") local controls "`controls' `benchmark'"
                quietly reghdfe `outcome' `focal' `controls' if sample_`measure', ///
                    absorb(`fixed_effect') vce(cluster class)
                assert e(N) == 2512 & e(N_clust) == 64
                local b = _b[`focal']
                local s = _se[`focal']
                local pv = 2 * ttail(e(df_r), abs(`b' / `s'))
                local bM = .
                local sM = .
                local pM = .
                local pM1 = .
                if inlist("`outcome_model'", "adjusted", "adjusted_drop") {
                    local bM = _b[`benchmark']
                    local sM = _se[`benchmark']
                    local pM = 2 * ttail(e(df_r), abs(`bM' / `sM'))
                    quietly test `benchmark' = 1
                    local pM1 = r(p)
                }
                quietly summarize `outcome' if e(sample)
                local ysd = r(sd)
                quietly summarize `focal' if e(sample)
                local xsd = r(sd)
                local donors = cond("`measure'" == "hm", 50, 20)
                post result_file ("`measure'") ("`outcome_model'") ("`focal'") (`specification') ///
                    (`b') (`s') (`pv') (`bM') (`sM') (`pM') (`pM1') (`ysd') (`xsd') ///
                    (`b' * `xsd' / `ysd') (e(N)) (`donors') (e(N_clust)) (e(r2))
            }
        }
    }
}
postclose result_file
use `results', clear
export delimited using "`out_dir'/tables/alternative_measure_comparison.csv", replace
log close alternative_review
