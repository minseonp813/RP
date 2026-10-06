clear all
set more off
set matsize 8000
set maxvar 10000

local review_dir "IminusM_review"
local out_dir "`review_dir'/outputs"
foreach folder in "`out_dir'" "`out_dir'/data" "`out_dir'/tables" "`out_dir'/figures" "`out_dir'/logs" {
    capture mkdir "`folder'"
}
capture log close ccei_ra_review
log using "`out_dir'/logs/ccei_ra_analysis.log", name(ccei_ra_review) text replace

* Use the current Table 3 sample and controls without changing manuscript files.
do "programs/prepare_table3_controls.do"

tempfile ccei_benchmark ra_benchmark
preserve
    use "results/placebo_normalized/placebo_normalized_member_wave.dta", clear
    tostring group_id, replace format(%14.0f)
    tostring id, replace format(%7.0f)
    keep group_id post id I_actual M_all_imp M_all_drop Istar_all_drop M_cls_imp Istar_cls_imp n_all nvalid_all degfrac_all
    rename I_actual I_ccei_cached
    rename M_all_imp M_ccei
    rename M_all_drop M_ccei_drop
    rename Istar_all_drop Istar_ccei_drop
    rename M_cls_imp M_ccei_sameclass
    rename Istar_cls_imp Istar_ccei_sameclass
    rename n_all n_ccei_donors
    rename nvalid_all nvalid_ccei_donors
    rename degfrac_all degfrac_ccei
    save `ccei_benchmark'
restore
merge 1:1 group_id post id using `ccei_benchmark', assert(match) nogen

preserve
    use "`out_dir'/data/ra_placebo_member_wave.dta", clear
    tostring group_id, replace format(%14.0f)
    tostring id, replace format(%7.0f)
    keep group_id post id I_actual M_all_imp M_all_drop M_outclass_imp M_outclass_drop ///
        Istar_all_drop Istar_outclass_imp Istar_outclass_drop n_all nvalid_all degfrac_all
    rename I_actual I_ra_cached
    rename M_all_imp M_ra
    rename M_all_drop M_ra_drop
    rename M_outclass_imp M_ra_outclass
    rename M_outclass_drop M_ra_outclass_drop
    rename Istar_all_drop Istar_ra_drop
    rename Istar_outclass_imp Istar_ra_outclass
    rename Istar_outclass_drop Istar_ra_outclass_drop
    rename n_all n_ra_donors
    rename nvalid_all nvalid_ra_donors
    rename degfrac_all degfrac_ra
    save `ra_benchmark'
restore
merge 1:1 group_id post id using `ra_benchmark', assert(match) nogen

gen double I_ccei = Ihat_ig
gen double Istar_ccei = I_ccei - M_ccei
assert missing(I_ccei_cached) == missing(I_ccei)
assert abs(I_ccei_cached - I_ccei) < 1e-4 if !missing(I_ccei)
assert n_ccei_donors == 651

gen double RA_distance_denom = (RA_i - RA_g)^2 + (RA_j - RA_g)^2
gen double I_ra = (RA_i - RA_g)^2 / RA_distance_denom if RA_distance_denom > 0
gen double Istar_ra = I_ra - M_ra
assert missing(I_ra_cached) == missing(I_ra)
assert abs(I_ra_cached - I_ra) < 1e-12 if !missing(I_ra)
assert n_ra_donors == 651

bysort group_id post: assert abs(M_ccei[1] + M_ccei[2] - 1) < 1e-7
bysort group_id post: assert abs(Istar_ccei[1] + Istar_ccei[2]) < 1e-7 if !missing(Istar_ccei[1], Istar_ccei[2])
bysort group_id post: assert abs(M_ra[1] + M_ra[2] - 1) < 1e-12
bysort group_id post: assert abs(Istar_ra[1] + Istar_ra[2]) < 1e-12 if !missing(Istar_ra[1], Istar_ra[2])

gen byte sample_ccei = balanced_t3
gen byte sample_ra = !missing(I_ra)
count if sample_ccei
assert r(N) == 2512
count if sample_ra
assert r(N) == 2604

local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local gender "female_i_male_j male_i_female_j"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"

label var HighCCEI_both_high "Higher CCEI"
label var ccei_gap_ij "CCEI difference"

* Main review comparison: raw I, I-M, and I controlling flexibly for M.
tempfile regression_results
postfile regression_file str8 measure str12 outcome_model str20 focal byte specification ///
    double beta se p beta_M se_M p_M p_M_equals_one outcome_sd focal_sd standardized ///
    long N clusters double r2 using `regression_results', replace

foreach measure in ccei ra {
    local actual "I_`measure'"
    local normalized "Istar_`measure'"
    local benchmark "M_`measure'"
    local sample "sample_`measure'"
    local expected_N = cond("`measure'" == "ccei", 2512, 2604)

    foreach outcome_model in raw normalized adjusted {
        local outcome "`actual'"
        if "`outcome_model'" == "normalized" local outcome "`normalized'"

        foreach definition in high gap {
            local focal "HighCCEI_both_high"
            if "`definition'" == "gap" local focal "ccei_gap_ij"

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
                if "`outcome_model'" == "adjusted" local controls "`controls' `benchmark'"

                quietly reghdfe `outcome' `focal' `controls' if `sample', ///
                    absorb(`fixed_effect') vce(cluster class) `singleton_option'
                assert e(N) == `expected_N' & e(N_clust) == 64
                local b = _b[`focal']
                local s = _se[`focal']
                local pv = 2 * ttail(e(df_r), abs(`b' / `s'))
                local bM = .
                local sM = .
                local pM = .
                local pM1 = .
                if "`outcome_model'" == "adjusted" {
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
                local std = `b' * `xsd' / `ysd'
                post regression_file ("`measure'") ("`outcome_model'") ("`focal'") (`specification') ///
                    (`b') (`s') (`pv') (`bM') (`sM') (`pM') (`pM1') (`ysd') (`xsd') (`std') ///
                    (e(N)) (e(N_clust)) (e(r2))
            }
        }
    }
}
postclose regression_file
preserve
    use `regression_results', clear
    export delimited using "`out_dir'/tables/main_comparison.csv", replace
restore

* Choice-buffer specifications from the appendix, using I-M throughout.
tempfile base_buffers all_buffers partner_buffers buffer_results
preserve
    foreach wave in base end {
        use "data/`wave'_raw.dta", clear
        keep if game_type == 1
        gen byte post = ("`wave'" == "end")
        gen byte corner_0 = (coord_x == 0 | coord_y == 0)
        gen byte mid_0 = (coord_x == coord_y)
        gen byte corner_025 = 40 * min(coord_x, coord_y) <= coord_x + coord_y
        gen byte mid_025 = 20 * abs(coord_x - coord_y) <= coord_x + coord_y
        gen byte corner_05 = 20 * min(coord_x, coord_y) <= coord_x + coord_y
        gen byte mid_05 = 10 * abs(coord_x - coord_y) <= coord_x + coord_y
        collapse (mean) corner_* mid_*, by(id post)
        if "`wave'" == "base" save `base_buffers'
    }
    append using `base_buffers'
    save `all_buffers'
    rename id partner_id
    save `partner_buffers'
restore
merge 1:1 id post using `all_buffers', assert(match using) keep(match) nogen
foreach suffix in 0 025 05 {
    rename corner_`suffix' corner_`suffix'_i
    rename mid_`suffix' mid_`suffix'_i
}
merge m:1 partner_id post using `partner_buffers', assert(match using) keep(match) nogen
foreach suffix in 0 025 05 {
    rename corner_`suffix' corner_`suffix'_j
    rename mid_`suffix' mid_`suffix'_j
}

postfile buffer_file str8 buffer str20 focal byte specification double beta se p long N double r2 ///
    using `buffer_results', replace
foreach buffer in exact pp2_5 pp5 {
    local suffix "0"
    if "`buffer'" == "pp2_5" local suffix "025"
    if "`buffer'" == "pp5" local suffix "05"
    capture drop candidate_corner_i candidate_corner_diff candidate_mid_i candidate_mid_diff
    gen double candidate_corner_i = corner_`suffix'_i
    gen double candidate_corner_diff = corner_`suffix'_i - corner_`suffix'_j
    gen double candidate_mid_i = mid_`suffix'_i
    gen double candidate_mid_diff = mid_`suffix'_i - mid_`suffix'_j
    local buffer_shares "candidate_corner_i candidate_corner_diff candidate_mid_i candidate_mid_diff"
    foreach focal in HighCCEI_both_high ccei_gap_ij {
        forvalues specification = 2/3 {
            local controls "`individual' `gender' `friendship' `missing_controls' `buffer_shares'"
            local fixed_effect "class"
            if `specification' == 3 {
                local controls "`individual' `friendship' `missing_controls' `buffer_shares'"
                local fixed_effect "id_fe"
            }
            quietly reghdfe Istar_ccei `focal' `controls' if sample_ccei, ///
                absorb(`fixed_effect') vce(cluster class)
            post buffer_file ("`buffer'") ("`focal'") (`specification') ///
                (_b[`focal']) (_se[`focal']) ///
                (2 * ttail(e(df_r), abs(_b[`focal'] / _se[`focal']))) (e(N)) (e(r2))
        }
    }
}
postclose buffer_file
preserve
    use `buffer_results', clear
    export delimited using "`out_dir'/tables/buffer_results.csv", replace
restore

* Mover analysis, preserving Table 3's six specifications.
gen double higher_mover = HighCCEI_both_high * mover
gen double gap_mover = ccei_gap_ij * mover
tempfile mover_results
postfile mover_file str20 focal str20 interaction byte specification ///
    double beta se p beta_mover se_mover p_mover beta_interaction se_interaction p_interaction ///
    long N double r2 using `mover_results', replace
foreach definition in high gap {
    local focal "HighCCEI_both_high"
    local interaction "higher_mover"
    if "`definition'" == "gap" {
        local focal "ccei_gap_ij"
        local interaction "gap_mover"
    }
    forvalues specification = 1/3 {
        local controls ""
        local fixed_effect "class"
        if `specification' == 2 local controls "`individual' `gender' `friendship' `missing_controls' `shares'"
        if `specification' == 3 {
            local controls "`individual' `friendship' `missing_controls' `shares'"
            local fixed_effect "id_fe"
        }
        quietly reghdfe Istar_ccei `focal' mover `interaction' `controls' if sample_ccei, ///
            absorb(`fixed_effect') vce(cluster class)
        local bm = _b[mover]
        local sm = _se[mover]
        local pm = cond(`sm' > 0, 2 * ttail(e(df_r), abs(`bm' / `sm')), .)
        local bi = _b[`interaction']
        local si = _se[`interaction']
        local pi = cond(`si' > 0, 2 * ttail(e(df_r), abs(`bi' / `si')), .)
        post mover_file ("`focal'") ("`interaction'") (`specification') ///
            (_b[`focal']) (_se[`focal']) (2 * ttail(e(df_r), abs(_b[`focal'] / _se[`focal']))) ///
            (`bm') (`sm') (`pm') (`bi') (`si') (`pi') (e(N)) (e(r2))
    }
}
postclose mover_file
preserve
    use `mover_results', clear
    export delimited using "`out_dir'/tables/mover_results.csv", replace
restore

* Candidate analysis file for figures, correlations, and later checks.
save "`out_dir'/data/ccei_ra_candidate_analysis.dta", replace
export delimited using "`out_dir'/data/ccei_ra_candidate_analysis.csv", replace

* Shorrocks-Shapley decomposition of the current Column (3), which excludes RA.
* The adjusted model gives M its own block; the normalized model does not.
keep if sample_ccei
quietly tabulate id_fe, generate(FE_id_)
unab id_dummies : FE_id_*
local id_base : word 1 of `id_dummies'
local individual_fe : list id_dummies - id_base
local characteristics "`individual' `friendship' `missing_controls'"
tempfile shapley_results
postfile shapley_file str12 outcome_model str28 block double shapley_value shapley_percent total_r2 ///
    using `shapley_results', replace

foreach outcome_model in normalized adjusted {
    local outcome "Istar_ccei"
    local benchmark_block ""
    local groups "HighCCEI_both_high, `characteristics', `shares', `individual_fe'"
    local nblocks = 4
    if "`outcome_model'" == "adjusted" {
        local outcome "I_ccei"
        local benchmark_block "M_ccei"
        local groups "HighCCEI_both_high, M_ccei, `characteristics', `shares', `individual_fe'"
        local nblocks = 5
    }
    quietly reg `outcome' HighCCEI_both_high `benchmark_block' `characteristics' `shares' `individual_fe', vce(cluster class)
    local total_r2 = e(r2)
    quietly shapley2, stat(r2) group("`groups'")
    matrix Sh = e(shapley)
    matrix Sr = e(shapley_rel)
    if rowsof(Sh) == 1 matrix Sh = Sh'
    if rowsof(Sr) == 1 matrix Sr = Sr'
    forvalues block_number = 1/`nblocks' {
        local block "Individual FE"
        if `block_number' == 1 local block "Higher CCEI"
        if "`outcome_model'" == "normalized" {
            if `block_number' == 2 local block "Individual/Friendship"
            if `block_number' == 3 local block "Corner/Midpoint shares"
        }
        else {
            if `block_number' == 2 local block "M benchmark"
            if `block_number' == 3 local block "Individual/Friendship"
            if `block_number' == 4 local block "Corner/Midpoint shares"
        }
        post shapley_file ("`outcome_model'") ("`block'") ///
            (Sh[`block_number',1]) (100 * Sr[`block_number',1]) (`total_r2')
    }
}
postclose shapley_file
use `shapley_results', clear
export delimited using "`out_dir'/tables/shapley_results.csv", replace

log close ccei_ra_review
