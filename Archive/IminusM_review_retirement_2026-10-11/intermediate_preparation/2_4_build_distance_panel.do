********************************************************************************
* Prepare the shared distance-analysis panel for manuscript scripts 06-09.
* Run from Code after 2_3_build_panel_individual.R. Reuses the retained full
* CCEI benchmark; no donor calculations or regressions run here.
* Output: data/distance_analysis.dta (all 2,608 student-waves).
********************************************************************************
clear all
set more off
capture mkdir "Logs"
capture log close distance_panel
log using "Logs/02_distance_panel.log", name(distance_panel) text replace

do "programs/prepare_table3_controls.do"
tempfile ccei_benchmark
preserve
    use "results/placebo_normalized/placebo_normalized_member_wave.dta", clear
    tostring group_id, replace format(%14.0f)
    tostring id, replace format(%7.0f)
    keep group_id post id I_actual M_all_imp n_all
    rename I_actual I_ccei_cached
    rename M_all_imp M_ccei
    rename n_all n_ccei_donors
    isid group_id post id
    save `ccei_benchmark'
restore
capture drop M_ccei n_ccei_donors
merge 1:1 group_id post id using `ccei_benchmark', assert(match) nogen

gen double I_ccei = Ihat_ig
gen double Istar_ccei = I_ccei - M_ccei
assert missing(I_ccei_cached) == missing(I_ccei)
assert abs(I_ccei_cached - I_ccei) < 1e-4 if !missing(I_ccei)
assert n_ccei_donors == 651 & !missing(M_ccei)
assert _N == 2608
count if !missing(I_ccei)
assert r(N) == 2560
bysort group_id post: assert _N == 2
by group_id post: assert abs(M_ccei[1] + M_ccei[2] - 1) < 1e-7
by group_id post: assert abs(Istar_ccei[1] + Istar_ccei[2]) < 1e-7 ///
    if !missing(Istar_ccei[1], Istar_ccei[2])

* Preserve the retained panel's order for identical numerical accumulation.
sort partner_id post
isid id post
save "data/distance_analysis.dta", replace
di as result "SUCCESS: prepared distance panel; 2608 rows, 2560 defined distances, 651 donors."
log close distance_panel
