********************************************************************************
* 06_Tables_Main.do: Main-paper tables and supplementary CEI/CCEI specifications
* Last updated: 2026-10-10
* Original analysis authors: Byunghun Hahn and Minseon Park
*   Attribution follows the archived table and regression script headers.
*
* Run from Code. All paths below are relative to that directory.
* Locked draft order: Table 1 (summary), Table 2 (balance), Table 3 (distance).
*
* Inputs (data/):
*   panel_individual.dta, panel_group.dta, panel_final.dta
*   panel_group_new_indices.dta (CEIV; built by 11_collective_quality.do)
*   base_raw.dta, end_raw.dta, riskpreference_pre.dta
*   male.dta, height.dta, network_survey.dta
*   Cognitive_raw_pre_full.xlsx, NonCognitive_raw_pre.xlsx
* Tables 1 and 3 use M_ccei carried into panel_individual.dta by 1_1 and 2_3.
*
* Called code:
*   programs/prepare_collective_sample.do
*
* Outputs (results/tables/; existing files are replaced):
*   table_ccei_summary_IminusM.tex (Table 1 uses Ihat_ig - M_ccei)
*   table_attrition_balance.tex, table_bargainingCCEI_M.tex (current Table 3)
*   final_collective_ccei.tex, final_collective_ccei_high_low.tex
*   {ccei_g,cei_g,type_a,type_b,type_c}_on_individual_ccei.tex
*   cei_four_outcomes_combined.tex
*   cei_ccei_models_{min,dist}.tex, cei_ccei_ols_min.tex
*   cei_joint_mlogit_{min,dist}.tex
*
* Manuscript copy:
*   ../Overleaf/tables_2025/table_ccei_summary_IminusM.tex
*   ../Overleaf/tables_2025/table_attrition_balance.tex
*   ../Overleaf/tables_2025/table_bargainingCCEI_M.tex
* Table 3 log: Logs/06_table3.log. Its section can run independently from Code.
*
* Other outputs (results/):
*   cei_classification_frequencies.csv, cei_joint_ame_pooled.csv
********************************************************************************

clear all
set more off
set matsize 8000

local distance_var "Ihat_ig"

local code_dir `"`c(pwd)'"'

local replication_dir `"`code_dir'"'
local data_dir `"`replication_dir'/data"'
local tex_dir `"`code_dir'/results/tables"'
cap mkdir `"`code_dir'/results"'
cap mkdir `"`tex_dir'"'

foreach required in panel_individual.dta panel_group.dta panel_final.dta ///
    panel_group_new_indices.dta ///
    base_raw.dta end_raw.dta riskpreference_pre.dta male.dta height.dta ///
    Cognitive_raw_pre_full.xlsx NonCognitive_raw_pre.xlsx network_survey.dta {
    capture confirm file `"`data_dir'/`required'"'
    if _rc {
        di as error "Missing required input: `data_dir'/`required'"
        exit 601
    }
}

********************************************************************************
* Table 1: Summary statistics, with placebo-adjusted preference distance I - M
********************************************************************************

use `"`data_dir'/panel_individual.dta"', clear
assert n_ccei_donors == 651
gen double table1_distance = Ihat_ig - M_ccei

foreach period in 0 1 {
    local suffix = cond(`period' == 0, "base", "end")
    foreach var in ccei_i RA_i {
        quietly summarize `var' if post == `period', detail
        local `var'_`suffix'_mean = r(mean)
        local `var'_`suffix'_sd   = r(sd)
        local `var'_`suffix'_p10  = r(p10)
        local `var'_`suffix'_p50  = r(p50)
        local `var'_`suffix'_p90  = r(p90)
        local `var'_`suffix'_N    = r(N)
    }
    quietly summarize table1_distance if post == `period', detail
    local distance_`suffix'_mean = r(mean)
    local distance_`suffix'_sd   = r(sd)
    local distance_`suffix'_p10  = r(p10)
    local distance_`suffix'_p50  = r(p50)
    local distance_`suffix'_p90  = r(p90)
    local distance_`suffix'_N    = r(N)
    foreach stat in mean sd p10 p50 p90 {
        if abs(`distance_`suffix'_`stat'') < .0005 {
            local distance_`suffix'_`stat' = 0
        }
    }
}

use `"`data_dir'/panel_group.dta"', clear
merge 1:1 group_id post using `"`data_dir'/panel_group_new_indices.dta"', ///
    keepusing(ceiv_g) assert(match) nogen
assert inrange(ceiv_g, 0, 1)

foreach period in 0 1 {
    local suffix = cond(`period' == 0, "base", "end")
    foreach var in ccei_g ceiv_g RA_g {
        quietly summarize `var' if post == `period', detail
        local `var'_`suffix'_mean = r(mean)
        local `var'_`suffix'_sd   = r(sd)
        local `var'_`suffix'_p10  = r(p10)
        local `var'_`suffix'_p50  = r(p50)
        local `var'_`suffix'_p90  = r(p90)
        local `var'_`suffix'_N    = r(N)
    }
}

capture file close summary_file
file open summary_file using `"`tex_dir'/table_ccei_summary_IminusM.tex"', write replace
local bs = char(92)
local distance_tex "`bs'ensuremath{I_{ig}-M_{ig}}"
file write summary_file "& (1) & (2) & (3) & (4) & (5) & (6) `bs'`bs'" _n
file write summary_file " & Mean & SD & p10 & p50 & p90 & N `bs'`bs'" _n
file write summary_file "`bs'midrule" _n
file write summary_file "`bs'multicolumn{7}{l}{`bs'emph{Panel A: Baseline}} `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual CCEI & " %6.3f (`ccei_i_base_mean') " & " %6.3f (`ccei_i_base_sd') " & " %6.3f (`ccei_i_base_p10') " & " %6.3f (`ccei_i_base_p50') " & " %6.3f (`ccei_i_base_p90') " & " %9.0fc (`ccei_i_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group CCEI & " %6.3f (`ccei_g_base_mean') " & " %6.3f (`ccei_g_base_sd') " & " %6.3f (`ccei_g_base_p10') " & " %6.3f (`ccei_g_base_p50') " & " %6.3f (`ccei_g_base_p90') " & " %9.0fc (`ccei_g_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group CEIV & " %6.3f (`ceiv_g_base_mean') " & " %6.3f (`ceiv_g_base_sd') " & " %6.3f (`ceiv_g_base_p10') " & " %6.3f (`ceiv_g_base_p50') " & " %6.3f (`ceiv_g_base_p90') " & " %9.0fc (`ceiv_g_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Preference distance `distance_tex' & " %6.3f (`distance_base_mean') " & " %6.3f (`distance_base_sd') " & " %6.3f (`distance_base_p10') " & " %6.3f (`distance_base_p50') " & " %6.3f (`distance_base_p90') " & " %9.0fc (`distance_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual risk aversion & " %6.3f (`RA_i_base_mean') " & " %6.3f (`RA_i_base_sd') " & " %6.3f (`RA_i_base_p10') " & " %6.3f (`RA_i_base_p50') " & " %6.3f (`RA_i_base_p90') " & " %9.0fc (`RA_i_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group risk aversion & " %6.3f (`RA_g_base_mean') " & " %6.3f (`RA_g_base_sd') " & " %6.3f (`RA_g_base_p10') " & " %6.3f (`RA_g_base_p50') " & " %6.3f (`RA_g_base_p90') " & " %9.0fc (`RA_g_base_N') " `bs'`bs'" _n
file write summary_file "`bs'addlinespace" _n
file write summary_file "`bs'multicolumn{7}{l}{`bs'emph{Panel B: Endline}} `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual CCEI & " %6.3f (`ccei_i_end_mean') " & " %6.3f (`ccei_i_end_sd') " & " %6.3f (`ccei_i_end_p10') " & " %6.3f (`ccei_i_end_p50') " & " %6.3f (`ccei_i_end_p90') " & " %9.0fc (`ccei_i_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group CCEI & " %6.3f (`ccei_g_end_mean') " & " %6.3f (`ccei_g_end_sd') " & " %6.3f (`ccei_g_end_p10') " & " %6.3f (`ccei_g_end_p50') " & " %6.3f (`ccei_g_end_p90') " & " %9.0fc (`ccei_g_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group CEIV & " %6.3f (`ceiv_g_end_mean') " & " %6.3f (`ceiv_g_end_sd') " & " %6.3f (`ceiv_g_end_p10') " & " %6.3f (`ceiv_g_end_p50') " & " %6.3f (`ceiv_g_end_p90') " & " %9.0fc (`ceiv_g_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Preference distance `distance_tex' & " %6.3f (`distance_end_mean') " & " %6.3f (`distance_end_sd') " & " %6.3f (`distance_end_p10') " & " %6.3f (`distance_end_p50') " & " %6.3f (`distance_end_p90') " & " %9.0fc (`distance_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual risk aversion & " %6.3f (`RA_i_end_mean') " & " %6.3f (`RA_i_end_sd') " & " %6.3f (`RA_i_end_p10') " & " %6.3f (`RA_i_end_p50') " & " %6.3f (`RA_i_end_p90') " & " %9.0fc (`RA_i_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group risk aversion & " %6.3f (`RA_g_end_mean') " & " %6.3f (`RA_g_end_sd') " & " %6.3f (`RA_g_end_p10') " & " %6.3f (`RA_g_end_p50') " & " %6.3f (`RA_g_end_p90') " & " %9.0fc (`RA_g_end_N') " `bs'`bs'" _n
file write summary_file "`bs'bottomrule" _n
file close summary_file
copy `"`tex_dir'/table_ccei_summary_IminusM.tex"' ///
    `"`code_dir'/../Overleaf/tables_2025/table_ccei_summary_IminusM.tex"', replace

********************************************************************************

* Table 3: actual distance I controlling for the full 651-donor benchmark M.
********************************************************************************
local code_dir `"`c(pwd)'"'
local data_dir `"`code_dir'/data"'
local tex_dir `"`code_dir'/results/tables"'
local distance_var "Ihat_ig"
cap mkdir `"`tex_dir'"'
use "data/panel_individual.dta", clear

global t34_group_nogender = "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
global t34_friend = "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
global t34_missing = "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global t34_share = "corner_share_i corner_share_diff mid_share_i mid_share_diff"

label var HighCCEI_both_high "\$Higher CCEI_i\$"
label var ccei_gap_ij "\$CCEI_i-CCEI_j\$"
label var mathscore_i "\$Math score_i\$"
label var mathscore_diff "\$Math score_{diff}\$"
label var inclass_popularity_i "\$In-degree_i\$"
label var inclass_pop_diff "\$In-degree_{diff}\$"
label var female_i_male_j "\$(Female_i, Male_j)\$"
label var male_i_female_j "\$(Male_i, Female_j)\$"

* Generate the six columns used in the current draft; no risk-aversion controls.
cap mkdir "Logs"
capture log close main_table3
log using "Logs/06_table3.log", name(main_table3) text replace
* Use every defined student-wave, matching Figure 3; no balance restriction.
* Retain one-wave students under individual FE (they supply no within variation).
preserve
gen byte available_t3 = !missing(`distance_var')
assert n_ccei_donors == 651 & !missing(M_ccei)
label var HighCCEI_both_high "\$Higher\ CCEI_i\$"
label var ccei_gap_ij "\$CCEI_i-CCEI_j\$"
label var M_ccei "\$M_{ig}\$"
label var mathscore_i "\$Math\ score_i\$"
label var mathscore_diff "\$Math\ score_{diff}\$"
label var inclass_popularity_i "\$In\text{-}degree_i\$"
label var inclass_pop_diff "\$In\text{-}degree_{diff}\$"
local full_controls "$t34_group_nogender $t34_friend $t34_missing $t34_share"
local gender "female_i_male_j male_i_female_j"
local selected "mathscore_i mathscore_diff female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff"

eststo clear
eststo bh1: reghdfe `distance_var' HighCCEI_both_high M_ccei if available_t3, absorb(class) vce(cluster class)
eststo bh3: reghdfe `distance_var' HighCCEI_both_high M_ccei `full_controls' `gender' if available_t3, absorb(class) vce(cluster class)
eststo bh4: reghdfe `distance_var' HighCCEI_both_high M_ccei `full_controls' if available_t3, absorb(id_fe) vce(cluster class) keepsingletons
eststo cd1: reghdfe `distance_var' ccei_gap_ij M_ccei if available_t3, absorb(class) vce(cluster class)
eststo cd3: reghdfe `distance_var' ccei_gap_ij M_ccei `full_controls' `gender' if available_t3, absorb(class) vce(cluster class)
eststo cd4: reghdfe `distance_var' ccei_gap_ij M_ccei `full_controls' if available_t3, absorb(id_fe) vce(cluster class) keepsingletons

foreach model in bh1 bh3 bh4 cd1 cd3 cd4 {
    estimates restore `model'
    assert e(N) == 2560 & e(N_clust) == 64
    assert e(sample) == available_t3
    quietly summarize `distance_var' if e(sample)
    estadd scalar ymean = r(mean) : `model'
    estadd scalar ysd = r(sd) : `model'
}
foreach model in bh1 bh3 cd1 cd3 {
    estadd local fixed_effects "Class" : `model'
}
foreach model in bh4 cd4 {
    estadd local fixed_effects "Individual" : `model'
}
foreach model in bh3 bh4 cd3 cd4 {
    estadd local characteristics "\checkmark" : `model'
    estadd local choice_shares "\checkmark" : `model'
}
esttab bh1 bh3 bh4 cd1 cd3 cd4 using `"`tex_dir'/table_bargainingCCEI_M.tex"', replace ///
    b(3) se(3) star(+ 0.1 * 0.05 ** 0.01) label nogap compress noomitted substitute(\_ _) ///
    keep(HighCCEI_both_high ccei_gap_ij M_ccei `selected') ///
    order(HighCCEI_both_high ccei_gap_ij M_ccei `selected') ///
    stats(N r2 fixed_effects characteristics choice_shares, fmt(0 3) ///
        labels("N" "R-squared" "Fixed effects" "Individual and friendship controls" "Corner/midpoint share controls")) ///
    fragment nomtitles nonumbers nolines prefoot("\midrule") postfoot("\bottomrule")
copy `"`tex_dir'/table_bargainingCCEI_M.tex"' ///
    `"`code_dir'/../Overleaf/tables_2025/table_bargainingCCEI_M.tex"', replace
restore
di as result "SUCCESS: exported current Table 3; six M-controlled columns, N=2560, clusters=64."
log close main_table3
