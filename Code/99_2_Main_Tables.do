********************************************************************************
* 99_2_Main_Tables.do
* Standalone generator for the five main tables in main_v2.tex.
* This file intentionally does not call any existing do-file.
********************************************************************************

version 17
clear all
set more off
set matsize 8000

local code_dir `"`c(pwd)'"'
local data_dir `"`code_dir'/data"'
local tex_dir `"C:/Users/minseonp/Dropbox/OverleafGit/Group Decision/tables_2025"'

foreach required in `"`data_dir'/finalized_panel_individual_251206.dta"' `"`data_dir'/finalized_panel_pbl_251206.dta"' `"`data_dir'/finalized_panel_final_251203.dta"' {
    capture confirm file `"`required'"'
    if _rc {
        di as error "Missing required input: `required'"
        exit 601
    }
}

capture which reghdfe
if _rc {
    di as error "The reghdfe package is required. Install it with: ssc install reghdfe"
    exit 199
}
capture which esttab
if _rc {
    di as error "The estout package is required. Install it with: ssc install estout"
    exit 199
}

********************************************************************************
* Table 1: Summary Statistics Of Experimental Measures
********************************************************************************

use `"`data_dir'/finalized_panel_individual_251206.dta"', clear
foreach period in 0 1 {
    local suffix = cond(`period' == 0, "base", "end")
    foreach var in ccei_i RA_i new2_I_ig {
        quietly summarize `var' if post == `period', detail
        local `var'_`suffix'_mean = r(mean)
        local `var'_`suffix'_sd   = r(sd)
        local `var'_`suffix'_p10  = r(p10)
        local `var'_`suffix'_p50  = r(p50)
        local `var'_`suffix'_p90  = r(p50)
        local `var'_`suffix'_p90  = r(p90)
        local `var'_`suffix'_N    = r(N)
    }
}

use `"`data_dir'/finalized_panel_pbl_251206.dta"', clear
foreach period in 0 1 {
    local suffix = cond(`period' == 0, "base", "end")
    foreach var in ccei_g RA_g {
        quietly summarize `var' if time == `period', detail
        local `var'_`suffix'_mean = r(mean)
        local `var'_`suffix'_sd   = r(sd)
        local `var'_`suffix'_p10  = r(p10)
        local `var'_`suffix'_p50  = r(p50)
        local `var'_`suffix'_p90  = r(p90)
        local `var'_`suffix'_N    = r(N)
    }
}

capture file close summary_file
file open summary_file using `"`tex_dir'/table_ccei_summary.tex"', write replace
local slash = char(92)
file write summary_file "& (1) & (2) & (3) & (4) & (5) & (6) `slash'`slash'" _n
file write summary_file "                 & Mean & SD & p10 & p50 & p90 & N `slash'`slash'" _n
file write summary_file "`slash'midrule" _n
file write summary_file "`slash'multicolumn{7}{l}{`slash'emph{Panel A: Baseline}} `slash'`slash'" _n
file write summary_file "`slash'hspace{1em}Individual risk aversion & " %6.3f (`RA_i_base_mean') " & " %6.3f (`RA_i_base_sd') " & " %6.3f (`RA_i_base_p10') " & " %6.3f (`RA_i_base_p50') " & " %6.3f (`RA_i_base_p90') " & " %9.0fc (`RA_i_base_N') " `slash'`slash'" _n
file write summary_file "`slash'hspace{1em}Group risk aversion      & " %6.3f (`RA_g_base_mean') " & " %6.3f (`RA_g_base_sd') " & " %6.3f (`RA_g_base_p10') " & " %6.3f (`RA_g_base_p50') " & " %6.3f (`RA_g_base_p90') " & " %9.0fc (`RA_g_base_N') " `slash'`slash'" _n
file write summary_file "`slash'hspace{1em}Individual CCEI & " %6.3f (`ccei_i_base_mean') " & " %6.3f (`ccei_i_base_sd') " & " %6.3f (`ccei_i_base_p10') " & " %6.3f (`ccei_i_base_p50') " & " %6.3f (`ccei_i_base_p90') " & " %9.0fc (`ccei_i_base_N') " `slash'`slash'" _n
file write summary_file "`slash'hspace{1em}Group CCEI      & " %6.3f (`ccei_g_base_mean') " & " %6.3f (`ccei_g_base_sd') " & " %6.3f (`ccei_g_base_p10') " & " %6.3f (`ccei_g_base_p50') " & " %6.3f (`ccei_g_base_p90') " & " %9.0fc (`ccei_g_base_N') " `slash'`slash'" _n
file write summary_file "`slash'hspace{1em}Preference distance `slash'ensuremath{I_{ig}} & " %6.3f (`new2_I_ig_base_mean') " & " %6.3f (`new2_I_ig_base_sd') " & " %6.3f (`new2_I_ig_base_p10') " & " %6.3f (`new2_I_ig_base_p50') " & " %6.3f (`new2_I_ig_base_p90') " & " %9.0fc (`new2_I_ig_base_N') " `slash'`slash'" _n
file write summary_file "`slash'addlinespace" _n
file write summary_file "`slash'multicolumn{7}{l}{`slash'emph{Panel B: Endline}} `slash'`slash'" _n
file write summary_file "`slash'hspace{1em}Individual risk aversion & " %6.3f (`RA_i_end_mean') " & " %6.3f (`RA_i_end_sd') " & " %6.3f (`RA_i_end_p10') " & " %6.3f (`RA_i_end_p50') " & " %6.3f (`RA_i_end_p90') " & " %9.0fc (`RA_i_end_N') " `slash'`slash'" _n
file write summary_file "`slash'hspace{1em}Group risk aversion      & " %6.3f (`RA_g_end_mean') " & " %6.3f (`RA_g_end_sd') " & " %6.3f (`RA_g_end_p10') " & " %6.3f (`RA_g_end_p50') " & " %6.3f (`RA_g_end_p90') " & " %9.0fc (`RA_g_end_N') " `slash'`slash'" _n
file write summary_file "`slash'hspace{1em}Individual CCEI & " %6.3f (`ccei_i_end_mean') " & " %6.3f (`ccei_i_end_sd') " & " %6.3f (`ccei_i_end_p10') " & " %6.3f (`ccei_i_end_p50') " & " %6.3f (`ccei_i_end_p90') " & " %9.0fc (`ccei_i_end_N') " `slash'`slash'" _n
file write summary_file "`slash'hspace{1em}Group CCEI      & " %6.3f (`ccei_g_end_mean') " & " %6.3f (`ccei_g_end_sd') " & " %6.3f (`ccei_g_end_p10') " & " %6.3f (`ccei_g_end_p50') " & " %6.3f (`ccei_g_end_p90') " & " %9.0fc (`ccei_g_end_N') " `slash'`slash'" _n
file write summary_file "`slash'hspace{1em}Preference distance `slash'ensuremath{I_{ig}} & " %6.3f (`new2_I_ig_end_mean') " & " %6.3f (`new2_I_ig_end_sd') " & " %6.3f (`new2_I_ig_end_p10') " & " %6.3f (`new2_I_ig_end_p50') " & " %6.3f (`new2_I_ig_end_p90') " & " %9.0fc (`new2_I_ig_end_N') " `slash'`slash'" _n
file write summary_file "`slash'bottomrule" _n
file close summary_file

********************************************************************************
* Table 2: Sample Attrition And Randomization Test
********************************************************************************

local rscript `"C:\PROGRA~1\R\R-46~1.1\bin\x64\Rscript.exe"'
local attrition_r `"C:\Users\minseonp\Dropbox\RP\CODE_R~1\12_ATT~1.R"'
shell C:\PROGRA~1\R\R-46~1.1\bin\x64\Rscript.exe --vanilla C:\Users\minseonp\Dropbox\RP\CODE_R~1\12_ATT~1.R
if _rc {
    di as error "The attrition R generator failed: `attrition_r'"
    exit _rc
}

********************************************************************************
* Tables 3 and 4: Individual Rationality And Revealed Influence; RA distance
********************************************************************************

use `"`data_dir'/finalized_panel_individual_251206.dta"', clear

tempfile choice_shares partner_choice_shares baseline_choice_shares
preserve
    use `"`data_dir'/for_graphs_baseline_raw.dta"', clear
    keep if game_type == 1
    gen post = 0
    gen corner_share = (coord_x == 0 | coord_y == 0)
    gen mid_share = (coord_x == coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    save `baseline_choice_shares'
    use `"`data_dir'/for_graphs_endline_raw.dta"', clear
    keep if game_type == 1
    gen post = 1
    gen corner_share = (coord_x == 0 | coord_y == 0)
    gen mid_share = (coord_x == coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    append using `baseline_choice_shares'
    save `choice_shares'
    rename id partner_id
    save `partner_choice_shares'
restore
merge m:1 id post using `choice_shares', keep(master match) nogen
rename corner_share corner_share_i
rename mid_share mid_share_i
merge m:1 partner_id post using `partner_choice_shares', keep(master match) nogen
rename corner_share corner_share_j
rename mid_share mid_share_j

capture drop corner_share_diff mid_share_diff female_i_male_j male_i_female_j
gen corner_share_diff = corner_share_i - corner_share_j
gen mid_share_diff = mid_share_i - mid_share_j
gen female_i_male_j = (male_i == 0 & male_j == 1)
gen male_i_female_j = (male_i == 1 & male_j == 0)
foreach v in mathscore_i outgoing_i opened_i agreeable_i conscientious_i stable_i {
    replace `v' = 0 if missing(`v')
}
capture drop math_diff
gen math_diff = mathscore_i - mathscore_j
replace math_diff = 0 if missing(math_diff)
foreach v in outgoing opened agreeable conscientious stable {
    capture drop `v'_diff `v'_diff_missing
    gen `v'_diff = `v'_i - `v'_j
    replace `v'_diff = 0 if missing(`v'_diff)
    gen `v'_diff_missing = missing(`v'_i) | missing(`v'_j)
}
capture drop inclass_pop_diff
capture drop height_diff
capture drop RA_diff
gen inclass_pop_diff = inclass_popularity_i - inclass_popularity_j
gen height_diff = height_i - height_j
global group_char "mathscore_i math_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff female_i_male_j male_i_female_j"
global group_char_no_gender "mathscore_i math_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
global friend_char "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
global missing_char "mathscore_dist_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global RA_char "RA_i RA_diff"
global share_char "corner_share_i corner_share_diff mid_share_i mid_share_diff"
gen RA_diff = RA_i - RA_j

reghdfe new2_I_ig HighCCEI $group_char $friend_char $missing_char, absorb(class) vce(cluster class)
gen byte balanced = e(sample)
reghdfe new2_I_ig HighCCEI $group_char_no_gender $friend_char $missing_char, absorb(id) vce(cluster class)
replace balanced = balanced & e(sample)

local dollar = char(36)
label var HighCCEI "`dollar'Higher CCEI_i`dollar'"
label var mathscore_i "`dollar'Math score_i`dollar'"
label var math_diff "`dollar'Math score_{diff}`dollar'"
label var inclass_popularity_i "`dollar'In-degree_i`dollar'"
label var inclass_pop_diff "`dollar'In-degree_{diff}`dollar'"
label var female_i_male_j "`dollar'(Female_i, Male_j)`dollar'"
label var male_i_female_j "`dollar'(Male_i, Female_j)`dollar'"

eststo clear
eststo: reghdfe new2_I_ig HighCCEI if balanced, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI $group_char $friend_char $missing_char if balanced, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI $group_char $friend_char $missing_char $RA_char $share_char if balanced, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI $group_char_no_gender $friend_char $missing_char $RA_char $share_char if balanced, absorb(id) vce(cluster class)
esttab using `"`tex_dir'/table_bargainingCCEI.tex"', replace b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01) label keep(HighCCEI mathscore_i math_diff inclass_popularity_i inclass_pop_diff female_i_male_j male_i_female_j) nomtitles fragment nonumbers nolines

capture drop RA_distance_denom RA_I_ig
 gen RA_distance_denom = (RA_i-RA_g)^2 + (RA_j-RA_g)^2
gen RA_I_ig = (RA_i-RA_g)^2 / RA_distance_denom
replace RA_I_ig = . if RA_distance_denom == 0
label var RA_I_ig "RA distance"
eststo clear
eststo: reghdfe RA_I_ig HighCCEI, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI $group_char $friend_char $missing_char, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI $group_char $friend_char $missing_char $RA_char $share_char, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI $group_char_no_gender $friend_char $missing_char $RA_char $share_char, absorb(id) vce(cluster class) keepsingletons
esttab using `"`tex_dir'/table_bargainingRA_distance.tex"', replace b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01) label keep(HighCCEI mathscore_i math_diff inclass_popularity_i inclass_pop_diff female_i_male_j male_i_female_j) nomtitles fragment nonumbers nolines

********************************************************************************
* Table 5: Collective CCEI
********************************************************************************

use `"`data_dir'/finalized_panel_pbl_251206.dta"', clear
gen male_diff = (male != male2)
gen friend = (friendship >= 1)
global group_char "mathscore_max mathscore_dist mathscore_max_missing mathscore_dist_missing height_max height_dist male_diff outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_dist conscientious_max stable_max stable_dist big5_max_missing big5_dist_missing"
global friend_char "inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist friend"
global RA_char "RA_max RA_dist"
label var ccei_max "`dollar'CCEI_{max,gt}`dollar'"
label var ccei_dist "`dollar'CCEI_{dist,gt}`dollar'"
eststo clear
eststo: reghdfe ccei_g ccei_max ccei_dist, absorb(class) vce(cluster class)
estadd local student_controls "No"
estadd local ra_controls "No"
estadd local class_fe "Yes"
estadd local pair_fe "No"
eststo: reghdfe ccei_g ccei_max ccei_dist $group_char $friend_char, absorb(class) vce(cluster class)
estadd local student_controls "Yes"
estadd local ra_controls "No"
estadd local class_fe "Yes"
estadd local pair_fe "No"
eststo: reghdfe ccei_g ccei_max ccei_dist $group_char $friend_char $RA_char, absorb(class) vce(cluster class)
estadd local student_controls "Yes"
estadd local ra_controls "Yes"
estadd local class_fe "Yes"
estadd local pair_fe "No"
eststo: reghdfe ccei_g ccei_max ccei_dist $group_char $friend_char $RA_char, absorb(group_id) vce(cluster class)
estadd local student_controls "Yes"
estadd local ra_controls "Yes"
estadd local class_fe "No"
estadd local pair_fe "Yes"
esttab using `"`tex_dir'/final_collective_ccei.tex"', replace b(3) se(3) stats(student_controls ra_controls class_fe pair_fe N r2, labels("Student and friendship controls" "RA controls" "Class fixed effects" "Pair fixed effects" "N" "R-squared") fmt(%9s %9s %9s %9s 0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01) label keep(ccei_max ccei_dist) nomtitles fragment nonumbers nolines substitute(\\_ _)

di as text "99_2_Main_Tables.do completed. Outputs written to: `tex_dir'"