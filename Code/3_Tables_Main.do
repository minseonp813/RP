********************************************************************************
* 3_Tables_Main.do: Locked main Tables 1--3 and Figure 8 estimates.
* Last updated: 2026-10-11
* Original analysis authors: Byunghun Hahn and Minseon Park.
* Run from Code. Figures 6--8 use the sample prepared here; 4 draws the figures.
* Inputs: data/panel_individual.dta, panel_group.dta, panel_final.dta,
*   riskpreference_pre.dta, male.dta, height.dta, network_survey.dta,
*   cog_pre_clean.dta, NonCognitive_raw_pre.xlsx,
*   ../TalkFile_ceiv_fresh_3dp.csv.
* Outputs: results/tables/table_ccei_summary_IminusM.tex,
*   table_attrition_balance.tex, table_bargainingCCEI_M.tex,
*   table3_focal_estimates.csv; results/collective_quality/joint_outcome_linear_ame.csv.
* The three LaTeX tables are copied to their existing Overleaf paths.
* Logs: Logs/3_table3.log and Logs/3_collective_outcomes.log.
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

do "programs/build_collective_analysis_sample.do" `"`data_dir'"'

foreach required in panel_individual.dta panel_group.dta panel_final.dta ///
    panel_group_new_indices.dta ///
    riskpreference_pre.dta male.dta height.dta ///
    cog_pre_clean.dta NonCognitive_raw_pre.xlsx network_survey.dta {
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
* Table 2: Attrition and randomization
********************************************************************************

local code_dir `"`c(pwd)'"'
local data_dir `"`code_dir'/data"'
local tex_dir `"`code_dir'/results/tables"'

tempfile t2_baseline t2_analysis_ids t2_analysis t2_male t2_height
tempfile t2_math t2_noncog t2_network_out t2_network_in t2_edges
tempfile t2_randomization

use `"`data_dir'/riskpreference_pre.dta"', clear
gen double t2_expensive = cond(intercept_x < intercept_y, coord_x, coord_y)
gen double risk_aversion = t2_expensive / (coord_x + coord_y) ///
    if inrange(round_number, 1, 18) & coord_x + coord_y != 0
collapse (mean) ccei=ccei_ind risk_aversion, by(id)
isid id
gen long t2_class = floor(id / 100)
save `t2_baseline'

use `"`data_dir'/male.dta"', clear
keep id male
replace male = 0 if inlist(male, 0, 2)
replace male = 1 if male == 1
bysort id: keep if _n == 1
isid id
save `t2_male'

use `"`data_dir'/height.dta"', clear
keep id height
bysort id: keep if _n == 1
isid id
save `t2_height'

* Reuse the canonical first-row/completeness selection from 1_2.
use `"`data_dir'/cog_pre_clean.dta"', clear
rename mathscore_base math_score
destring id, replace
keep id math_score
isid id
save `t2_math'

import excel using `"`data_dir'/NonCognitive_raw_pre.xlsx"', firstrow clear
rename id_new id
foreach q of varlist Playerq33-Playerq42 {
    replace `q' = . if `q' == 0
}
gen double outgoing = (Playerq33 + (6 - Playerq38)) / 2 ///
    if !missing(Playerq33, Playerq38)
gen double agreeable = ((6 - Playerq34) + Playerq39) / 2 ///
    if !missing(Playerq34, Playerq39)
gen double conscientious = (Playerq35 + (6 - Playerq40)) / 2 ///
    if !missing(Playerq35, Playerq40)
gen double emotional_stability = ((6 - Playerq36) + Playerq41) / 2 ///
    if !missing(Playerq36, Playerq41)
gen double openness = (Playerq37 + (6 - Playerq42)) / 2 ///
    if !missing(Playerq37, Playerq42)
keep id outgoing agreeable conscientious emotional_stability openness
drop if missing(id)
bysort id: keep if _n == 1
isid id
save `t2_noncog'

use `"`data_dir'/network_survey.dta"', clear
keep if t == 0 & !missing(id)
gen long t2_id_class = floor(id / 100)
gen long t2_obj_class = floor(obj_id / 100)
keep if !missing(obj_id) & id != obj_id & t2_id_class == t2_obj_class
keep id obj_id
duplicates drop id obj_id, force
save `t2_edges'

preserve
    collapse (count) inclass_n_friends=obj_id, by(id)
    save `t2_network_out'
restore
rename obj_id network_id
collapse (count) inclass_popularity=id, by(network_id)
rename network_id id
save `t2_network_in'

use `t2_baseline', clear
merge 1:1 id using `t2_male', nogen keep(master match)
merge 1:1 id using `t2_height', nogen keep(master match)
merge 1:1 id using `t2_math', nogen keep(master match)
merge 1:1 id using `t2_noncog', nogen keep(master match)
merge 1:1 id using `t2_network_out', nogen keep(master match)
merge 1:1 id using `t2_network_in', nogen keep(master match)
replace inclass_n_friends = 0 if missing(inclass_n_friends)
replace inclass_popularity = 0 if missing(inclass_popularity)
count
assert r(N) == 1572
save `t2_baseline', replace

use `"`data_dir'/panel_individual.dta"', clear
keep if post == 0
destring id, replace
assert real(class) == floor(id / 100)
keep id ccei_i RA_i male_i height_i mathscore_i outgoing_i agreeable_i ///
    conscientious_i stable_i opened_i inclass_n_friends_i ///
    inclass_popularity_i mathscore_i_missing outgoing_i_missing ///
    agreeable_i_missing conscientious_i_missing stable_i_missing ///
    opened_i_missing
rename ccei_i ccei
rename RA_i risk_aversion
rename male_i male
rename height_i height
rename mathscore_i math_score
rename outgoing_i outgoing
rename agreeable_i agreeable
rename conscientious_i conscientious
rename stable_i emotional_stability
rename opened_i openness
rename inclass_n_friends_i inclass_n_friends
rename inclass_popularity_i inclass_popularity
replace math_score = . if mathscore_i_missing == 1
replace outgoing = . if outgoing_i_missing == 1
replace agreeable = . if agreeable_i_missing == 1
replace conscientious = . if conscientious_i_missing == 1
replace emotional_stability = . if stable_i_missing == 1
replace openness = . if opened_i_missing == 1
drop *_missing
isid id
gen long t2_class = floor(id / 100)
save `t2_analysis'
keep id
gen byte in_analysis = 1
save `t2_analysis_ids'

local t2_vars "ccei risk_aversion male height math_score inclass_n_friends inclass_popularity agreeable conscientious emotional_stability outgoing openness"
local t2_labels `""Individual CCEI" "Risk aversion" "Male" "Height" "Math score" "Out-degree" "In-degree" "Agreeableness" "Conscientiousness" "Emotional stability" "Outgoingness" "Openness""'

* Stack the overlapping samples so class clusters also account for shared students.
use `t2_baseline', clear
append using `t2_analysis', generate(t2_mean_sample)
local t2_k = 0
foreach v of local t2_vars {
    local ++t2_k
    quietly summarize `v' if t2_mean_sample == 0
    local t2_base_mean_`t2_k' = r(mean)

    quietly summarize `v' if t2_mean_sample == 1
    local t2_analysis_mean_`t2_k' = r(mean)
    local t2_diff_`t2_k' = `t2_analysis_mean_`t2_k'' - `t2_base_mean_`t2_k''

    quietly regress `v' t2_mean_sample, vce(cluster t2_class)
    local t2_attrition_p_`t2_k' = 2 * ttail(e(df_r), ///
        abs(_b[t2_mean_sample] / _se[t2_mean_sample]))
}

use `t2_baseline', clear
merge 1:1 id using `t2_analysis_ids', nogen keep(master match)
replace in_analysis = 0 if missing(in_analysis)
quietly regress in_analysis `t2_vars', vce(cluster t2_class)
quietly test `t2_vars'
local t2_attrition_F = r(F)
local t2_attrition_df = r(df)
local t2_attrition_df_r = r(df_r)
local t2_attrition_joint_p = r(p)

use `"`data_dir'/panel_final.dta"', clear
isid group_id
sort group_id
set rng mt64
set seed 100000
gen double t2_rand = runiform()
gen byte t2_selected_first = (t2_rand >= 0.5)
gen long t2_selected_id = cond(t2_selected_first, real(id_mover_base), real(id_nonmover_base))
gen long t2_partner_id = cond(t2_selected_first, real(id_nonmover_base), real(id_mover_base))
keep group_id class t2_selected_id t2_partner_id
egen long t2_class_fe = group(class)

rename t2_selected_id id
merge m:1 id using `t2_baseline', nogen keep(master match)
foreach v of local t2_vars {
    rename `v' `v'_i
}
rename id t2_selected_id
rename t2_partner_id id
merge m:1 id using `t2_baseline', nogen keep(master match)
foreach v of local t2_vars {
    rename `v' `v'_j
}
rename id t2_partner_id
assert _N == 652
save `t2_randomization'

local t2_joint_constraints ""
local t2_k = 0
foreach v of local t2_vars {
    local ++t2_k
    * suest requires conventional OLS fits; clustering is applied to the joint VCE.
    quietly regress `v'_i `v'_j i.t2_class_fe
    estimates store t2_rand_`t2_k'

    quietly regress `v'_i `v'_j i.t2_class_fe, vce(cluster t2_class_fe)
    local t2_random_beta_`t2_k' = _b[`v'_j]
    local t2_random_p_`t2_k' = 2 * ttail(e(df_r), abs(_b[`v'_j] / _se[`v'_j]))
    local t2_joint_constraints `"`t2_joint_constraints' ([t2_rand_`t2_k'_mean]`v'_j = 0)"'
}

quietly suest t2_rand_1 t2_rand_2 t2_rand_3 t2_rand_4 t2_rand_5 ///
    t2_rand_6 t2_rand_7 t2_rand_8 t2_rand_9 t2_rand_10 t2_rand_11 t2_rand_12, ///
    vce(cluster t2_class_fe)
quietly test `t2_joint_constraints'
local t2_random_chi2 = r(chi2)
local t2_random_df = r(df)
local t2_random_joint_p = r(p)

local bs = char(92)
file open t2_file using `"`tex_dir'/table_attrition_balance.tex"', write replace
file write t2_file "& (1) & (2) & (3) & (4) & (5) & (6) `bs'`bs'" _n
file write t2_file "& `bs'multicolumn{2}{c}{Randomization Test} & `bs'multicolumn{4}{c}{Sample Attrition} `bs'`bs'" _n
file write t2_file "`bs'cmidrule(lr){2-3}`bs'cmidrule(lr){4-7}" _n
file write t2_file "& & & `bs'multicolumn{2}{c}{Sample} & & `bs'`bs' `bs'cmidrule(lr){4-5}" _n
file write t2_file "& `bs'ensuremath{`bs'beta} & `bs'ensuremath{p}-value & Baseline & Analysis & Diff. & `bs'ensuremath{p}-value `bs'`bs' `bs'midrule" _n
file write t2_file "`bs'multicolumn{7}{l}{`bs'textbf{`bs'textit{Panel A: Experimental Measures}}} `bs'`bs'" _n

local t2_k = 0
foreach v of local t2_vars {
    local ++t2_k
    local t2_label : word `t2_k' of `t2_labels'
    if `t2_k' == 3 {
        file write t2_file "`bs'addlinespace" _n
        file write t2_file "`bs'multicolumn{7}{l}{`bs'textbf{`bs'textit{Panel B: Survey Measures}}} `bs'`bs'" _n
    }
    if `t2_k' == 4 {
        file write t2_file "`t2_label' & " %6.3f (`t2_random_beta_`t2_k'') " & " %6.3f (`t2_random_p_`t2_k'') " & " %6.1f (`t2_base_mean_`t2_k'') " & " %6.1f (`t2_analysis_mean_`t2_k'') " & " %6.3f (`t2_diff_`t2_k'') " & " %6.3f (`t2_attrition_p_`t2_k'') " `bs'`bs'" _n
    }
    else {
        file write t2_file "`t2_label' & " %6.3f (`t2_random_beta_`t2_k'') " & " %6.3f (`t2_random_p_`t2_k'') " & " %6.3f (`t2_base_mean_`t2_k'') " & " %6.3f (`t2_analysis_mean_`t2_k'') " & " %6.3f (`t2_diff_`t2_k'') " & " %6.3f (`t2_attrition_p_`t2_k'') " `bs'`bs'" _n
    }
}

file write t2_file "`bs'midrule" _n
file write t2_file "Joint test: & `bs'multicolumn{2}{l}{`bs'ensuremath{`bs'chi^{2}(" %3.0f (`t2_random_df') ") = " %6.2f (`t2_random_chi2') "}, `bs'ensuremath{p = " %6.3f (`t2_random_joint_p') "}} & `bs'multicolumn{4}{l}{`bs'ensuremath{F(" %3.0f (`t2_attrition_df') "," %6.0f (`t2_attrition_df_r') ") = " %6.2f (`t2_attrition_F') "}, `bs'ensuremath{p = " %6.3f (`t2_attrition_joint_p') "}} `bs'`bs'" _n
file write t2_file "N & `bs'multicolumn{2}{c}{652} & 1,572 & 1,304 & & `bs'`bs'" _n
file write t2_file "`bs'bottomrule" _n
file close t2_file
copy `"`tex_dir'/table_attrition_balance.tex"' ///
    `"`code_dir'/../Overleaf/tables_2025/table_attrition_balance.tex"', replace
estimates clear


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
log using "Logs/3_table3.log", name(main_table3) text replace
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
tempname focal_output
tempfile focal_results
postfile `focal_output' byte specification str18 focal double actual_coefficient double actual_se ///
    double actual_coefficient_M long N clusters using `focal_results'
local column = 0
foreach model in bh1 bh3 bh4 cd1 cd3 cd4 {
    local ++column
    estimates restore `model'
    local focal "HighCCEI_both_high"
    if `column'>3 local focal "ccei_gap_ij"
    post `focal_output' (`column') ("`focal'") (_b[`focal']) (_se[`focal']) ///
        (_b[M_ccei]) (e(N)) (e(N_clust))
}
postclose `focal_output'
use `focal_results', clear
export delimited using "`tex_dir'/table3_focal_estimates.csv", replace
restore
di as result "SUCCESS: exported current Table 3; six M-controlled columns, N=2560, clusters=64."
log close main_table3

********************************************************************************
* Figure 8 estimates: three LPM specifications for the four joint outcomes.
* Run this section independently from Code. 4 draws the two main panels.
********************************************************************************
set more off
adopath ++ "programs"
local out "results/collective_quality"
cap mkdir "results"
cap mkdir "`out'"
cap mkdir "Logs"
capture log close collective_main
log using "Logs/3_collective_outcomes.log", name(collective_main) text replace
do "programs/build_collective_analysis_sample.do" "`c(pwd)'/data"

* Figure 8: four LPM outcomes in three specifications.
local full_controls "$t5_group $t5_friend $t5_share"
tempname ames
tempfile ame_results
postfile `ames' str13 specification byte outcome str7 member double estimate se low high ///
    double share long n pairs clusters using `ame_results'
foreach specification in class_fe full_controls pair_fe {
    local controls "`full_controls'"
    if "`specification'"=="class_fe" local controls ""
    local fe class_fe
    if "`specification'"=="pair_fe" local fe pair_fe
    collective_linear_ame joint_category, controls(`controls') fe(`fe')
    assert r(n)==1304 & r(pairs)==652 & r(clusters)==64
    matrix effects = r(effects)
    forvalues category=1/4 {
        forvalues member=1/2 {
            local name = cond(`member'==1,"maximum","minimum")
            local start = 4*(`member'-1)
            post `ames' ("`specification'") (`category') ("`name'") ///
                (effects[`category',`start'+1]) (effects[`category',`start'+2]) ///
                (effects[`category',`start'+3]) (effects[`category',`start'+4]) ///
                (effects[`category',9]) (1304) (652) (64)
        }
    }
}
postclose `ames'
preserve
    use `ame_results', clear
    export delimited using "`out'/joint_outcome_linear_ame.csv", replace
restore

di as result "SUCCESS: Figure 8 LPMs validated on 1,304 pair-waves and 64 classes."
log close collective_main
