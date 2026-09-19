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
    base_raw.dta end_raw.dta riskpreference_pre.dta male.dta height.dta ///
    Cognitive_raw_pre_full.xlsx NonCognitive_raw_pre.xlsx network_survey.dta {
    capture confirm file `"`data_dir'/`required'"'
    if _rc {
        di as error "Missing required input: `data_dir'/`required'"
        exit 601
    }
}
* Table 1: Summary statistics

use `"`data_dir'/panel_individual.dta"', clear

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
    quietly summarize `distance_var' if post == `period', detail
    local distance_`suffix'_mean = r(mean)
    local distance_`suffix'_sd   = r(sd)
    local distance_`suffix'_p10  = r(p10)
    local distance_`suffix'_p50  = r(p50)
    local distance_`suffix'_p90  = r(p90)
    local distance_`suffix'_N    = r(N)
}

use `"`data_dir'/panel_group.dta"', clear

foreach period in 0 1 {
    local suffix = cond(`period' == 0, "base", "end")
    foreach var in ccei_g RA_g {
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
file open summary_file using `"`tex_dir'/table_ccei_summary.tex"', write replace
local bs = char(92)
local distance_tex "`bs'ensuremath{I_{ig}}"
file write summary_file "& (1) & (2) & (3) & (4) & (5) & (6) `bs'`bs'" _n
file write summary_file " & Mean & SD & p10 & p50 & p90 & N `bs'`bs'" _n
file write summary_file "`bs'midrule" _n
file write summary_file "`bs'multicolumn{7}{l}{`bs'emph{Panel A: Baseline}} `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual CCEI & " %6.3f (`ccei_i_base_mean') " & " %6.3f (`ccei_i_base_sd') " & " %6.3f (`ccei_i_base_p10') " & " %6.3f (`ccei_i_base_p50') " & " %6.3f (`ccei_i_base_p90') " & " %9.0fc (`ccei_i_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group CCEI & " %6.3f (`ccei_g_base_mean') " & " %6.3f (`ccei_g_base_sd') " & " %6.3f (`ccei_g_base_p10') " & " %6.3f (`ccei_g_base_p50') " & " %6.3f (`ccei_g_base_p90') " & " %9.0fc (`ccei_g_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Preference distance `distance_tex' & " %6.3f (`distance_base_mean') " & " %6.3f (`distance_base_sd') " & " %6.3f (`distance_base_p10') " & " %6.3f (`distance_base_p50') " & " %6.3f (`distance_base_p90') " & " %9.0fc (`distance_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual risk aversion & " %6.3f (`RA_i_base_mean') " & " %6.3f (`RA_i_base_sd') " & " %6.3f (`RA_i_base_p10') " & " %6.3f (`RA_i_base_p50') " & " %6.3f (`RA_i_base_p90') " & " %9.0fc (`RA_i_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group risk aversion & " %6.3f (`RA_g_base_mean') " & " %6.3f (`RA_g_base_sd') " & " %6.3f (`RA_g_base_p10') " & " %6.3f (`RA_g_base_p50') " & " %6.3f (`RA_g_base_p90') " & " %9.0fc (`RA_g_base_N') " `bs'`bs'" _n
file write summary_file "`bs'addlinespace" _n
file write summary_file "`bs'multicolumn{7}{l}{`bs'emph{Panel B: Endline}} `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual CCEI & " %6.3f (`ccei_i_end_mean') " & " %6.3f (`ccei_i_end_sd') " & " %6.3f (`ccei_i_end_p10') " & " %6.3f (`ccei_i_end_p50') " & " %6.3f (`ccei_i_end_p90') " & " %9.0fc (`ccei_i_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group CCEI & " %6.3f (`ccei_g_end_mean') " & " %6.3f (`ccei_g_end_sd') " & " %6.3f (`ccei_g_end_p10') " & " %6.3f (`ccei_g_end_p50') " & " %6.3f (`ccei_g_end_p90') " & " %9.0fc (`ccei_g_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Preference distance `distance_tex' & " %6.3f (`distance_end_mean') " & " %6.3f (`distance_end_sd') " & " %6.3f (`distance_end_p10') " & " %6.3f (`distance_end_p50') " & " %6.3f (`distance_end_p90') " & " %9.0fc (`distance_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual risk aversion & " %6.3f (`RA_i_end_mean') " & " %6.3f (`RA_i_end_sd') " & " %6.3f (`RA_i_end_p10') " & " %6.3f (`RA_i_end_p50') " & " %6.3f (`RA_i_end_p90') " & " %9.0fc (`RA_i_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group risk aversion & " %6.3f (`RA_g_end_mean') " & " %6.3f (`RA_g_end_sd') " & " %6.3f (`RA_g_end_p10') " & " %6.3f (`RA_g_end_p50') " & " %6.3f (`RA_g_end_p90') " & " %9.0fc (`RA_g_end_N') " `bs'`bs'" _n
file write summary_file "`bs'bottomrule" _n
file close summary_file

* Table 2: Attrition and randomization

tempfile t2_baseline t2_analysis_ids t2_analysis t2_male t2_height
tempfile t2_math t2_noncog t2_network_out t2_network_in t2_edges
tempfile t2_randomization

use `"`data_dir'/riskpreference_pre.dta"', clear
gen double t2_expensive = cond(intercept_x < intercept_y, coord_x, coord_y)
gen double risk_aversion = t2_expensive / (coord_x + coord_y) ///
    if inrange(round_number, 1, 18) & coord_x + coord_y != 0
collapse (mean) ccei=ccei_ind risk_aversion, by(id)
isid id
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

import excel using `"`data_dir'/Cognitive_raw_pre_full.xlsx"', firstrow clear
gen str12 t2_id_string = trim(label)
replace t2_id_string = "11" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "B"
replace t2_id_string = "12" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "P"
replace t2_id_string = "13" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "C"
replace t2_id_string = "14" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "F"
replace t2_id_string = "15" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "M"
replace t2_id_string = "16" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "R"
replace t2_id_string = "21" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "L"
replace t2_id_string = "22" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "Q"
replace t2_id_string = "23" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "J"
replace t2_id_string = "24" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "A"
replace t2_id_string = "25" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "K"
replace t2_id_string = "26" + substr(t2_id_string, 2, .) if substr(upper(t2_id_string), 1, 1) == "E"
drop id
destring t2_id_string, gen(id) force
foreach q of varlist cq1-cq5 {
    replace `q' = . if `q' == 0
}
egen byte t2_math_nonmissing = rownonmiss(cq1-cq5)
gen byte math_score = (cq1 == 2) + (cq2 == 4) + (cq3 == 3) + ///
    (cq4 == 5) + (cq5 == 1)
drop if t2_math_nonmissing == 0 | missing(id)
gsort id -t2_math_nonmissing
by id: keep if _n == 1
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
save `t2_analysis'
keep id
gen byte in_analysis = 1
save `t2_analysis_ids'

local t2_vars "ccei risk_aversion male height math_score inclass_n_friends inclass_popularity agreeable conscientious emotional_stability outgoing openness"
local t2_labels `""Individual CCEI" "Risk aversion" "Male" "Height" "Math score" "Out-degree" "In-degree" "Agreeableness" "Conscientiousness" "Emotional stability" "Outgoingness" "Openness""'

local t2_k = 0
foreach v of local t2_vars {
    local ++t2_k
    use `t2_baseline', clear
    quietly summarize `v'
    local t2_base_mean_`t2_k' = r(mean)
    local t2_base_sd_`t2_k' = r(sd)
    local t2_base_n_`t2_k' = r(N)

    use `t2_analysis', clear
    quietly summarize `v'
    local t2_analysis_mean_`t2_k' = r(mean)
    local t2_analysis_sd_`t2_k' = r(sd)
    local t2_analysis_n_`t2_k' = r(N)
    local t2_diff_`t2_k' = `t2_analysis_mean_`t2_k'' - `t2_base_mean_`t2_k''

    quietly ttesti `t2_base_n_`t2_k'' `t2_base_mean_`t2_k'' `t2_base_sd_`t2_k'' ///
        `t2_analysis_n_`t2_k'' `t2_analysis_mean_`t2_k'' `t2_analysis_sd_`t2_k'', unequal
    local t2_attrition_p_`t2_k' = r(p)
}

use `t2_baseline', clear
merge 1:1 id using `t2_analysis_ids', nogen keep(master match)
replace in_analysis = 0 if missing(in_analysis)
quietly regress in_analysis `t2_vars'
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
    quietly regress `v'_i `v'_j i.t2_class_fe
    estimates store t2_rand_`t2_k'
    local t2_random_beta_`t2_k' = _b[`v'_j]
    local t2_random_p_`t2_k' = 2 * ttail(e(df_r), abs(_b[`v'_j] / _se[`v'_j]))
    local t2_joint_constraints `"`t2_joint_constraints' ([t2_rand_`t2_k'_mean]`v'_j = 0)"'
}

quietly suest t2_rand_1 t2_rand_2 t2_rand_3 t2_rand_4 t2_rand_5 ///
    t2_rand_6 t2_rand_7 t2_rand_8 t2_rand_9 t2_rand_10 t2_rand_11 t2_rand_12
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
estimates clear

* Prepare Tables 3 and 4

use `"`data_dir'/panel_individual.dta"', clear

tempfile choice_shares partner_choice_shares baseline_choice_shares
preserve
    use `"`data_dir'/base_raw.dta"', clear
    keep if game_type == 1
    gen post = 0
    gen byte corner_share = (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    save `baseline_choice_shares'

    use `"`data_dir'/end_raw.dta"', clear
    keep if game_type == 1
    gen post = 1
    gen byte corner_share = (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
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
merge m:1 partner_id post using `partner_choice_shares', keep(master match) nogen
rename corner_share corner_share_j
rename mid_share mid_share_j

capture drop corner_share_diff mid_share_diff female_i_male_j male_i_female_j
gen corner_share_diff = corner_share_i - corner_share_j
gen mid_share_diff = mid_share_i - mid_share_j
gen female_i_male_j = (male_i == 0 & male_j == 1)
gen male_i_female_j = (male_i == 1 & male_j == 0)

global t34_group = "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff female_i_male_j male_i_female_j"
global t34_group_nogender = "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
global t34_friend = "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
global t34_missing = "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global t34_ra = "RA_i RA_diff"
global t34_share = "corner_share_i corner_share_diff mid_share_i mid_share_diff"

bysort id: egen n_distance = total(!missing(`distance_var'))
gen byte balanced_t3 = (n_distance == 2)
drop n_distance
egen long id_fe = group(id)

label var HighCCEI_both_high "\$Higher CCEI_i\$"
label var HighCCEI_both_low "\$Higher CCEI_i\$"
label var ccei_gap_ij "\$CCEI_i-CCEI_j\$"
label var mathscore_i "\$Math score_i\$"
label var mathscore_diff "\$Math score_{diff}\$"
label var inclass_popularity_i "\$In-degree_i\$"
label var inclass_pop_diff "\$In-degree_{diff}\$"
label var female_i_male_j "\$(Female_i, Male_j)\$"
label var male_i_female_j "\$(Male_i, Female_j)\$"

* Table 3: Individual rationality and revealed influence

eststo clear
eststo bh1: reghdfe `distance_var' HighCCEI_both_high if balanced_t3, absorb(class) vce(cluster class)
* eststo bh2: reghdfe `distance_var' HighCCEI_both_high $t34_group $t34_friend $t34_missing if balanced_t3, absorb(class) vce(cluster class)
eststo bh3: reghdfe `distance_var' HighCCEI_both_high $t34_group $t34_friend $t34_missing $t34_share if balanced_t3, absorb(class) vce(cluster class)
eststo bh4: reghdfe `distance_var' HighCCEI_both_high $t34_group_nogender $t34_friend $t34_missing $t34_share if balanced_t3, absorb(id_fe) vce(cluster class)

eststo cd1: reghdfe `distance_var' ccei_gap_ij if balanced_t3, absorb(class) vce(cluster class)
* eststo cd2: reghdfe `distance_var' ccei_gap_ij $t34_group $t34_friend $t34_missing if balanced_t3, absorb(class) vce(cluster class)
eststo cd3: reghdfe `distance_var' ccei_gap_ij $t34_group $t34_friend $t34_missing $t34_share if balanced_t3, absorb(class) vce(cluster class)
eststo cd4: reghdfe `distance_var' ccei_gap_ij $t34_group_nogender $t34_friend $t34_missing $t34_share if balanced_t3, absorb(id_fe) vce(cluster class)

foreach m in bh1 bh3 cd1 cd3 {
    estadd local fixed_effects "Class" : `m'
}
foreach m in bh4 cd4 {
    estadd local fixed_effects "Individual" : `m'
}
foreach m in bh3 bh4 cd3 cd4 {
    estadd local individual_controls "\checkmark" : `m'
    estadd local friendship_controls "\checkmark" : `m'
    estadd local share_controls "\checkmark" : `m'
}

esttab bh1 bh3 bh4 cd1 cd3 cd4 using `"`tex_dir'/table_bargainingCCEI.tex"', replace ///
    b(3) se(3) stats(N r2 fixed_effects individual_controls friendship_controls share_controls, ///
    labels("N" "R-squared" "Fixed effects" "Other individual characteristics" ///
    "Other friendship characteristics" "Corner/midpoint share controls") ///
    fmt(0 3 %9s %9s %9s %9s)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label substitute(\_ _) ///
    keep(HighCCEI_both_high ccei_gap_ij mathscore_i mathscore_diff inclass_popularity_i inclass_pop_diff female_i_male_j male_i_female_j) ///
    order(HighCCEI_both_high ccei_gap_ij mathscore_i mathscore_diff female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff) ///
    nomtitles fragment nonumbers nolines prefoot("\hline") postfoot("\hline \bottomrule")

* Table 4: Risk-aversion distance

capture drop RA_distance_denom RA_I_ig
gen RA_distance_denom = (RA_i - RA_g)^2 + (RA_j - RA_g)^2
gen RA_I_ig = (RA_i - RA_g)^2 / RA_distance_denom
replace RA_I_ig = . if RA_distance_denom == 0

* Retain the separate ties-assigned-Low robustness specification.
eststo clear
eststo: reghdfe RA_I_ig HighCCEI_both_low, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI_both_low $t34_group $t34_friend $t34_missing, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI_both_low $t34_group $t34_friend $t34_missing $t34_ra $t34_share, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI_both_low $t34_group_nogender $t34_friend $t34_missing $t34_ra $t34_share, absorb(id_fe) vce(cluster class) keepsingletons
esttab using `"`tex_dir'/table_bargainingRA_distance_bothlow.tex"', replace ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label substitute(\_ _) ///
    keep(HighCCEI_both_low mathscore_i mathscore_diff inclass_popularity_i inclass_pop_diff female_i_male_j male_i_female_j) ///
    nomtitles fragment nonumbers nolines prefoot("\hline") postfoot("\bottomrule")

* Refresh the combined appendix table and its component fragments.
do "99_21_appendix_robustness.do"

* Table 5: Collective CCEI

use `"`data_dir'/panel_individual.dta"', clear

tempfile t5_shares t5_partner_shares t5_base_shares
preserve
    use `"`data_dir'/base_raw.dta"', clear
    keep if game_type == 1
    gen post = 0
    gen byte corner_share = (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    save `t5_base_shares'

    use `"`data_dir'/end_raw.dta"', clear
    keep if game_type == 1
    gen post = 1
    gen byte corner_share = (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    append using `t5_base_shares'
    save `t5_shares'
    rename id partner_id
    save `t5_partner_shares'
restore

merge m:1 id post using `t5_shares', keep(master match) nogen
rename corner_share corner_share_i
rename mid_share mid_share_i
merge m:1 partner_id post using `t5_partner_shares', keep(master match) nogen
rename corner_share corner_share_j
rename mid_share mid_share_j

foreach v in mathscore outgoing opened agreeable conscientious stable {
    replace `v'_i = 0 if missing(`v'_i)
    replace `v'_j = 0 if missing(`v'_j)
}
foreach v in ccei RA mathscore height outgoing opened agreeable conscientious stable inclass_n_friends inclass_popularity corner_share mid_share {
    capture drop `v'_max
    capture drop `v'_dist
    egen `v'_max = rowmax(`v'_i `v'_j)
    gen `v'_dist = abs(`v'_i - `v'_j)
}
capture drop male_diff
capture drop friend
gen male_diff = (male_i != male_j)
gen friend = (friendship >= 1)
bysort group_id post: keep if _n == 1
isid group_id post
egen long class_fe = group(class)
egen long pair_fe = group(group_id)

global t5_group = "mathscore_max mathscore_dist height_max height_dist male_diff outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_max conscientious_dist stable_max stable_dist mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global t5_friend = "inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist friend"
global t5_ra = "RA_max RA_dist"
global t5_share = "corner_share_max corner_share_dist mid_share_max mid_share_dist"

label var ccei_max "\$\text{CCEI}_{\text{max},gt}\$"
label var ccei_dist "\$\text{CCEI}_{\text{dist},gt}\$"

eststo clear
eststo: reghdfe ccei_g ccei_max ccei_dist, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe ccei_g ccei_max ccei_dist $t5_group $t5_friend, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe ccei_g ccei_max ccei_dist $t5_group $t5_friend $t5_ra $t5_share, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe ccei_g ccei_max ccei_dist $t5_group $t5_friend $t5_ra $t5_share, absorb(pair_fe) vce(cluster class_fe)

esttab using `"`tex_dir'/final_collective_ccei.tex"', replace ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(ccei_max ccei_dist) prefoot("\midrule") postfoot("\bottomrule") ///
    nomtitles fragment nonumbers nolines substitute(\_ _)

* Alternative CCEI specification

capture drop ccei_high ccei_low
gen double ccei_high = ccei_max
gen double ccei_low = ccei_max - ccei_dist
label var ccei_high "\$\text{CCEI}_{\text{high},gt}\$"
label var ccei_low "\$\text{CCEI}_{\text{low},gt}\$"

forvalues s = 1/4 {
    estimates restore est`s'
    local old_N`s' = e(N)
    local old_r2`s' = e(r2)
    local expected_high`s' = _b[ccei_max] + _b[ccei_dist]
    local expected_low`s' = -_b[ccei_dist]
}

eststo clear
forvalues s = 1/4 {
    local hl_controls ""
    if `s' >= 2 local hl_controls "$t5_group $t5_friend"
    if `s' >= 3 local hl_controls "`hl_controls' $t5_ra $t5_share"
    local hl_fe "class_fe"
    if `s' == 4 local hl_fe "pair_fe"

    eststo: reghdfe ccei_g ccei_high ccei_low `hl_controls', ///
        absorb(`hl_fe') vce(cluster class_fe)

    assert e(N) == `old_N`s''
    assert abs(e(r2) - `old_r2`s'') < 1e-6
    assert abs(_b[ccei_high] - `expected_high`s'') < 1e-6
    assert abs(_b[ccei_low] - `expected_low`s'') < 1e-6

    local hl_fe_label "Class"
    if `s' == 4 local hl_fe_label "Pair"
    local hl_student ""
    local hl_ra_share ""
    if `s' >= 2 local hl_student "\checkmark"
    if `s' >= 3 local hl_ra_share "\checkmark"
    estadd local fixed_effects "`hl_fe_label'"
    estadd local student_friend "`hl_student'"
    estadd local ra_controls "`hl_ra_share'"
    estadd local share_controls "`hl_ra_share'"
}

esttab using `"`tex_dir'/final_collective_ccei_high_low.tex"', replace ///
    b(3) se(3) ///
    stats(N r2 fixed_effects student_friend ra_controls share_controls, ///
        labels("N" "R-squared" "Fixed effects" ///
            "Student and friendship controls" "RA controls" ///
            "Corner/midpoint share controls") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(ccei_high ccei_low) order(ccei_high ccei_low) ///
    prefoot("\midrule") postfoot("\bottomrule") ///
    nomtitles fragment nonumbers nolines substitute(\_ _)

di as result "Additional output: `tex_dir'/final_collective_ccei_high_low.tex"

* CEI tables

label var cei_g "Collective Efficiency Index"
assert cei_full == (cei_g >= 1 - 1e-9)
assert ccei_full == (ccei_g == 1)
gen byte type_a = cei_type_a
gen byte type_b = cei_type_b
gen byte type_c = cei_type_c
gen byte type_d = cei_type_d
assert type_a + type_b + type_c + type_d == 1

label var type_a "Stable-weight rationalizable"
label var type_b "Varying-weight rationalizable"
label var type_c "Revealed Pareto inefficiency"
label var type_d "Other"

preserve
    contract post type_a type_b type_c type_d, freq(frequency)
    export delimited using `"`code_dir'/results/cei_classification_frequencies.csv"', replace
restore

local combined_file `"`tex_dir'/cei_four_outcomes_combined.tex"'
capture erase `"`combined_file'"'

foreach outcome in ccei_g cei_g type_a type_b type_c {
    eststo clear
    eststo m1: reghdfe `outcome' ccei_max ccei_dist, ///
        absorb(class_fe) vce(cluster class_fe)
    eststo m2: reghdfe `outcome' ccei_max ccei_dist $t5_group $t5_friend, ///
        absorb(class_fe) vce(cluster class_fe)
    eststo m3: reghdfe `outcome' ccei_max ccei_dist $t5_group $t5_friend $t5_ra $t5_share, ///
        absorb(class_fe) vce(cluster class_fe)
    eststo m4: reghdfe `outcome' ccei_max ccei_dist $t5_group $t5_friend $t5_ra $t5_share, ///
        absorb(pair_fe) vce(cluster class_fe)

    foreach model in m1 m2 m3 m4 {
        estimates restore `model'
        local fe_label "Class"
        if "`model'" == "m4" local fe_label "Pair"
        local student_controls ""
        local ra_controls ""
        local share_controls ""
        if inlist("`model'", "m2", "m3", "m4") local student_controls "\checkmark"
        if inlist("`model'", "m3", "m4") {
            local ra_controls "\checkmark"
            local share_controls "\checkmark"
        }
        estadd local fixed_effects "`fe_label'"
        estadd local student_friend "`student_controls'"
        estadd local ra_controls "`ra_controls'"
        estadd local share_controls "`share_controls'"
    }

    esttab m1 m2 m3 m4 using `"`tex_dir'/`outcome'_on_individual_ccei.tex"', replace ///
        b(3) se(3) ///
        stats(N r2 fixed_effects student_friend ra_controls share_controls, ///
            labels("N" "R-squared" "Fixed effects" ///
                "Student and friendship controls" "RA controls" ///
                "Corner/midpoint share controls") fmt(0 3)) ///
        nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
        keep(ccei_max ccei_dist) order(ccei_max ccei_dist) ///
        prefoot("\midrule") postfoot("\bottomrule") ///
        nomtitles fragment nonumbers nolines substitute(\_ _)

    if "`outcome'" != "ccei_g" {
        local panel_title ""
        local write_mode "append"
        local combined_footer ""
        if "`outcome'" == "cei_g" {
            local panel_title "Panel A: Collective Efficiency Index"
            local write_mode "replace"
        }
        if "`outcome'" == "type_a" local panel_title "Panel B: Stable-weight rationalizability"
        if "`outcome'" == "type_b" local panel_title "Panel C: Varying-weight rationalizability"
        if "`outcome'" == "type_c" {
            local panel_title "Panel D: Revealed Pareto inefficiency"
            local combined_footer `"\midrule Fixed effects & Class & Class & Class & Pair \\ Student and friendship controls & & \checkmark & \checkmark & \checkmark \\ RA controls & & & \checkmark & \checkmark \\ Corner/midpoint share controls & & & \checkmark & \checkmark \\ \bottomrule"'
        }

        esttab m1 m2 m3 m4 using `"`combined_file'"', `write_mode' ///
            b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
            nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
            keep(ccei_max ccei_dist) order(ccei_max ccei_dist) ///
            prehead("\multicolumn{5}{l}{\textit{`panel_title'}} \\ \midrule") ///
            prefoot("") postfoot(`"`combined_footer'"') ///
            nomtitles fragment nonumbers nolines substitute(\_ _)
    }
}

gen double ccei_min = ccei_max - ccei_dist
assert abs(ccei_min - min(ccei_i, ccei_j)) < 1e-7 ///
    if !missing(ccei_i, ccei_j)

assert inrange(ccei_g, 0, 1)
assert cei_full == (cei_g >= 1 - 1e-9)
assert ccei_full == (ccei_g >= 1 - 1e-9)

gen byte cei_case3 = 1 if cei_full == 0
replace cei_case3 = 2 if cei_full == 1 & ccei_full == 1
replace cei_case3 = 3 if cei_full == 1 & ccei_full == 0
label define cei_case3_lbl 1 "Pareto-inefficient" ///
    2 "Stable-weight" 3 "Varying-weight"
label values cei_case3 cei_case3_lbl
assert inlist(cei_case3, 1, 2, 3)
quietly count if cei_case3 == 1
assert r(N) == 411
quietly count if cei_case3 == 2
assert r(N) == 510
quietly count if cei_case3 == 3
assert r(N) == 383

label var ccei_max "\$\text{CCEI}_{\text{max},gt}\$"
label var ccei_min "\$\text{CCEI}_{\text{min},gt}\$"
label var ccei_dist "\$\text{CCEI}_{\text{dist},gt}\$"

* CEI robustness. Match Table 3 by excluding risk-aversion controls.

capture program drop fit_ols
program define fit_ols
    syntax varname, SECOND(varname) PREFIX(name)

    forvalues spec = 1/4 {
        local controls
        if `spec' >= 2 local controls "$t5_group $t5_friend"
        if `spec' >= 3 local controls "`controls' $t5_share"

        if `spec' <= 3 {
            quietly reghdfe `varlist' ccei_max `second' `controls', ///
                absorb(class_fe) vce(cluster class_fe)
        }
        else {
            quietly reghdfe `varlist' ccei_max `second' `controls', ///
                absorb(pair_fe) vce(cluster class_fe)
        }
        estimates store `prefix'`spec'
    }
end

capture program drop fit_fraclogit
program define fit_fraclogit, eclass
    syntax varname, SECOND(varname) PREFIX(name)

    forvalues spec = 1/3 {
        local controls
        if `spec' >= 2 local controls "$t5_group $t5_friend"
        if `spec' >= 3 local controls "`controls' $t5_share"

        quietly fracreg logit `varlist' c.ccei_max c.`second' ///
            `controls' i.class_fe, vce(cluster class_fe)
        local model_n = e(N)
        quietly margins, dydx(ccei_max `second') post
        ereturn scalar N_model = `model_n'
        estimates store `prefix'`spec'
    }
end

foreach second in ccei_min ccei_dist {
    if "`second'" == "ccei_min" local stem min
    else local stem dist

    fit_ols ccei_g, second(`second') prefix(ols_ccei_`stem')
    fit_ols cei_g, second(`second') prefix(ols_cei_`stem')
    fit_fraclogit ccei_g, second(`second') prefix(frac_ccei_`stem')
    fit_fraclogit cei_g, second(`second') prefix(frac_cei_`stem')
}

* Fractional-response models.
local pw_tv_controls mathscore_max mathscore_dist outgoing_max outgoing_dist ///
    opened_max opened_dist ///
    agreeable_max agreeable_dist conscientious_max conscientious_dist ///
    stable_max stable_dist mathscore_diff_missing outgoing_diff_missing ///
    opened_diff_missing agreeable_diff_missing conscientious_diff_missing ///
    stable_diff_missing inclass_n_friends_max inclass_n_friends_dist ///
    inclass_popularity_max inclass_popularity_dist friend ///
    corner_share_max corner_share_dist mid_share_max mid_share_dist
local pw_control_means
local j = 0
foreach x of local pw_tv_controls {
    local ++j
    bysort pair_fe: egen double pw_c`j' = mean(`x')
    local pw_control_means `pw_control_means' pw_c`j'
}

bysort pair_fe: egen double pw_mean_max = mean(ccei_max)
bysort pair_fe: egen double pw_mean_min = mean(ccei_min)
bysort pair_fe: egen double pw_mean_dist = mean(ccei_dist)

foreach second in ccei_min ccei_dist {
    if "`second'" == "ccei_min" {
        local stem min
        local focal_means pw_mean_max pw_mean_min
    }
    else {
        local stem dist
        local focal_means pw_mean_max pw_mean_dist
    }

    foreach outcome in ccei_g cei_g {
        if "`outcome'" == "ccei_g" local suffix ccei
        else local suffix cei

        quietly glm `outcome' c.ccei_max c.`second' ///
            $t5_group $t5_friend $t5_share ///
            `focal_means' `pw_control_means' i.post i.class_fe, ///
            family(binomial) link(probit) vce(cluster class_fe) nolog
        local model_n = e(N)
        quietly margins, dydx(ccei_max `second') post
        estadd scalar N_model = `model_n'
        estimates store pw_`suffix'_`stem'
    }
}

capture program drop export_table1
program define export_table1
    syntax, STEM(name) SECOND(varname) OUTfile(string) [OLSONLY]

    local ols_footer
    if "`olsonly'" != "" {
        local ols_footer "\midrule Fixed effects & Class & Class & Class & Pair \\ Student and friendship controls & & \checkmark & \checkmark & \checkmark \\ Corner/midpoint share controls & & & \checkmark & \checkmark \\ \bottomrule"
    }

esttab ols_ccei_`stem'1 ols_ccei_`stem'2 ols_ccei_`stem'3 ///
    ols_ccei_`stem'4 using `"`outfile'"', ///
    replace b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(ccei_max `second') order(ccei_max `second') ///
    fragment nomtitles nonumbers nolines substitute(\_ _) ///
    prehead("&\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}&\multicolumn{1}{c}{(4)}\\" ///
        "\midrule" ///
        "\multicolumn{5}{l}{\textit{Panel A: Group CCEI -- OLS}}\\") ///
    prefoot("\midrule") postfoot("")

esttab ols_cei_`stem'1 ols_cei_`stem'2 ols_cei_`stem'3 ///
    ols_cei_`stem'4 using `"`outfile'"', ///
    append b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(ccei_max `second') order(ccei_max `second') ///
    fragment nomtitles nonumbers nolines substitute(\_ _) ///
    prehead("\midrule" ///
        "\multicolumn{5}{l}{\textit{Panel B: Group CEI -- OLS}}\\") ///
    prefoot("\midrule") postfoot("`ols_footer'")

    if "`olsonly'" != "" exit

esttab frac_ccei_`stem'1 frac_ccei_`stem'2 frac_ccei_`stem'3 ///
    pw_ccei_`stem' using `"`outfile'"', ///
    append b(3) se(3) stats(N_model, labels("N") fmt(0)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(ccei_max `second') order(ccei_max `second') ///
    fragment nomtitles nonumbers nolines substitute(\_ _) ///
    prehead("\midrule" ///
        "\multicolumn{5}{l}{\textit{Panel C: Group CCEI -- fractional-response APEs}}\\") ///
    prefoot("\midrule") postfoot("")

esttab frac_cei_`stem'1 frac_cei_`stem'2 frac_cei_`stem'3 ///
    pw_cei_`stem' using `"`outfile'"', ///
    append b(3) se(3) stats(N_model, labels("N") fmt(0)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(ccei_max `second') order(ccei_max `second') ///
    fragment nomtitles nonumbers nolines substitute(\_ _) ///
    prehead("\midrule" ///
        "\multicolumn{5}{l}{\textit{Panel D: Group CEI -- fractional-response APEs}}\\") ///
    prefoot("\midrule") postfoot("\bottomrule")
end

local cei_robust_table_models_min `"`tex_dir'/cei_ccei_models_min.tex"'
local cei_robust_table_models_dist `"`tex_dir'/cei_ccei_models_dist.tex"'
export_table1, stem(min) second(ccei_min) outfile(`"`cei_robust_table_models_min'"')
export_table1, stem(dist) second(ccei_dist) outfile(`"`cei_robust_table_models_dist'"')
export_table1, stem(min) second(ccei_min) ///
    outfile(`"`tex_dir'/cei_ccei_ols_min.tex"') olsonly

* Multinomial logit.
gen double ccei_max_01 = 10 * ccei_max
gen double ccei_min_01 = 10 * ccei_min
gen double ccei_dist_01 = 10 * ccei_dist
label var ccei_max_01 "CCEI max (0.1)"
label var ccei_min_01 "CCEI min (0.1)"
label var ccei_dist_01 "CCEI distance (0.1)"

* Section 6.3: pooled four-category average marginal effects, no RA controls.
gen byte cei_case4 = 1 + ccei_full + 2 * cei_full
mlogit cei_case4 c.ccei_max_01 c.ccei_min_01 ///
    $t5_group $t5_friend $t5_share i.class_fe, ///
    baseoutcome(1) vce(cluster class_fe)
assert e(converged) == 1
assert e(N) == _N
local joint_n = e(N)
local joint_clusters = e(N_clust)
isid group_id post
bysort group_id: assert _N == 2
local joint_pairs = `joint_n' / 2
tempname ame_handle
tempfile pooled_ames
postfile `ame_handle' byte outcome str7 member ///
    double estimate se low high share long n pairs clusters using `pooled_ames'
forvalues outcome = 1/4 {
    quietly count if e(sample) & cei_case4 == `outcome'
    local share = r(N) / `joint_n'
    margins, dydx(ccei_max_01 ccei_min_01) predict(outcome(`outcome'))
    matrix ame = r(table)
    forvalues col = 1/2 {
        local member = cond(`col' == 1, "maximum", "minimum")
        post `ame_handle' (`outcome') ("`member'") ///
            (ame[1,`col']) (ame[2,`col']) (ame[5,`col']) (ame[6,`col']) ///
            (`share') (`joint_n') (`joint_pairs') (`joint_clusters')
    }
}
postclose `ame_handle'
preserve
    use `pooled_ames', clear
    bysort member: egen double ame_sum = total(estimate)
    assert abs(ame_sum) < 1e-8
    assert !missing(estimate, se, low, high)
    drop ame_sum
    export delimited using `"`code_dir'/results/cei_joint_ame_pooled.csv"', replace
restore

foreach stem in min dist {
    local second01 ccei_`stem'_01

    forvalues spec = 1/3 {
        local controls
        if `spec' >= 2 local controls "$t5_group $t5_friend"
        if `spec' >= 3 local controls "`controls' $t5_share"

        quietly mlogit cei_case3 c.ccei_max_01 c.`second01' ///
            `controls' i.class_fe, baseoutcome(1) vce(cluster class_fe)
        assert e(converged) == 1
        estadd scalar N_pairs = e(N) / 2
        estimates store joint_ml_`stem'`spec'
    }
}

* Pair fixed effects.
xtset pair_fe post
bysort pair_fe (post): gen byte ml_switch = cei_case3[1] != cei_case3[2]
bysort pair_fe (post): gen double d_mid_max = ///
    mid_share_max[2] - mid_share_max[1]
bysort pair_fe (post): gen double d_mid_dist = ///
    mid_share_dist[2] - mid_share_dist[1]
quietly regress d_mid_max d_mid_dist if post == 1 & ml_switch, noconstant
local gamma = _b[d_mid_dist]
gen double mid_share_orth = mid_share_max - `gamma' * mid_share_dist
quietly summarize mid_share_orth
replace mid_share_orth = mid_share_orth / r(sd)

quietly xtmlogit cei_case3 c.ccei_max_01 c.ccei_min_01 ///
    $t5_group $t5_friend corner_share_max corner_share_dist ///
    mid_share_orth mid_share_dist, fe baseoutcome(1) ///
    vce(cluster class_fe)
assert e(converged) == 1
local conditional_n = e(N)
local conditional_pairs = e(N_g)
local conditional_r2 = 1 - e(ll) / e(ll_0)
estadd scalar r2_p = `conditional_r2'
estadd scalar N_pairs = `conditional_pairs'
estimates store joint_ml_min4

* The distance model is an exact reparameterization of the minimum model.
* Transform the stable fit instead of refitting the numerically fragile model.
quietly nlcom ///
    (stable_max: _b[Stable_weight:ccei_max_01] + ///
        _b[Stable_weight:ccei_min_01]) ///
    (stable_dist: -_b[Stable_weight:ccei_min_01]) ///
    (varying_max: _b[Varying_weight:ccei_max_01] + ///
        _b[Varying_weight:ccei_min_01]) ///
    (varying_dist: -_b[Varying_weight:ccei_min_01]), post
matrix dist_b = J(1, 2, 0), e(b)
matrix dist_V = (J(2, 2, 0), J(2, 4, 0) \ J(4, 2, 0), e(V))
matrix coleq dist_b = Pareto_inefficient Pareto_inefficient ///
    Stable_weight Stable_weight Varying_weight Varying_weight
matrix colnames dist_b = o.ccei_max_01 o.ccei_dist_01 ///
    ccei_max_01 ccei_dist_01 ccei_max_01 ccei_dist_01
matrix roweq dist_V = Pareto_inefficient Pareto_inefficient ///
    Stable_weight Stable_weight Varying_weight Varying_weight
matrix coleq dist_V = Pareto_inefficient Pareto_inefficient ///
    Stable_weight Stable_weight Varying_weight Varying_weight
matrix rownames dist_V = o.ccei_max_01 o.ccei_dist_01 ///
    ccei_max_01 ccei_dist_01 ccei_max_01 ccei_dist_01
matrix colnames dist_V = o.ccei_max_01 o.ccei_dist_01 ///
    ccei_max_01 ccei_dist_01 ccei_max_01 ccei_dist_01
capture program drop post_dist_mlogit
program define post_dist_mlogit, eclass
    syntax, B(name) V(name) N(integer)
    ereturn post `b' `v', obs(`n')
    ereturn local cmd "post_dist_mlogit"
    ereturn local eqnames "Pareto_inefficient Stable_weight Varying_weight"
    ereturn scalar k_eq = 3
end
post_dist_mlogit, b(dist_b) v(dist_V) n(`conditional_n')
estadd scalar r2_p = `conditional_r2'
estadd scalar N_pairs = `conditional_pairs'
estimates store joint_ml_dist4

capture program drop export_table2
program define export_table2
    syntax, STEM(name) SECOND(name) OUTfile(string)
    local second01 ccei_`second'_01

esttab joint_ml_`stem'1 joint_ml_`stem'2 joint_ml_`stem'3 ///
    joint_ml_`stem'4 using `"`outfile'"', ///
    replace b(3) se(3) ///
    stats(N N_pairs r2_p, ///
        labels("Pair-wave observations" "Pairs" "Pseudo R-squared") ///
        fmt(0 0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(Stable_weight:ccei_max_01 Stable_weight:`second01' ///
        Varying_weight:ccei_max_01 Varying_weight:`second01') ///
    order(ccei_max_01 `second01') ///
    eqlabels("Stable-weight vs. Pareto inefficient" ///
        "Varying-weight vs. Pareto inefficient") ///
    prehead("&\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}&\multicolumn{1}{c}{(4)}\\" ///
        "\midrule") ///
    prefoot("\midrule") postfoot("\bottomrule") ///
    fragment nomtitles nonumbers nolines
end

local cei_robust_table_mlogit_min `"`tex_dir'/cei_joint_mlogit_min.tex"'
local cei_robust_table_mlogit_dist `"`tex_dir'/cei_joint_mlogit_dist.tex"'
export_table2, stem(min) second(min) outfile(`"`cei_robust_table_mlogit_min'"')
export_table2, stem(dist) second(dist) outfile(`"`cei_robust_table_mlogit_dist'"')

di as result "Created: `cei_robust_table_models_min'"
di as result "Created: `cei_robust_table_models_dist'"
di as result "Created: `cei_robust_table_mlogit_min'"
di as result "Created: `cei_robust_table_mlogit_dist'"

