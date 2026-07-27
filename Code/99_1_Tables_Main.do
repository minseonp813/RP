********************************************************************************
* 99_Tables_Main.do
* Main-paper tables only. Existing 99_* files are intentionally left unchanged.
*
* Run this file from C:/Users/hahn0/RP/Code.
********************************************************************************

version 17
clear all
set more off
set matsize 8000

local code_dir `"`c(pwd)'"'

* Minseon/Dropbox version (kept for reference):
* local replication_dir `"C:/Users/minseonp/Dropbox/RP/Code_Replication Package_Upload"'
* local tex_dir `"C:/Users/minseonp/Dropbox/OverleafGit/Group Decision/tables_2025"'

* Byunghun/current repository:
local replication_dir `"`code_dir'/../Code_Replication Package_Upload"'
local data_dir `"`replication_dir'/data"'
local tex_dir `"`code_dir'/results"'

cap mkdir `"`tex_dir'"'

foreach required in panel_individual.dta panel_group.dta base_raw.dta end_raw.dta {
    capture confirm file `"`data_dir'/`required'"'
    if _rc {
        di as error "Missing required input: `data_dir'/`required'"
        exit 601
    }
}

capture which reghdfe
if _rc {
    di as error "reghdfe is required. Install it with: ssc install reghdfe"
    exit 199
}
capture which esttab
if _rc {
    di as error "estout is required. Install it with: ssc install estout"
    exit 199
}

********************************************************************************
* Table 1: Summary Statistics Of Experimental Measures
********************************************************************************

use `"`data_dir'/panel_individual.dta"', clear

foreach period in 0 1 {
    local suffix = cond(`period' == 0, "base", "end")
    foreach var in ccei_i I_ig RA_i {
        quietly summarize `var' if post == `period', detail
        local `var'_`suffix'_mean = r(mean)
        local `var'_`suffix'_sd   = r(sd)
        local `var'_`suffix'_p10  = r(p10)
        local `var'_`suffix'_p50  = r(p50)
        local `var'_`suffix'_p90  = r(p90)
        local `var'_`suffix'_N    = r(N)
    }
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
file write summary_file "& (1) & (2) & (3) & (4) & (5) & (6) `bs'`bs'" _n
file write summary_file " & Mean & SD & p10 & p50 & p90 & N `bs'`bs'" _n
file write summary_file "`bs'midrule" _n
file write summary_file "`bs'multicolumn{7}{l}{`bs'emph{Panel A: Baseline}} `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual risk aversion & " %6.3f (`RA_i_base_mean') " & " %6.3f (`RA_i_base_sd') " & " %6.3f (`RA_i_base_p10') " & " %6.3f (`RA_i_base_p50') " & " %6.3f (`RA_i_base_p90') " & " %9.0fc (`RA_i_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group risk aversion & " %6.3f (`RA_g_base_mean') " & " %6.3f (`RA_g_base_sd') " & " %6.3f (`RA_g_base_p10') " & " %6.3f (`RA_g_base_p50') " & " %6.3f (`RA_g_base_p90') " & " %9.0fc (`RA_g_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual CCEI & " %6.3f (`ccei_i_base_mean') " & " %6.3f (`ccei_i_base_sd') " & " %6.3f (`ccei_i_base_p10') " & " %6.3f (`ccei_i_base_p50') " & " %6.3f (`ccei_i_base_p90') " & " %9.0fc (`ccei_i_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group CCEI & " %6.3f (`ccei_g_base_mean') " & " %6.3f (`ccei_g_base_sd') " & " %6.3f (`ccei_g_base_p10') " & " %6.3f (`ccei_g_base_p50') " & " %6.3f (`ccei_g_base_p90') " & " %9.0fc (`ccei_g_base_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Preference distance `bs'ensuremath{I_{ig}} & " %6.3f (`I_ig_base_mean') " & " %6.3f (`I_ig_base_sd') " & " %6.3f (`I_ig_base_p10') " & " %6.3f (`I_ig_base_p50') " & " %6.3f (`I_ig_base_p90') " & " %9.0fc (`I_ig_base_N') " `bs'`bs'" _n
file write summary_file "`bs'addlinespace" _n
file write summary_file "`bs'multicolumn{7}{l}{`bs'emph{Panel B: Endline}} `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual risk aversion & " %6.3f (`RA_i_end_mean') " & " %6.3f (`RA_i_end_sd') " & " %6.3f (`RA_i_end_p10') " & " %6.3f (`RA_i_end_p50') " & " %6.3f (`RA_i_end_p90') " & " %9.0fc (`RA_i_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group risk aversion & " %6.3f (`RA_g_end_mean') " & " %6.3f (`RA_g_end_sd') " & " %6.3f (`RA_g_end_p10') " & " %6.3f (`RA_g_end_p50') " & " %6.3f (`RA_g_end_p90') " & " %9.0fc (`RA_g_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Individual CCEI & " %6.3f (`ccei_i_end_mean') " & " %6.3f (`ccei_i_end_sd') " & " %6.3f (`ccei_i_end_p10') " & " %6.3f (`ccei_i_end_p50') " & " %6.3f (`ccei_i_end_p90') " & " %9.0fc (`ccei_i_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Group CCEI & " %6.3f (`ccei_g_end_mean') " & " %6.3f (`ccei_g_end_sd') " & " %6.3f (`ccei_g_end_p10') " & " %6.3f (`ccei_g_end_p50') " & " %6.3f (`ccei_g_end_p90') " & " %9.0fc (`ccei_g_end_N') " `bs'`bs'" _n
file write summary_file "`bs'hspace{1em}Preference distance `bs'ensuremath{I_{ig}} & " %6.3f (`I_ig_end_mean') " & " %6.3f (`I_ig_end_sd') " & " %6.3f (`I_ig_end_p10') " & " %6.3f (`I_ig_end_p50') " & " %6.3f (`I_ig_end_p90') " & " %9.0fc (`I_ig_end_N') " `bs'`bs'" _n
file write summary_file "`bs'bottomrule" _n
file close summary_file

********************************************************************************
* Table 2: Sample Attrition And Randomization Test
********************************************************************************

* Everything in Table 2 is generated here. The random member draw is created
* only in memory and never added to a cleaned dataset.
tempfile t2_baseline t2_analysis_ids t2_analysis t2_male t2_height
tempfile t2_math t2_noncog t2_network_out t2_network_in t2_edges
tempfile t2_randomization

* Baseline experimental measures and the universe of baseline participants.
use `"`data_dir'/riskpreference_pre.dta"', clear
gen double t2_expensive = cond(intercept_x < intercept_y, coord_x, coord_y)
gen double risk_aversion = t2_expensive / (coord_x + coord_y) ///
    if inrange(round_number, 1, 18) & coord_x + coord_y != 0
collapse (mean) ccei=ccei_ind risk_aversion, by(id)
isid id
save `t2_baseline'

* Gender.
use `"`data_dir'/male.dta"', clear
keep id male
replace male = 0 if inlist(male, 0, 2)
replace male = 1 if male == 1
bysort id: keep if _n == 1
isid id
save `t2_male'

* Height.
use `"`data_dir'/height.dta"', clear
keep id height
bysort id: keep if _n == 1
isid id
save `t2_height'

* Baseline math score. Letter-prefixed labels are converted to the numeric IDs
* used in the experimental files.
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

* Baseline Big Five measures.
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

* Baseline within-class out-degree and in-degree.
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

* Assemble the 1,572-person baseline sample.
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

* Assemble the 1,304-person baseline analysis sample, restoring genuine
* missing values that were imputed in the replication panel.
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

* Attrition means, differences, and Welch two-sample t-tests.
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

* Joint attrition test.
use `t2_baseline', clear
merge 1:1 id using `t2_analysis_ids', nogen keep(master match)
replace in_analysis = 0 if missing(in_analysis)
quietly regress in_analysis `t2_vars'
quietly test `t2_vars'
local t2_attrition_F = r(F)
local t2_attrition_df = r(df)
local t2_attrition_df_r = r(df_r)
local t2_attrition_joint_p = r(p)

* Randomly orient the 652 pairs. The draw is recreated every run and discarded.
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

* Twelve partner-characteristic regressions with class fixed effects.
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

* Write the Table 2 tabular fragment. The paper supplies the surrounding
* table, tabular, caption, label, and notes environments.
local bs = char(92)
file open t2_file using `"`tex_dir'/table_attrition_balance.tex"', write replace
file write t2_file "& (1) & (2) & (3) & (4) & (5) & (6) `bs'`bs'" _n
file write t2_file "& `bs'multicolumn{4}{c}{Sample Attrition} & `bs'multicolumn{2}{c}{Randomization Test} `bs'`bs'" _n
file write t2_file "`bs'cmidrule(lr){2-5}`bs'cmidrule(lr){6-7}" _n
file write t2_file "& `bs'multicolumn{2}{c}{Sample} & & & & `bs'`bs' `bs'cmidrule(lr){2-3}" _n
file write t2_file "& Baseline & Analysis & Diff. & `bs'ensuremath{p}-value & `bs'ensuremath{`bs'beta} & `bs'ensuremath{p}-value `bs'`bs' `bs'midrule" _n
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
        file write t2_file "`t2_label' & " %6.1f (`t2_base_mean_`t2_k'') " & " %6.1f (`t2_analysis_mean_`t2_k'') " & " %6.3f (`t2_diff_`t2_k'') " & " %6.3f (`t2_attrition_p_`t2_k'') " & " %6.3f (`t2_random_beta_`t2_k'') " & " %6.3f (`t2_random_p_`t2_k'') " `bs'`bs'" _n
    }
    else {
        file write t2_file "`t2_label' & " %6.3f (`t2_base_mean_`t2_k'') " & " %6.3f (`t2_analysis_mean_`t2_k'') " & " %6.3f (`t2_diff_`t2_k'') " & " %6.3f (`t2_attrition_p_`t2_k'') " & " %6.3f (`t2_random_beta_`t2_k'') " & " %6.3f (`t2_random_p_`t2_k'') " `bs'`bs'" _n
    }
}

file write t2_file "`bs'midrule" _n
file write t2_file "Joint test: & `bs'multicolumn{4}{l}{`bs'ensuremath{F(" %3.0f (`t2_attrition_df') "," %6.0f (`t2_attrition_df_r') ") = " %6.2f (`t2_attrition_F') "}, `bs'ensuremath{p = " %6.3f (`t2_attrition_joint_p') "}} & `bs'multicolumn{2}{l}{`bs'ensuremath{`bs'chi^{2}(" %3.0f (`t2_random_df') ") = " %6.2f (`t2_random_chi2') "}, `bs'ensuremath{p = " %6.3f (`t2_random_joint_p') "}} `bs'`bs'" _n
file write t2_file "N & 1,572 & 1,304 & & & `bs'multicolumn{2}{c}{652} `bs'`bs'" _n
file write t2_file "`bs'bottomrule" _n
file close t2_file
estimates clear

********************************************************************************
* Shared preparation for Tables 3 and 4
********************************************************************************

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

bysort id: egen n_I_ig = total(!missing(I_ig))
gen byte balanced_t3 = (n_I_ig == 2)
drop n_I_ig
egen long id_fe = group(id)

label var HighCCEI "\$Higher CCEI_i\$"
label var mathscore_i "\$Math score_i\$"
label var mathscore_diff "\$Math score_{diff}\$"
label var inclass_popularity_i "\$In-degree_i\$"
label var inclass_pop_diff "\$In-degree_{diff}\$"
label var female_i_male_j "\$(Female_i, Male_j)\$"
label var male_i_female_j "\$(Male_i, Female_j)\$"

********************************************************************************
* Table 3: Individual Rationality And Revealed Influence On Group Choices
********************************************************************************

eststo clear
eststo: reghdfe I_ig HighCCEI if balanced_t3, absorb(class) vce(cluster class)
eststo: reghdfe I_ig HighCCEI $t34_group $t34_friend $t34_missing if balanced_t3, absorb(class) vce(cluster class)
eststo: reghdfe I_ig HighCCEI $t34_group $t34_friend $t34_missing $t34_ra $t34_share if balanced_t3, absorb(class) vce(cluster class)
eststo: reghdfe I_ig HighCCEI $t34_group_nogender $t34_friend $t34_missing $t34_ra $t34_share if balanced_t3, absorb(id_fe) vce(cluster class)
esttab using `"`tex_dir'/table_bargainingCCEI.tex"', replace ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label substitute(\_ _) ///
    keep(HighCCEI mathscore_i mathscore_diff inclass_popularity_i inclass_pop_diff female_i_male_j male_i_female_j) ///
    nomtitles fragment nonumbers nolines prefoot("\hline") postfoot("\bottomrule")

********************************************************************************
* Table 4: Preference Aggregation Using d(RA_i, RA_g)
********************************************************************************

capture drop RA_distance_denom RA_I_ig
gen RA_distance_denom = (RA_i - RA_g)^2 + (RA_j - RA_g)^2
gen RA_I_ig = (RA_i - RA_g)^2 / RA_distance_denom
replace RA_I_ig = . if RA_distance_denom == 0

eststo clear
eststo: reghdfe RA_I_ig HighCCEI, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI $t34_group $t34_friend $t34_missing, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI $t34_group $t34_friend $t34_missing $t34_ra $t34_share, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI $t34_group_nogender $t34_friend $t34_missing $t34_ra $t34_share, absorb(id_fe) vce(cluster class) keepsingletons
esttab using `"`tex_dir'/table_bargainingRA_distance.tex"', replace ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label substitute(\_ _) ///
    keep(HighCCEI mathscore_i mathscore_diff inclass_popularity_i inclass_pop_diff female_i_male_j male_i_female_j) ///
    nomtitles fragment nonumbers nolines prefoot("\hline") postfoot("\bottomrule")

********************************************************************************
* Table 5: Collective CCEI
********************************************************************************

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

di as result "99_1_Tables_Main.do completed. Outputs: `tex_dir'"
