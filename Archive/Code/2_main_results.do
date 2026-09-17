********************************************************************************
* Generate Descriptive Statistics 
* Written by Minseon Park 09012025
* Latest updated by Minseon Park 12272025
********************************************************************************

set more off
set matsize 8000

* Add path to ado files
adopath + "`c(pwd)'"
cd "~/Dropbox/RP/RA_Byunghun"


********************************************************************************
* Table: Bargaining using CCEI
********************************************************************************

use "data/finalized_panel_individual_251206.dta", clear

tempfile choice_shares partner_choice_shares
preserve
    use "data/for_graphs_baseline_raw.dta", clear
    keep if game_type == 1
    gen post = 0
    gen corner_share = (coord_x == 0 | coord_y == 0)
    gen mid_share = (coord_x == coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    tempfile baseline_choice_shares
    save `baseline_choice_shares'

    use "data/for_graphs_endline_raw.dta", clear
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

gen corner_share_diff = corner_share_i - corner_share_j
gen mid_share_diff = mid_share_i - mid_share_j

gen female_i_male_j = (male_i == 0 & male_j == 1)
gen male_i_female_j = (male_i == 1 & male_j == 0)

* Imputation
foreach v in mathscore_i outgoing_i opened_i agreeable_i conscientious_i stable_i {
    replace `v' = 0 if missing(`v')
}

* when choosing covarariates, don't include things like within-pair friendship. 
* those only matters when interacted with CCEI variable. otherwise, given that 
* bargaining index sums up to 1 within pair, those can't be significant
global group_char = "mathscore_i math_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff female_i_male_j male_i_female_j"
global group_char_no_gender = "mathscore_i math_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
global friend_char = "inclass_n_friends inclass_n_diff inclass_popularity inclass_pop_diff" 
global missing_char = "mathscore_dist_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing "
global RA_char = "RA_i RA_diff"
global share_char = "corner_share_i corner_share_diff mid_share_i mid_share_diff"

reghdfe new2_I_ig HighCCEI $group_char $friend_char $missing_char, absorb(class) vce(cluster class)
gen cluster_class = e(sample)

reghdfe new2_I_ig HighCCEI $group_char_no_gender $friend_char $missing_char, absorb(id) vce(cluster class)
gen cluster_id = e(sample)

* keep if cluster_class == 0
gen balanced = cluster_id
sort id group_id post

g RA_diff = RA_i-RA_j

label var RA_diff "Diff in RA"
label var math_diff "Diff in Math"
label var inclass_popularity "In-degree"
label var inclass_pop_diff "Diff in In-degree"
label var corner_share_i "Corner Share"
label var corner_share_diff "Diff in Corner Share"
label var mid_share_i "Midpoint Share"
label var mid_share_diff "Diff in Midpoint Share"
label var female_i_male_j "(Female_i, Male_j)"
label var male_i_female_j "(Male_i, Female_j)"
label var HighCCEI_post "High CCEI*Endline" // post is not right since we're assuming there was no intervention
label var RA_i "Risk Attitude"


** Main bargaining-index regressions; Shapley decomposition is reported in the appendix
eststo clear
eststo: reghdfe new2_I_ig HighCCEI if balanced==1, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI $group_char $friend_char $missing_char if balanced==1, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI $group_char $friend_char $missing_char $RA_char $share_char if balanced==1, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI $group_char_no_gender $friend_char $missing_char $RA_char $share_char if balanced==1, absorb(id) vce(cluster class)
esttab , b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01) drop(*missing*) label
esttab using "Tables/table_bargainingCCEI.tex", replace ///
	b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
	nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
	keep(HighCCEI mathscore_i math_diff inclass_popularity inclass_pop_diff female_i_male_j male_i_female_j) ///
	nomtitles fragment nonumbers nolines


********************************************************************************
* Table: Bargaining using normalized RA distance
********************************************************************************

g RA_distance_denom = (RA_i-RA_g)^2 + (RA_j-RA_g)^2
g RA_I_ig = (RA_i-RA_g)^2/RA_distance_denom
replace RA_I_ig = . if RA_distance_denom == 0
label var RA_I_ig "RA distance"
eststo clear
eststo: reghdfe RA_I_ig HighCCEI if balanced==1, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI $group_char $friend_char $missing_char if balanced==1, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI $group_char $friend_char $missing_char $RA_char $share_char if balanced==1, absorb(class) vce(cluster class)
eststo: reghdfe RA_I_ig HighCCEI $group_char $friend_char $missing_char $RA_char $share_char if balanced==1, absorb(id) vce(cluster class)
esttab , b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01) drop(*missing*) label
esttab using "Tables/table_bargainingRA_distance.tex", replace ///
	b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
	nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
	keep(HighCCEI mathscore_i math_diff inclass_popularity inclass_pop_diff female_i_male_j male_i_female_j) ///
	nomtitles fragment nonumbers nolines


use "data/finalized_panel_pbl_250831.dta", clear

g RA_highCCEI = cond(ccei_1>=ccei_2,RA_1,RA_2)
g RA_lowCCEI = cond(ccei_1<ccei_2,RA_1,RA_2)

gen RA_highMATH     = cond(mathscore_1 >= mathscore_2, RA_1, RA_2)
gen RA_lowMATH      = cond(mathscore_1 <  mathscore_2, RA_1, RA_2)

gen RA_highFRIENDS  = cond(inclass_n_friends_1 >= inclass_n_friends_2, RA_1, RA_2)
gen RA_highPOP      = cond(inclass_popularity_1 >= inclass_popularity_2, RA_1, RA_2)
gen RA_highHEIGHT   = cond(height >= height2, RA_1, RA_2)
gen RA_highMALE     = cond(male >= male2, RA_1, RA_2)
gen RA_highAGREE    = cond(agreeable >= agreeable2, RA_1, RA_2)
gen RA_highCONS     = cond(conscientious >= conscientious2, RA_1, RA_2)
gen RA_highOPEN     = cond(opened >= opened2, RA_1, RA_2)
gen RA_highOUT      = cond(outgoing >= outgoing2, RA_1, RA_2)
gen RA_highSTAB     = cond(stable >= stable2, RA_1, RA_2)

gen RA_highRA     = cond(RA_1 >= RA_2, RA_1, RA_2)

global RA_obs = "RA_highMATH RA_lowMATH RA_highHEIGHT RA_highMALE RA_highFRIENDS RA_highPOP  RA_highAGREE RA_highCONS RA_highOPEN RA_highOUT RA_highSTAB"

eststo clear
eststo: reghdfe RA_g RA_highCCEI RA_lowCCEI, absorb(class endline ) vce(cluster class)
test RA_highCCEI=RA_lowCCEI
estadd scalar p_val = r(p) 
eststo: reghdfe RA_g RA_highCCEI RA_lowCCEI $RA_obs, absorb(class endline ) vce(cluster class)
test RA_highCCEI=RA_lowCCEI
estadd scalar p_val = r(p) 

eststo: reghdfe RA_g RA_highCCEI RA_lowCCEI $RA_obs RA_highRA, absorb(class endline ) vce(cluster class)
test RA_highCCEI=RA_lowCCEI
estadd scalar p_val = r(p) 

eststo: reghdfe RA_g RA_highCCEI RA_lowCCEI endline#c.RA_highCCEI $RA_obs RA_highRA endline , absorb(class endline) vce(cluster class)
test RA_highCCEI=RA_lowCCEI
estadd scalar p_val = r(p) 
test RA_highCCEI + 1.endline#c.RA_highCCEI = RA_lowCCEI+ endline 
estadd scalar p_val1 = r(p)
eststo: reghdfe RA_g RA_highCCEI RA_lowCCEI $RA_obs RA_highRA, absorb(group_id ) vce(cluster class)
test RA_highCCEI=RA_lowCCEI
estadd scalar p_val = r(p) 
esttab, star(+ 0.1 * 0.05 ** 0.01) b(3) se(3) stats(N r2 p_val p_val1, labels("N" "R-squared" "p-value") fmt(0 3 3))
esttab using "results/table_bargainingRA.csv", replace b(3) se(3) stats(N r2 p_val p_val1, labels("N" "R-squared" "p-value") fmt(0 3 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01) 



********************************************************************************
* Table: Collective CCEI
********************************************************************************

use "data/finalized_panel_pbl_251206.dta", clear

g male_diff = cond(male~=male2,1,0)
g friend = cond(friendship>=1,1,0)

global group_char = "mathscore_max mathscore_dist mathscore_max_missing mathscore_dist_missing height_max height_dist male_diff	outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_dist conscientious_max stable_max stable_dist big5_max_missing big5_dist_missing"
global friend_char = "inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist friend" 
global RA_char "RA_max RA_dist"

label var RA_dist "$\text{RA}_{\text{dist},gt}$"
label var RA_max "$\text{RA}_{\text{max},gt}$"

la var ccei_max "$\text{CCEI}_{\text{max},gt}$"
la var ccei_dist "$\text{CCEI}_{\text{dist},gt}$"
la var mathscore_max "$\text{Math Score}_{\text{max},gt}$"
la var mathscore_dist "$\text{Math Score}_{\text{dist},gt}$"
la var end_max "Max CCEI*Endline"
la var end_dist "Diff in CCEI*Endline"
la var male_diff "Different gender"
la var friend "Friendship tie"

** Group-CCEI regressions
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

esttab using "Tables/table_groupCCEI.tex", replace ///
	b(3) se(3) stats(student_controls ra_controls class_fe pair_fe N r2, labels("Student and friendship controls" "RA controls" "Class fixed effects" "Pair fixed effects" "N" "R-squared") fmt(%9s %9s %9s %9s 0 3)) ///
	nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
	keep(ccei_max ccei_dist) ///
	nomtitles fragment nonumbers nolines substitute(\_ _)
	
	
********************************************************************************
* Table: Bargaining using CCEI - Mechanism
********************************************************************************

use "data/finalized_panel_individual_250831.dta", clear

ds peer*
local varlist = r(varlist)
foreach y in `varlist' {
	recode `y' (0=.) (5=.)
}

local tab_vars value* pbl* teacher* brothers* sisters*
foreach y of varlist `tab_vars' {
    summarize `y', detail
    di "`y' median = " r(p50)
}


* higher CCEI - easier to communicate what you want?
* maybe it's easier to say what you want when you know what you want
	* covariates that could mediate the "easiness of communication"
	* not a direct evidence, but quite informative

* Friend
eststo: reghdfe new2_I_ig HighCCEI if friendship~=2, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI if friendship==2, absorb(class) vce(cluster class)

* Male
eststo: reghdfe new2_I_ig HighCCEI if male_i~=male_j, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI if male_i==male_j, absorb(class) vce(cluster class)

* Outgoing
eststo: reghdfe new2_I_ig HighCCEI if outgoing_i<=3.5 | outgoing_j<=3.5, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI if outgoing_i>3.5 & outgoing_j>3.5, absorb(class) vce(cluster class)

* Value Trust
eststo: reghdfe new2_I_ig HighCCEI if value_trust_i<=2 | value_trust_j<=2, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI if value_trust_i>2 & value_trust_j>2, absorb(class) vce(cluster class)

* Value Cowork
eststo: reghdfe new2_I_ig HighCCEI if value_cowork_i<=3 | value_cowork_j<=3, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI if value_cowork_i>3 & value_cowork_j>3, absorb(class) vce(cluster class)

* PBL Effect: Share Idea
eststo: reghdfe new2_I_ig HighCCEI if pbleffect_sharidea_i<=3 | pbleffect_sharidea_j<=3, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI if pbleffect_sharidea_i>3 & pbleffect_sharidea_j>3, absorb(class) vce(cluster class)

* Teacher Induce
eststo: reghdfe new2_I_ig HighCCEI if teacher_induce_i<=3 | teacher_induce_j<=3, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI if teacher_induce_i>3 & teacher_induce_j>3, absorb(class) vce(cluster class)

* Class Participate
eststo: reghdfe new2_I_ig HighCCEI if class_participate_i<=3 | class_participate_j<=3, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI if class_participate_i>3 & class_participate_j>3, absorb(class) vce(cluster class)

esttab
esttab using "results/table_bargainingCCEI_mech.csv", scsv replace ///
    cells(b(fmt(3)) se(fmt(3) par) ci(fmt(3) par)) ///
    stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    mtitles( ///
        "Friend~=2" "Friend=2" ///
        "Male~=Male" "Male=Male" ///
        "Outgoing Low" "Outgoing High" ///
        "Valuetrust Low" "Valuetrust High" ///
        "Valuecowork Low" "Valuecowork High" ///
        "Pbleffectsharidea Low" "Pbleffectsharidea High" ///
        "Teacherinduce Low" "Teacherinduce High" ///
        "Classparticipate Low" "Classparticipate High" ///
    ) ///
    nogap compress nonumber collab(none) ///
    addnote("Standard errors in parentheses." "Confidence intervals in brackets.") ///
    star(+ 0.1 * 0.05 ** 0.01)


	
**************************************************

**# Bookmark #2

use "data/finalized_panel_individual_250831.dta", clear

log using suest, replace t

eststo clear

* Friend
qui reg new2_I_ig HighCCEI ib1.class if friendship~=2
estimates store friend_not_2
qui reg new2_I_ig HighCCEI ib1.class if friendship==2
estimates store friend_is_2
qui suest friend_not_2 friend_is_2, vce(cluster class)
test [friend_not_2_mean]HighCCEI = [friend_is_2_mean]HighCCEI

* Male
qui reg new2_I_ig HighCCEI ib1.class if male_i~=male_j
estimates store male_not_same
qui reg new2_I_ig HighCCEI ib1.class if male_i==male_j
estimates store male_same
qui suest male_not_same male_same, vce(cluster class)
test [male_not_same_mean]HighCCEI = [male_same_mean]HighCCEI

* Outgoing
qui reg new2_I_ig HighCCEI ib1.class if outgoing_i<=3.5 | outgoing_j<=3.5
estimates store outgoing_low
qui reg new2_I_ig HighCCEI ib1.class if outgoing_i>3.5 & outgoing_j>3.5
estimates store outgoing_high
qui suest outgoing_low outgoing_high, vce(cluster class)
test [outgoing_low_mean]HighCCEI = [outgoing_high_mean]HighCCEI

* Open
qui reg new2_I_ig HighCCEI ib1.class if opened_i<=3.5 | opened_j<=3.5
estimates store open_low
qui reg new2_I_ig HighCCEI ib1.class if opened_i>3.5 & opened_j>3.5
estimates store open_high
qui suest open_low open_high, vce(cluster class)
test [open_low_mean]HighCCEI = [open_high_mean]HighCCEI

* Value Trust
qui reg new2_I_ig HighCCEI ib1.class if value_trust_i<=2 | value_trust_j<=2
estimates store trust_low
qui reg new2_I_ig HighCCEI ib1.class if value_trust_i>2 & value_trust_j>2
estimates store trust_high
qui suest trust_low trust_high, vce(cluster class)
test [trust_low_mean]HighCCEI = [trust_high_mean]HighCCEI

* Value Cowork
qui reg new2_I_ig HighCCEI ib1.class if value_cowork_i<=3 | value_cowork_j<=3
estimates store cowork_low
qui reg new2_I_ig HighCCEI ib1.class if value_cowork_i>3 & value_cowork_j>3
estimates store cowork_high
qui suest cowork_low cowork_high, vce(cluster class)
test [cowork_low_mean]HighCCEI = [cowork_high_mean]HighCCEI

* PBL Effect: Share Idea
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_sharidea_i<=3 | pbleffect_sharidea_j<=3
estimates store shareidea_low
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_sharidea_i>3 & pbleffect_sharidea_j>3
estimates store shareidea_high
qui suest shareidea_low shareidea_high, vce(cluster class)
test [shareidea_low_mean]HighCCEI = [shareidea_high_mean]HighCCEI

* PBL Effect: Help Study
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_helpstudy_i<=3 | pbleffect_helpstudy_j<=3
estimates store helpstudy_low
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_helpstudy_i>3 & pbleffect_helpstudy_j>3
estimates store helpstudy_high
qui suest helpstudy_low helpstudy_high, vce(cluster class)
test [helpstudy_low_mean]HighCCEI = [helpstudy_high_mean]HighCCEI

* PBL Effect: Help Non-Study
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_helpnonstudy_i<=3 | pbleffect_helpnonstudy_j<=3
estimates store helpnonstudy_low
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_helpnonstudy_i>3 & pbleffect_helpnonstudy_j>3
estimates store helpnonstudy_high
qui suest helpnonstudy_low helpnonstudy_high, vce(cluster class)
test [helpnonstudy_low_mean]HighCCEI = [helpnonstudy_high_mean]HighCCEI

* PBL Effect: Old Friend
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_oldfriend_i<=3 | pbleffect_oldfriend_j<=3
estimates store oldfriend_low
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_oldfriend_i>3 & pbleffect_oldfriend_j>3
estimates store oldfriend_high
qui suest oldfriend_low oldfriend_high, vce(cluster class)
test [oldfriend_low_mean]HighCCEI = [oldfriend_high_mean]HighCCEI

* PBL Effect: New Friend
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_newfriend_i<=3 | pbleffect_newfriend_j<=3
estimates store newfriend_low
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_newfriend_i>3 & pbleffect_newfriend_j>3
estimates store newfriend_high
qui suest newfriend_low newfriend_high, vce(cluster class)
test [newfriend_low_mean]HighCCEI = [newfriend_high_mean]HighCCEI

* PBL Effect: Friend
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_friend_i<=3 | pbleffect_friend_j<=3
estimates store friend_low
qui reg new2_I_ig HighCCEI ib1.class if pbleffect_friend_i>3 & pbleffect_friend_j>3
estimates store friend_high
qui suest friend_low friend_high, vce(cluster class)
test [friend_low_mean]HighCCEI = [friend_high_mean]HighCCEI

* Teacher Induce
qui reg new2_I_ig HighCCEI ib1.class if teacher_induce_i<=3 | teacher_induce_j<=3
estimates store teacher_low
qui reg new2_I_ig HighCCEI ib1.class if teacher_induce_i>3 & teacher_induce_j>3
estimates store teacher_high
qui suest teacher_low teacher_high, vce(cluster class)
test [teacher_low_mean]HighCCEI = [teacher_high_mean]HighCCEI

* Class Participate
qui reg new2_I_ig HighCCEI ib1.class if class_participate_i<=3 | class_participate_j<=3
estimates store participate_low
qui reg new2_I_ig HighCCEI ib1.class if class_participate_i>3 & class_participate_j>3
estimates store participate_high
qui suest participate_low participate_high, vce(cluster class)
test [participate_low_mean]HighCCEI = [participate_high_mean]HighCCEI

* TODO: value variable
* TODO: statistical test on heterogeneity results
* TODO: prep an appendix table that shows that it's not about functional form of covariates


use "data/finalized_panel_individual_250831.dta", clear

* why change over time? what interactions are going on?
foreach y in peer_sociable_i peer_fair_i peer_selfish_i peer_helpful_i peer_reciprocal_i class_sociable_i class_belonged_i class_outcast_i class_harass_i inclass_n_friends_i inclass_popularity_i n_eigenvec_i n_betweenness_i n_closeness_i max_closeness_i max_betweenness_i max_eigenvec_i class_participate_i value_fair_i value_warmglow_i value_trust_i value_cowork_i pbl_korean_i pbl_eng_i pbl_math_i pbl_science_i pbl_socialsci_i pblclass_horizontal_i pblclass_horizontal_q1_i pbleffect_sharidea_i pbleffect_helpstudy_i pbleffect_helpnonstudy_i pbleffect_oldfriend_i pbleffect_newfriend_i pbleffect_inclass_i pbleffect_outclass_i pbleffect_friend_i pbleffect_confidence_i pbleffect_school_i teacher_prepare_i teacher_induce_i brothers_i sisters_i {
	bysort class post: egen `y'_ct = mean(`y')
}

eststo clear
foreach y in peer_sociable_i peer_fair_i peer_selfish_i peer_helpful_i peer_reciprocal_i class_sociable_i class_belonged_i class_outcast_i class_harass_i inclass_n_friends_i inclass_popularity_i n_eigenvec_i n_betweenness_i n_closeness_i max_closeness_i max_betweenness_i max_eigenvec_i class_participate_i value_fair_i value_warmglow_i value_trust_i value_cowork_i pbl_korean_i pbl_eng_i pbl_math_i pbl_science_i pbl_socialsci_i pblclass_horizontal_i pblclass_horizontal_q1_i pbleffect_sharidea_i pbleffect_helpstudy_i pbleffect_helpnonstudy_i pbleffect_oldfriend_i pbleffect_newfriend_i pbleffect_inclass_i pbleffect_outclass_i pbleffect_friend_i pbleffect_confidence_i pbleffect_school_i teacher_prepare_i teacher_induce_i brothers_i sisters_i {
eststo: reghdfe new2_I_ig HighCCEI##c.`y'_ct post HighCCEI_post, absorb(class mover) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI##c.`y' post HighCCEI_post, absorb(class mover) vce(cluster class)
}
esttab, star(+ 0.1 * 0.05 ** 0.01)
esttab using "results/table_bargainingCCEI_mech2.csv", replace ///
    b(3) se(3) nogap compress star(+ 0.1 * 0.05 ** 0.01)
	
tab class_participate_i
recode 	 class_participate_i (0=.)
reghdfe new2_I_ig HighCCEI post HighCCEI_post, absorb(class mover) vce(cluster class)
reghdfe new2_I_ig HighCCEI##c.class_participate_i post HighCCEI_post, absorb(class mover) vce(cluster class)
* TODO: one last shot with more "data mining" approach	
* TODO: survey measure about overall vibe of the class or PBL
