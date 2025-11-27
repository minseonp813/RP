********************************************************************************
* Table: Collective CCEI
********************************************************************************

set more off
set matsize 8000

use "data/finalized_panel_pbl_250825.dta", clear

global group_char = "mathscore_max mathscore_dist mathscore_max_missing mathscore_dist_missing height_max height_dist i.malepair_co	outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_dist conscientious_max stable_max stable_dist big5_max_missing big5_dist_missing"
global friend_char = "inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist i.friendship" 

la var ccei_max "$\text{CCEI}_{\text{max},gt}$"
la var ccei_dist "$\text{CCEI}_{\text{dist},gt}$"
la var mathscore_max "$\text{Math Score}_{\text{max},gt}$"
la var mathscore_dist "$\text{Math Score}_{\text{dist},gt}$"
la var endline "Endline"
la var end_max "$\text{Endline} \times \text{CCEI}_{\text{max},gt}$ "
la var end_dist "$\text{Endline} \times \text{CCEI}_{\text{dist},gt}$"

eststo clear
eststo: reghdfe ccei_g ccei_max ccei_dist, absorb(class) vce(cluster class)
eststo: reghdfe ccei_g ccei_max ccei_dist endline end_max end_dist, absorb(class) vce(cluster class)
eststo: reghdfe ccei_g $group_char $friend_char, absorb(class) vce(cluster class)
eststo: reghdfe ccei_g ccei_max ccei_dist endline end_max end_dist $group_char $friend_char, absorb(class) vce(cluster class)
eststo: reghdfe ccei_g ccei_max ccei_dist $group_char $friend_char, absorb(group_id) vce(cluster class)
esttab using "results/table_groupCCEI.csv", replace b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) keep(*ccei* mathscore_max mathscore_dist *end* *pop*) nogap compress star(+ 0.1 * 0.05 ** 0.01)
esttab using "results/table_groupCCEI.tex", replace ///
    b(%9.3f) se(%9.3f) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    keep(*ccei* mathscore_max mathscore_dist *end*) label ///
    nogap fragment nomtitles nonumbers compress collabels(none) nolines


********************************************************************************
* Table: Bargaining using RA
********************************************************************************

g RA_highCCEI = cond(ccei_1>=ccei_2,RA_1,RA_2)
g RA_lowCCEI = cond(ccei_1<ccei_2,RA_1,RA_2)

eststo clear
eststo: reghdfe RA_g RA_highCCEI RA_lowCCEI, absorb(class) vce(cluster class)
eststo: reghdfe RA_g endline##c.RA_highCCEI endline##c.RA_lowCCEI, absorb(class) vce(cluster class)
*TODO: add other characteristics similarly
esttab , b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01) 
*esttab using "results/table_bargainingCCEI.csv", replace b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01) drop(*missing*)
// TODO: check why we're losing some obs




********************************************************************************
* Table: Bargaining using CCEI
********************************************************************************
	
use "data/finalized_panel_individual_250825.dta", clear

egen malepair = group(male_i male_j)

global group_char = "mathscore_i math_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff i.malepair"
global friend_char = "inclass_n_friends inclass_n_diff inclass_popularity inclass_pop_diff i.friendship" 
global missing_char = "mathscore_dist_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing "

eststo clear
eststo: reghdfe new2_I_ig HighCCEI, absorb(class mover) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI post HighCCEI_post, absorb(class mover) vce(cluster class)
eststo: reghdfe new2_I_ig $group_char $friend_char $missing_char, absorb(class mover) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI post HighCCEI_post $group_char $friend_char $missing_char, absorb(class mover) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI $group_char $friend_char $missing_char, absorb(id) vce(cluster class)
esttab , b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01) drop(*missing*)
esttab using "results/table_bargainingCCEI.csv", replace b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01) drop(*missing*)
// TODO: check why we're losing some obs


********************************************************************************
* Table: Bargaining using CCEI - Mechanism
********************************************************************************

* higher CCEI - easier to communicate what you want?
* maybe it's easier to say what you want when you know what you want
	* covariates that could mediate the "easiness of communication"
	* not a direct evidence, but quite informative 
eststo clear
eststo: reghdfe new2_I_ig i.HighCCEI if friendship~=2, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig i.HighCCEI if friendship==2, absorb(class) vce(cluster class)

eststo: reghdfe new2_I_ig i.HighCCEI if male_i~=male_j, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig i.HighCCEI if male_i==male_j, absorb(class) vce(cluster class)

eststo: reghdfe new2_I_ig i.HighCCEI if outgoing_i<=3.5|outgoing_j<=3.5, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig i.HighCCEI if outgoing_i>3.5&outgoing_j>3.5, absorb(class) vce(cluster class)

eststo: reghdfe new2_I_ig i.HighCCEI if agreeable_i<=3|agreeable_j<=3, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig i.HighCCEI if agreeable_i>3&agreeable_j>3, absorb(class) vce(cluster class)

eststo: reghdfe new2_I_ig i.HighCCEI if opened_i<=3.5|opened_j<=3.5, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig i.HighCCEI if opened_i>3.5&opened_j>3.5, absorb(class) vce(cluster class)
esttab , b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01)
* TODO: survey measure on who took the lead
* TODO: prep an appendix table that shows that it's not about functional form of covariates


* why change over time? what interactions are going on?
g friendship_b = friendship if post==0
sort group_id friendship_b
bysort group_id: replace friendship_b = friendship_b[1] if missing(friendship_b)

eststo clear
eststo: reghdfe new2_I_ig HighCCEI post HighCCEI_post if friendship_b~=2, absorb(class) vce(cluster class)
eststo: reghdfe new2_I_ig HighCCEI post HighCCEI_post if friendship_b==2, absorb(class) vce(cluster class)
esttab , b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01)
* TODO: survey measure about overall vibe of the class or PBL




/*use "data/finalized_panel_individual_250825.dta", clear

egen malepair = group(male_i male_j)

global group_char = "mathscore_i math_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff i.malepair"
global friend_char = "inclass_n_friends inclass_n_diff inclass_popularity inclass_pop_diff i.friendship" 
global missing_char = "mathscore_dist_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing "

g RA_ijg = abs(RA_i - RA_g)/ (abs(RA_i - RA_g)+abs(RA_j - RA_g))

eststo clear
eststo: reghdfe RA_ijg HighCCEI, absorb(class mover) vce(cluster class)
eststo: reghdfe RA_ijg HighCCEI post HighCCEI_post, absorb(class mover) vce(cluster class)
eststo: reghdfe RA_ijg $group_char $friend_char $missing_char, absorb(class mover) vce(cluster class)
eststo: reghdfe RA_ijg HighCCEI post HighCCEI_post $group_char $friend_char $missing_char, absorb(class mover) vce(cluster class)
eststo: reghdfe RA_ijg HighCCEI post HighCCEI_post $group_char $friend_char $missing_char, absorb(id) vce(cluster class)
esttab , b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01)


g pe = cond(RA_g-RA_i>=0 & RA_g-RA_j<=0 | RA_g-RA_i<0 & RA_g-RA_j>=0,1,0)

eststo clear
eststo: reghdfe pe ccei_max ccei_dist, absorb(class) vce(cluster class)
eststo: reghdfe pe ccei_max ccei_dist endline end_max end_dist, absorb(class) vce(cluster class)
eststo: reghdfe pe $group_char $friend_char, absorb(class) vce(cluster class)
eststo: reghdfe pe ccei_max ccei_dist endline end_max end_dist $group_char $friend_char, absorb(class) vce(cluster class)
eststo: reghdfe pe ccei_max ccei_dist $group_char $friend_char, absorb(group_id) vce(cluster class)
esttab , replace b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) keep(*ccei* mathscore_max mathscore_dist *end* *pop* *male*) nogap compress 	


preserve
keep if pe==1
eststo clear
eststo: reghdfe RA_ijg HighCCEI, absorb(class mover) vce(cluster class)
eststo: reghdfe RA_ijg HighCCEI post HighCCEI_post, absorb(class mover) vce(cluster class)
eststo: reghdfe RA_ijg $group_char $friend_char , absorb(class mover) vce(cluster class)
eststo: reghdfe RA_ijg HighCCEI post HighCCEI_post $group_char $friend_char, absorb(class mover) vce(cluster class)
eststo: reghdfe RA_ijg HighCCEI post HighCCEI_post $group_char $friend_char, absorb(id) vce(cluster class)
esttab , b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress star(+ 0.1 * 0.05 ** 0.01)
restore

