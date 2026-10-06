************************************************************************
* WRITTEN BY Byunghun Hahn, 250825
* Label
************************************************************************

use "data/finalized_panel_individual_250831.dta", clear

label list

des


label variable ccei_i "CCEI"
label variable RA_i "Risk Aversion"
label variable new2_I_ig "Bargaining Power Index"
label variable male_i "Male"
label variable height_i "Height"
label variable inclass_n_friends_i "Out-Degree"
label variable inclass_popularity_i "In-Degree"
label variable mathscore_i "Math Score"
label variable outgoing_i "Outgoing"
label variable opened_i "Opened"
label variable agreeable_i "Agreeable"
label variable conscientious_i "Conscientious"
label variable stable_i "Stable"

label variable post  "Endline"
label variable HighCCEI "High CCEI"
label variable RT  "Risk Tolerance"
label variable ccei_ind_max "Max CCEI within Group"
label variable ccei_ind_dist "CCEI Dist within Group"
label variable mathscore_max "Max Mathscore within Group"
label variable mathscore_dist "Mathscore Dist within Group"
label variable ccei_j "Partner CCEI"
label variable mover "Mover"
label variable ccei_g "Group CCEI"
label variable f_high "High CCEI (F-GARP)"
label variable f_ccei_i "CCEI (F-GARP)"
label variable f_ccei_g "Group CCEI (F-GARP)"
label variable RA_g "Group Risk Aversion"
label variable RA_j "Partner Risk Aversion"

save "data/finalized_panel_individual_250831.dta", replace

********************************************************

use "data/finalized_panel_pbl_250831.dta", clear

label list

des

label variable ccei_g "CCEI"
label variable RA_g "Risk Aversion"
* label variable friend_oneside "Friendship: One-sided"
* label variable friend_mutual "Friendship: Mutual"


global group_char = "mathscore_max mathscore_dist mathscore_max_missing mathscore_dist_missing height_max height_dist i.malepair_co	outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_dist conscientious_max stable_max stable_dist big5_max_missing big5_dist_missing"
global friend_char = "oneside_friendship mutual_friendship inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist"

label variable mathscore_max "Max Mathscore"
label variable mathscore_dist "Dist Mathscore"
label variable height_max "Max Height"
label variable height_dist "Dist Height"
label variable oneside_friendship "Friendship: One-sided"
label variable mutual_friendship "Friendship: Mutual"
label variable inclass_n_friends_max "Max Out-Degree"
label variable inclass_n_friends_dist "Dist Out-Degre"
label variable inclass_popularity_max "Max In-Degree"
label variable inclass_popularity_dist "Dist In-Degre"

save "data/finalized_panel_pbl_250831.dta", replace
