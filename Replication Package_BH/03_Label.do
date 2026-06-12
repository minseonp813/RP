************************************************************************
* WRITTEN BY Byunghun Hahn
* Label
************************************************************************

********************************************************
* panel_individual
********************************************************

use "data/panel_individual.dta", clear

label list

des


label variable id "ID"
label variable partner_id "Partner ID"
label variable group_id "Group ID"
label variable endline "Endline"
label variable post "Endline"
label variable time "Endline"
label variable person "Person"
label variable mover "Mover"

label variable ccei_i "CCEI"
label variable ccei_j "Partner CCEI"
label variable ccei_g "Group CCEI"
label variable ccei_ig "Individual + Group CCEI"
label variable ccei_jg "Partner Individual + Group CCEI"
label variable ccei_hg "High-Player CCEI"
label variable ccei_lg "Low-Player CCEI"
label variable ccei_hlg "High-Low-Group CCEI"

label variable I_ig "Bargaining Power Index"
label variable new2_I_ig "Bargaining Power Index"
label variable I_hg "High-Player Bargaining Power Index"
label variable I_lg "Low-Player Bargaining Power Index"

label variable HighCCEI "High CCEI"
label variable HighCCEI_post "Endline x High CCEI"

label variable RA_i "Risk Aversion"
label variable RA_j "Partner Risk Aversion"
label variable RA_g "Group Risk Aversion"
label variable RA_diff "Risk Aversion Difference"
label variable RA_dist "Risk Aversion Distance"

label variable mathscore_i "Math Score"
label variable mathscore_j "Partner Math Score"
label variable math_diff "Math Score Difference"
label variable math_dist "Math Score Distance"

label variable male_i "Male"
label variable male_j "Partner Male"
label variable male_diff "Male Difference"
label variable male_dist "Male Distance"
label variable malepair "Number of Male Students in Pair"

label variable height_i "Height"
label variable height_j "Partner Height"
label variable height_diff "Height Difference"
label variable height_dist "Height Distance"

label variable weight_i "Weight"
label variable weight_j "Partner Weight"
label variable weight_diff "Weight Difference"
label variable weight_dist "Weight Distance"

label variable outgoing_i "Outgoing"
label variable outgoing_j "Partner Outgoing"
label variable outgoing_diff "Outgoing Difference"

label variable opened_i "Opened"
label variable opened_j "Partner Opened"
label variable opened_diff "Opened Difference"

label variable agreeable_i "Agreeable"
label variable agreeable_j "Partner Agreeable"
label variable agreeable_diff "Agreeable Difference"

label variable conscientious_i "Conscientious"
label variable conscientious_j "Partner Conscientious"
label variable conscientious_diff "Conscientious Difference"

label variable stable_i "Stable"
label variable stable_j "Partner Stable"
label variable stable_diff "Stable Difference"

label variable class_i "Class"
label variable class_j "Partner Class"

label variable inclass_n_friends_i "Out-Degree"
label variable inclass_n_friends_j "Partner Out-Degree"
label variable inclass_n_diff "Out-Degree Difference"
label variable inclass_n_dist "Out-Degree Distance"

label variable inclass_popularity_i "In-Degree"
label variable inclass_popularity_j "Partner In-Degree"
label variable inclass_pop_diff "In-Degree Difference"
label variable inclass_pop_dist "In-Degree Distance"

label variable friendship_i_to_j "Friendship: I Nominated J"
label variable friendship_j_to_i "Friendship: J Nominated I"
label variable mutual_friendship "Friendship: Mutual"
label variable oneway_friendship "Friendship: One-sided"
label variable oneside_friendship "Friendship: One-sided"
label variable none_friendship "Friendship: None"
label variable friendship "Friendship Status"

label variable class_sleep_i "Sleep in Class"
label variable class_sleep_j "Partner Sleep in Class"
label variable class_sleep_diff "Sleep in Class Difference"
label variable class_sleep_dist "Sleep in Class Distance"

label variable class_participate_i "Class Participation"
label variable class_participate_j "Partner Class Participation"
label variable class_participate_diff "Class Participation Difference"
label variable class_participate_dist "Class Participation Distance"

label variable teacher_prepare_i "Teacher Preparation"
label variable teacher_prepare_j "Partner Teacher Preparation"
label variable teacher_prepare_diff "Teacher Preparation Difference"
label variable teacher_prepare_dist "Teacher Preparation Distance"

label variable teacher_induce_i "Teacher Inducement"
label variable teacher_induce_j "Partner Teacher Inducement"
label variable teacher_induce_diff "Teacher Inducement Difference"
label variable teacher_induce_dist "Teacher Inducement Distance"

label variable class_study_i "Class Study"
label variable class_study_j "Partner Class Study"
label variable class_study_diff "Class Study Difference"
label variable class_study_dist "Class Study Distance"

label variable class_dislike_i "Class Dislike"
label variable class_dislike_j "Partner Class Dislike"
label variable class_dislike_diff "Class Dislike Difference"
label variable class_dislike_dist "Class Dislike Distance"

label variable class_practical_i "Class Practicality"
label variable class_practical_j "Partner Class Practicality"
label variable class_practical_diff "Class Practicality Difference"
label variable class_practical_dist "Class Practicality Distance"

label variable pbl_korean_i "PBL: Korean"
label variable pbl_korean_j "Partner PBL: Korean"
label variable pbl_korean_diff "PBL Korean Difference"
label variable pbl_korean_dist "PBL Korean Distance"

label variable pbl_eng_i "PBL: English"
label variable pbl_eng_j "Partner PBL: English"
label variable pbl_eng_diff "PBL English Difference"
label variable pbl_eng_dist "PBL English Distance"

label variable pbl_math_i "PBL: Math"
label variable pbl_math_j "Partner PBL: Math"
label variable pbl_math_diff "PBL Math Difference"
label variable pbl_math_dist "PBL Math Distance"

label variable pbl_science_i "PBL: Science"
label variable pbl_science_j "Partner PBL: Science"
label variable pbl_science_diff "PBL Science Difference"
label variable pbl_science_dist "PBL Science Distance"

label variable pbl_socialsci_i "PBL: Social Science"
label variable pbl_socialsci_j "Partner PBL: Social Science"
label variable pbl_socialsci_diff "PBL Social Science Difference"
label variable pbl_socialsci_dist "PBL Social Science Distance"

label variable RAT_score_generous_i "RAT Score: Generous"
label variable RAT_score_generous_j "Partner RAT Score: Generous"
label variable RAT_score_generous_diff "RAT Score Difference: Generous"
label variable RAT_score_generous_dist "RAT Score Distance: Generous"

label variable RAT_score_strict_i "RAT Score: Strict"
label variable RAT_score_strict_j "Partner RAT Score: Strict"
label variable RAT_score_strict_diff "RAT Score Difference: Strict"
label variable RAT_score_strict_dist "RAT Score Distance: Strict"

label variable f_ccei_i "CCEI (F-GARP)"
label variable f_ccei_j "Partner CCEI (F-GARP)"
label variable f_ccei_g "Group CCEI (F-GARP)"
label variable f_ccei_ig "Individual + Group CCEI (F-GARP)"
label variable f_ccei_jg "Partner Individual + Group CCEI (F-GARP)"
label variable f_ccei_hlg "High-Low-Group CCEI (F-GARP)"

label variable f_I_ig "Bargaining Power Index (F-GARP)"
label variable new2_f_I_ig "Bargaining Power Index (F-GARP)"

label variable HighF_CCEI "High CCEI (F-GARP)"
label variable HighF_CCEI_post "Endline x High CCEI (F-GARP)"

label variable min_mpi_i "Minimum MPI"
label variable min_mpi_j "Partner Minimum MPI"
label variable min_mpi_g "Group Minimum MPI"
label variable min_mpi_ig "Individual + Group Minimum MPI"
label variable min_mpi_jg "Partner Individual + Group Minimum MPI"


label variable max_mpi_i "Maximum MPI"
label variable max_mpi_j "Partner Maximum MPI"
label variable max_mpi_g "Group Maximum MPI"
label variable max_mpi_ig "Individual + Group Maximum MPI"
label variable max_mpi_jg "Partner Individual + Group Maximum MPI"

label variable HighMaxMPI "High Maximum MPI"
label variable HighMaxMPI_post "Endline x High Maximum MPI"
label variable I_max_mpi_ig "Bargaining Power Index: Maximum MPI"
label variable new2_I_max_mpi_ig "Bargaining Power Index: Maximum MPI"

label variable rev_max_mpi_i "Reversed Maximum MPI"
label variable rev_max_mpi_j "Partner Reversed Maximum MPI"
label variable rev_max_mpi_g "Group Reversed Maximum MPI"
label variable rev_max_mpi_ig "Individual + Group Reversed Maximum MPI"
label variable rev_max_mpi_jg "Partner Individual + Group Reversed Maximum MPI"
label variable rev_max_mpi_hg "High-Player Reversed Maximum MPI"
label variable rev_max_mpi_lg "Low-Player Reversed Maximum MPI"
label variable rev_max_mpi_hlg "High-Low-Group Reversed Maximum MPI"

label variable HighRevMaxMPI "High Reversed Maximum MPI"
label variable HighRevMaxMPI_post "Endline x High Reversed Maximum MPI"
label variable I_rev_max_mpi_ig "Bargaining Power Index: Reversed Maximum MPI"
label variable new2_I_rev_max_mpi_ig "Bargaining Power Index: Reversed Maximum MPI"
label variable I_rev_max_mpi_hg "High-Player Bargaining Power Index: Reversed Maximum MPI"
label variable I_rev_max_mpi_lg "Low-Player Bargaining Power Index: Reversed Maximum MPI"

label variable rmpi_i "Reversed Maximum MPI"
label variable rmpi_j "Partner Reversed Maximum MPI"
label variable rmpi_g "Group Reversed Maximum MPI"
label variable rmpi_ig "Individual + Group Reversed Maximum MPI"
label variable rmpi_jg "Partner Individual + Group Reversed Maximum MPI"
label variable I_rmpi_ig "Bargaining Power Index: Reversed Maximum MPI"
label variable HighRMPI "High Reversed Maximum MPI"
label variable HighRMPI_post "Endline x High Reversed Maximum MPI"

save "data/panel_individual.dta", replace


********************************************************
* panel_pbl
********************************************************

use "data/panel_pbl.dta", clear

label list

des


label variable group_id "Group ID"
label variable endline "Endline"
label variable time "Endline"
label variable id_1 "Person 1 ID"
label variable id_2 "Person 2 ID"

label variable ccei_1 "CCEI: Person 1"
label variable ccei_2 "CCEI: Person 2"
label variable ccei_g "Group CCEI"
label variable ccei_1g "Individual + Group CCEI: Person 1"
label variable ccei_2g "Individual + Group CCEI: Person 2"
label variable ccei_hg "High-Player CCEI"
label variable ccei_lg "Low-Player CCEI"
label variable ccei_hlg "High-Low-Group CCEI"

label variable ccei_ind_max "Max CCEI within Group"
label variable ccei_ind_min "Min CCEI within Group"
label variable ccei_ind_dist "CCEI Distance within Group"

label variable ccei_ig_max "Max Individual + Group CCEI within Group"
label variable ccei_ig_min "Min Individual + Group CCEI within Group"
label variable ccei_ig_dist "Individual + Group CCEI Distance within Group"

label variable ccei_max "Max CCEI within Group"
label variable ccei_dist "CCEI Distance within Group"

label variable high_ccei "High CCEI"
label variable I_hg "High-Player Bargaining Power Index"
label variable I_lg "Low-Player Bargaining Power Index"

label variable end_ccei_ind_max "Endline x Max CCEI within Group"
label variable end_ccei_ind_dist "Endline x CCEI Distance within Group"
label variable end_max "Endline x Max CCEI within Group"
label variable end_dist "Endline x CCEI Distance within Group"

label variable RA_g "Group Risk Aversion"
label variable RA_1 "Risk Aversion: Person 1"
label variable RA_2 "Risk Aversion: Person 2"
label variable RA_ind_max "Max Risk Aversion within Group"
label variable RA_ind_min "Min Risk Aversion within Group"
label variable RA_ind_dist "Risk Aversion Distance within Group"

label variable mathscore_1 "Math Score: Person 1"
label variable mathscore_2 "Math Score: Person 2"
label variable mathscore_max "Max Math Score"
label variable mathscore_min "Min Math Score"
label variable mathscore_dist "Math Score Distance"

label variable male_1 "Male: Person 1"
label variable male_2 "Male: Person 2"
label variable male_max "Max Male"
label variable male_min "Min Male"
label variable male_dist "Male Distance"
label variable malepair "Number of Male Students in Pair"

label variable height_1 "Height: Person 1"
label variable height_2 "Height: Person 2"
label variable height_max "Max Height"
label variable height_min "Min Height"
label variable height_dist "Height Distance"

label variable weight_1 "Weight: Person 1"
label variable weight_2 "Weight: Person 2"
label variable weight_max "Max Weight"
label variable weight_min "Min Weight"
label variable weight_dist "Weight Distance"

label variable outgoing_1 "Outgoing: Person 1"
label variable outgoing_2 "Outgoing: Person 2"
label variable outgoing_max "Max Outgoing"
label variable outgoing_min "Min Outgoing"
label variable outgoing_dist "Outgoing Distance"

label variable opened_1 "Opened: Person 1"
label variable opened_2 "Opened: Person 2"
label variable opened_max "Max Opened"
label variable opened_min "Min Opened"
label variable opened_dist "Opened Distance"

label variable agreeable_1 "Agreeable: Person 1"
label variable agreeable_2 "Agreeable: Person 2"
label variable agreeable_max "Max Agreeable"
label variable agreeable_min "Min Agreeable"
label variable agreeable_dist "Agreeable Distance"

label variable conscientious_1 "Conscientious: Person 1"
label variable conscientious_2 "Conscientious: Person 2"
label variable conscientious_max "Max Conscientious"
label variable conscientious_min "Min Conscientious"
label variable conscientious_dist "Conscientious Distance"

label variable stable_1 "Stable: Person 1"
label variable stable_2 "Stable: Person 2"
label variable stable_max "Max Stable"
label variable stable_min "Min Stable"
label variable stable_dist "Stable Distance"

label variable class_1 "Class: Person 1"
label variable class_2 "Class: Person 2"
label variable class "Class"

label variable inclass_n_friends_1 "Out-Degree: Person 1"
label variable inclass_n_friends_2 "Out-Degree: Person 2"
label variable inclass_n_friends_max "Max Out-Degree"
label variable inclass_n_friends_dist "Out-Degree Distance"

label variable inclass_popularity_1 "In-Degree: Person 1"
label variable inclass_popularity_2 "In-Degree: Person 2"
label variable inclass_popularity_max "Max In-Degree"
label variable inclass_popularity_dist "In-Degree Distance"

label variable friendship_1 "Friendship: Person 1 Nominated Person 2"
label variable friendship_2 "Friendship: Person 2 Nominated Person 1"
label variable mutual_friendship "Friendship: Mutual"
label variable oneway_friendship "Friendship: One-sided"
label variable oneside_friendship "Friendship: One-sided"
label variable none_friendship "Friendship: None"
label variable friendship "Friendship Status"

label variable class_sleep_1 "Sleep in Class: Person 1"
label variable class_sleep_2 "Sleep in Class: Person 2"
label variable class_sleep_max "Max Sleep in Class"
label variable class_sleep_min "Min Sleep in Class"
label variable class_sleep_dist "Sleep in Class Distance"

label variable class_participate_1 "Class Participation: Person 1"
label variable class_participate_2 "Class Participation: Person 2"
label variable class_participate_max "Max Class Participation"
label variable class_participate_min "Min Class Participation"
label variable class_participate_dist "Class Participation Distance"

label variable teacher_prepare_1 "Teacher Preparation: Person 1"
label variable teacher_prepare_2 "Teacher Preparation: Person 2"
label variable teacher_prepare_max "Max Teacher Preparation"
label variable teacher_prepare_min "Min Teacher Preparation"
label variable teacher_prepare_dist "Teacher Preparation Distance"

label variable teacher_induce_1 "Teacher Inducement: Person 1"
label variable teacher_induce_2 "Teacher Inducement: Person 2"
label variable teacher_induce_max "Max Teacher Inducement"
label variable teacher_induce_min "Min Teacher Inducement"
label variable teacher_induce_dist "Teacher Inducement Distance"

label variable class_study_1 "Class Study: Person 1"
label variable class_study_2 "Class Study: Person 2"
label variable class_study_max "Max Class Study"
label variable class_study_min "Min Class Study"
label variable class_study_dist "Class Study Distance"

label variable class_dislike_1 "Class Dislike: Person 1"
label variable class_dislike_2 "Class Dislike: Person 2"
label variable class_dislike_max "Max Class Dislike"
label variable class_dislike_min "Min Class Dislike"
label variable class_dislike_dist "Class Dislike Distance"

label variable class_practical_1 "Class Practicality: Person 1"
label variable class_practical_2 "Class Practicality: Person 2"
label variable class_practical_max "Max Class Practicality"
label variable class_practical_min "Min Class Practicality"
label variable class_practical_dist "Class Practicality Distance"

label variable pbl_korean_1 "PBL: Korean, Person 1"
label variable pbl_korean_2 "PBL: Korean, Person 2"
label variable pbl_korean_max "Max PBL: Korean"
label variable pbl_korean_min "Min PBL: Korean"
label variable pbl_korean_dist "PBL Korean Distance"

label variable pbl_eng_1 "PBL: English, Person 1"
label variable pbl_eng_2 "PBL: English, Person 2"
label variable pbl_eng_max "Max PBL: English"
label variable pbl_eng_min "Min PBL: English"
label variable pbl_eng_dist "PBL English Distance"

label variable pbl_math_1 "PBL: Math, Person 1"
label variable pbl_math_2 "PBL: Math, Person 2"
label variable pbl_math_max "Max PBL: Math"
label variable pbl_math_min "Min PBL: Math"
label variable pbl_math_dist "PBL Math Distance"

label variable pbl_science_1 "PBL: Science, Person 1"
label variable pbl_science_2 "PBL: Science, Person 2"
label variable pbl_science_max "Max PBL: Science"
label variable pbl_science_min "Min PBL: Science"
label variable pbl_science_dist "PBL Science Distance"

label variable pbl_socialsci_1 "PBL: Social Science, Person 1"
label variable pbl_socialsci_2 "PBL: Social Science, Person 2"
label variable pbl_socialsci_max "Max PBL: Social Science"
label variable pbl_socialsci_min "Min PBL: Social Science"
label variable pbl_socialsci_dist "PBL Social Science Distance"

label variable RAT_score_generous_1 "RAT Score: Generous, Person 1"
label variable RAT_score_generous_2 "RAT Score: Generous, Person 2"
label variable RAT_score_generous_max "Max RAT Score: Generous"
label variable RAT_score_generous_dist "RAT Score Distance: Generous"

label variable RAT_score_strict_1 "RAT Score: Strict, Person 1"
label variable RAT_score_strict_2 "RAT Score: Strict, Person 2"
label variable RAT_score_strict_max "Max RAT Score: Strict"
label variable RAT_score_strict_dist "RAT Score Distance: Strict"

label variable f_ccei_1 "CCEI (F-GARP): Person 1"
label variable f_ccei_2 "CCEI (F-GARP): Person 2"
label variable f_ccei_g "Group CCEI (F-GARP)"
label variable f_ccei_1g "Individual + Group CCEI (F-GARP): Person 1"
label variable f_ccei_2g "Individual + Group CCEI (F-GARP): Person 2"
label variable f_ccei_hg "High-Player CCEI (F-GARP)"
label variable f_ccei_lg "Low-Player CCEI (F-GARP)"
label variable f_ccei_hlg "High-Low-Group CCEI (F-GARP)"

label variable f_ccei_ind_max "Max CCEI within Group (F-GARP)"
label variable f_ccei_ind_min "Min CCEI within Group (F-GARP)"
label variable f_ccei_ind_dist "CCEI Distance within Group (F-GARP)"
label variable f_ccei_max "Max CCEI within Group (F-GARP)"
label variable f_ccei_dist "CCEI Distance within Group (F-GARP)"

label variable f_ccei_ig_max "Max Individual + Group CCEI within Group (F-GARP)"
label variable f_ccei_ig_min "Min Individual + Group CCEI within Group (F-GARP)"
label variable f_ccei_ig_dist "Individual + Group CCEI Distance within Group (F-GARP)"

label variable f_high_ccei "High CCEI (F-GARP)"
label variable f_I_hg "High-Player Bargaining Power Index (F-GARP)"
label variable f_I_lg "Low-Player Bargaining Power Index (F-GARP)"

label variable end_f_ccei_ind_max "Endline x Max CCEI within Group (F-GARP)"
label variable end_f_ccei_ind_dist "Endline x CCEI Distance within Group (F-GARP)"

label variable min_mpi_1 "Minimum MPI: Person 1"
label variable min_mpi_2 "Minimum MPI: Person 2"
label variable min_mpi_g "Group Minimum MPI"
label variable min_mpi_1g "Individual + Group Minimum MPI: Person 1"
label variable min_mpi_2g "Individual + Group Minimum MPI: Person 2"
label variable min_mpi_hg "High-Player Minimum MPI"
label variable min_mpi_lg "Low-Player Minimum MPI"
label variable min_mpi_hlg "High-Low-Group Minimum MPI"

label variable min_mpi_ind_lowest "Lowest Minimum MPI within Group"
label variable min_mpi_ind_highest "Highest Minimum MPI within Group"
label variable min_mpi_ind_dist "Minimum MPI Distance within Group"

label variable min_mpi_ig_lowest "Lowest Individual + Group Minimum MPI within Group"
label variable min_mpi_ig_highest "Highest Individual + Group Minimum MPI within Group"
label variable min_mpi_ig_dist "Individual + Group Minimum MPI Distance within Group"

label variable min_mpi_high "High Minimum MPI"
label variable I_min_mpi_hg "High-Player Bargaining Power Index: Minimum MPI"
label variable I_min_mpi_lg "Low-Player Bargaining Power Index: Minimum MPI"

label variable max_mpi_1 "Maximum MPI: Person 1"
label variable max_mpi_2 "Maximum MPI: Person 2"
label variable max_mpi_g "Group Maximum MPI"
label variable max_mpi_1g "Individual + Group Maximum MPI: Person 1"
label variable max_mpi_2g "Individual + Group Maximum MPI: Person 2"
label variable max_mpi_hg "High-Player Maximum MPI"
label variable max_mpi_lg "Low-Player Maximum MPI"
label variable max_mpi_hlg "High-Low-Group Maximum MPI"

label variable max_mpi_ind_lowest "Lowest Maximum MPI within Group"
label variable max_mpi_ind_highest "Highest Maximum MPI within Group"
label variable max_mpi_ind_dist "Maximum MPI Distance within Group"

label variable max_mpi_ig_lowest "Lowest Individual + Group Maximum MPI within Group"
label variable max_mpi_ig_highest "Highest Individual + Group Maximum MPI within Group"
label variable max_mpi_ig_dist "Individual + Group Maximum MPI Distance within Group"

label variable max_mpi_high "High Maximum MPI"
label variable I_max_mpi_hg "High-Player Bargaining Power Index: Maximum MPI"
label variable I_max_mpi_lg "Low-Player Bargaining Power Index: Maximum MPI"

label variable rev_max_mpi_1 "Reversed Maximum MPI: Person 1"
label variable rev_max_mpi_2 "Reversed Maximum MPI: Person 2"
label variable rev_max_mpi_g "Group Reversed Maximum MPI"
label variable rev_max_mpi_1g "Individual + Group Reversed Maximum MPI: Person 1"
label variable rev_max_mpi_2g "Individual + Group Reversed Maximum MPI: Person 2"
label variable rev_max_mpi_hg "High-Player Reversed Maximum MPI"
label variable rev_max_mpi_lg "Low-Player Reversed Maximum MPI"
label variable rev_max_mpi_hlg "High-Low-Group Reversed Maximum MPI"

label variable rev_max_mpi_ind_max "Max Reversed Maximum MPI within Group"
label variable rev_max_mpi_ind_min "Min Reversed Maximum MPI within Group"
label variable rev_max_mpi_ind_dist "Reversed Maximum MPI Distance within Group"

label variable rev_max_mpi_ig_max "Max Individual + Group Reversed Maximum MPI within Group"
label variable rev_max_mpi_ig_min "Min Individual + Group Reversed Maximum MPI within Group"
label variable rev_max_mpi_ig_dist "Individual + Group Reversed Maximum MPI Distance within Group"

label variable rev_max_mpi_high "High Reversed Maximum MPI"
label variable I_rev_max_mpi_hg "High-Player Bargaining Power Index: Reversed Maximum MPI"
label variable I_rev_max_mpi_lg "Low-Player Bargaining Power Index: Reversed Maximum MPI"

label variable rev_max_mpi_max "Max Reversed Maximum MPI within Group"
label variable rev_max_mpi_dist "Reversed Maximum MPI Distance within Group"


save "data/panel_pbl.dta", replace