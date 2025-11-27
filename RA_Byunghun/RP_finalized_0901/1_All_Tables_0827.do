************************************************************************
* WRITTEN BY Byunghun Hahn, 250825
* All Tables
************************************************************************

* 1. Table A1 : Balance Test

set more off
set matsize 8000

use "data/finalized_panel_final_250825.dta", clear

* Case 1: LHS = Mover

reg ccei_1 ccei_2 i.class
reg RA_1 RA_2 i.class
reg mathscore mathscore2 i.class
reg male male2 i.class
reg outgoing outgoing2 i.class
reg opened opened2 i.class
reg agreeable agreeable2 i.class
reg conscientious conscientious2 i.class
reg stable stable2 i.class
reg height height2 i.class
reg inclass_n_friends_1 inclass_n_friends_2 i.class
reg inclass_popularity_1 inclass_popularity_2 i.class

eststo m1  : reg ccei_1         ccei_2         i.class
eststo m2  : reg RA_1           RA_2           i.class
eststo m3  : reg mathscore    mathscore2    i.class
eststo m4  : reg male         male2         i.class
eststo m5  : reg outgoing     outgoing2     i.class
eststo m6  : reg opened       opened2       i.class
eststo m7 : reg agreeable    agreeable2    i.class
eststo m8 : reg conscientious conscientious2 i.class
eststo m9 : reg stable       stable2       i.class
eststo m10 : reg height       height2       i.class
eststo m11 : reg inclass_n_friends_1 inclass_n_friends_2 i.class
eststo m12 : reg inclass_popularity_1 inclass_popularity_2 i.class

suest m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12

* all beta=0 joint Wald test
test ( [m1_mean]ccei_2                    = 0 ) ///
     ( [m2_mean]RA_2                      = 0 ) ///
     ( [m3_mean]mathscore2               = 0 ) ///
     ( [m4_mean]male2                    = 0 ) ///
     ( [m5_mean]outgoing2                = 0 ) ///
     ( [m6_mean]opened2                  = 0 ) ///
     ( [m7_mean]agreeable2               = 0 ) ///
     ( [m8_mean]conscientious2           = 0 ) ///
     ( [m9_mean]stable2                  = 0 ) ///
     ( [m10_mean]height2                 = 0 ) ///
     ( [m11_mean]inclass_n_friends_2     = 0 ) ///
     ( [m12_mean]inclass_popularity_2    = 0 )


******************************************************

* 1. Table 1 : Balance Test

use "data/finalized_panel_individual_250825.dta", clear
sum ccei_i RA_i male_i height_i inclass_n_friends_i inclass_popularity_i mathscore_i outgoing_i opened_i agreeable_i conscientious_i stable_i if post==0


* Case 2: LHS = randomly selected

use "data/finalized_panel_final_250825.dta", clear

replace height = 182 if height > 182
replace height = 146 if height < 146

replace height2 = 182 if height2 > 182
replace height2 = 146 if height2 < 146

set seed 100000

* 1. Generate Random Number (0~1)
gen double rand = runiform()

* 2. if rand >= 0.5, ccei_i = ccei_1 and ccei_j = ccei_2
* if rand < 0.5, ccei_i = ccei_2 and ccei_j = ccei_1

gen ccei_i         = cond(rand >= 0.5, ccei_1, ccei_2)
gen ccei_j         = cond(rand >= 0.5, ccei_2, ccei_1)

gen RA_i           = cond(rand >= 0.5, RA_1, RA_2)
gen RA_j           = cond(rand >= 0.5, RA_2, RA_1)

gen mathscore_i    = cond(rand >= 0.5, mathscore, mathscore2)
gen mathscore_j    = cond(rand >= 0.5, mathscore2, mathscore)

gen male_i         = cond(rand >= 0.5, male, male2)
gen male_j         = cond(rand >= 0.5, male2, male)

gen outgoing_i     = cond(rand >= 0.5, outgoing, outgoing2)
gen outgoing_j     = cond(rand >= 0.5, outgoing2, outgoing)

gen opened_i       = cond(rand >= 0.5, opened, opened2)
gen opened_j       = cond(rand >= 0.5, opened2, opened)

gen agreeable_i    = cond(rand >= 0.5, agreeable, agreeable2)
gen agreeable_j    = cond(rand >= 0.5, agreeable2, agreeable)

gen conscientious_i = cond(rand >= 0.5, conscientious, conscientious2)
gen conscientious_j = cond(rand >= 0.5, conscientious2, conscientious)

gen stable_i       = cond(rand >= 0.5, stable, stable2)
gen stable_j       = cond(rand >= 0.5, stable2, stable)

gen height_i       = cond(rand >= 0.5, height, height2)
gen height_j       = cond(rand >= 0.5, height2, height)

gen inclass_n_friends_i   = cond(rand >= 0.5, inclass_n_friends_1, inclass_n_friends_2)
gen inclass_n_friends_j   = cond(rand >= 0.5, inclass_n_friends_2, inclass_n_friends_1)

gen inclass_popularity_i   = cond(rand >= 0.5, inclass_popularity_1, inclass_popularity_2)
gen inclass_popularity_j   = cond(rand >= 0.5, inclass_popularity_2, inclass_popularity_1)


reg ccei_i ccei_j i.class
reg RA_i RA_j i.class
reg mathscore_i mathscore_j i.class
reg male_i male_j i.class
reg outgoing_i outgoing_j i.class
reg opened_i opened_j i.class
reg agreeable_i agreeable_j i.class
reg conscientious_i conscientious_j i.class
reg stable_i stable_j i.class
reg height_i height_j i.class
reg inclass_n_friends_i inclass_n_friends_j i.class
reg inclass_popularity_i inclass_popularity_j i.class




**************************************************************

eststo m1  : reg ccei_i         ccei_j         i.class
eststo m2  : reg RA_i           RA_j           i.class
eststo m3  : reg mathscore_i    mathscore_j    i.class
eststo m4  : reg male_i         male_j         i.class
eststo m5  : reg outgoing_i     outgoing_j     i.class
eststo m6  : reg opened_i       opened_j       i.class
eststo m7 : reg agreeable_i    agreeable_j    i.class
eststo m8 : reg conscientious_i conscientious_j i.class
eststo m9 : reg stable_i       stable_j       i.class
eststo m10 : reg height_i       height_j       i.class
eststo m11 : reg inclass_n_friends_i inclass_n_friends_j i.class
eststo m12 : reg inclass_popularity_i inclass_popularity_j i.class

suest m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12

* all beta=0 joint Wald test
test ( [m1_mean]ccei_j                   = 0 ) ///
     ( [m2_mean]RA_j                     = 0 ) ///
     ( [m3_mean]mathscore_j             = 0 ) ///
     ( [m4_mean]male_j                  = 0 ) ///
     ( [m5_mean]outgoing_j              = 0 ) ///
     ( [m6_mean]opened_j                = 0 ) ///
     ( [m7_mean]agreeable_j             = 0 ) ///
     ( [m8_mean]conscientious_j         = 0 ) ///
     ( [m9_mean]stable_j                = 0 ) ///
     ( [m10_mean]height_j               = 0 ) ///
     ( [m11_mean]inclass_n_friends_j    = 0 ) ///
     ( [m12_mean]inclass_popularity_j   = 0 )
	 
**************************************************************

* Table 2

/*
Group char = height, gender composition, cognitive/non-cognitive ability
Friendship = in-degree, out-degree, mutual friendship

* Table 2- Group CCEI
	col 1 - CCEI + class FE
	col 2 - CCEI + class FE + endline
	col 3 - group char + friendship + class FE
	col 4 - CCEI + class FE + endline + group char + friendship 
	
* Friendship - Let's add whether individuals pointed each other as friends on top of in-degree/out-degree. In the main table, let's only show mutual friendship variables. To be specific, add 1(one side friendship), and 1(mutual friendship) in the regression equation. 
*/

use "data/finalized_panel_pbl_250827.dta", clear

global group_char = "mathscore_max mathscore_dist mathscore_max_missing mathscore_dist_missing height_max height_dist i.malepair_co	outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_dist conscientious_max stable_max stable_dist big5_max_missing big5_dist_missing"
global friend_char = "oneside_friendship mutual_friendship inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist"

la var ccei_max "$\text{CCEI}_{\text{max},gt}$"
la var ccei_dist "$\text{CCEI}_{\text{dist},gt}$"
la var mathscore_max "$\text{Math Score}_{\text{max},gt}$"
la var mathscore_dist "$\text{Math Score}_{\text{dist},gt}$"
la var endline "Endline"
la var end_max "$\text{Endline} \times \text{CCEI}_{\text{max},gt}$ "
la var end_dist "$\text{Endline} \times \text{CCEI}_{\text{dist},gt}$"
la var oneside_friendship "$\text{Oneside Friendship}$"
la var mutual_friendship "$\text{Mutual Friendship}$"

eststo clear
eststo: reg ccei_g ccei_max ccei_dist i.class, r cluster(class)
eststo: reg ccei_g ccei_max ccei_dist endline end_max end_dist i.class, r cluster(class)
eststo: reg ccei_g i.class $group_char $friend_char, r cluster(class)
eststo: reg ccei_g ccei_max ccei_dist endline end_max end_dist i.class $group_char $friend_char, r cluster(class)
esttab using "results/table_groupCCEI.tex", replace ///
    b(%9.3f) se(%9.3f) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    keep(*ccei* mathscore_max mathscore_dist endline end_max end_dist oneside_friendship mutual_friendship) label ///
    nogap fragment nomtitles nonumbers compress collabels(none) nolines


* (NEW) Table A2

la var RA_max "$\text{RA}_{\text{max},gt}$"
la var RA_dist "$\text{RA}_{\text{dist},gt}$"
la var corner_ratio_max "$\text{Corner Ratio}_{\text{max},gt}$"
la var corner_ratio_dist "$\text{Corner Ratio}_{\text{dist},gt}$"

reg ccei_g ccei_max ccei_dist endline end_max end_dist i.class $group_char $friend_char, r cluster(class)

reg ccei_g ccei_max ccei_dist endline end_max end_dist i.class $group_char $friend_char RA_max RA_dist, r cluster(class)

reg ccei_g ccei_max ccei_dist endline end_max end_dist i.class $group_char $friend_char corner_ratio_max corner_ratio_dist, r cluster(class)


eststo clear
eststo: reg ccei_g ccei_max ccei_dist endline end_max end_dist i.class $group_char $friend_char, r cluster(class)
eststo: reg ccei_g ccei_max ccei_dist endline end_max end_dist i.class $group_char $friend_char RA_max RA_dist, r cluster(class)
eststo: reg ccei_g ccei_max ccei_dist endline end_max end_dist i.class $group_char $friend_char corner_ratio_max corner_ratio_dist, r cluster(class)

esttab using "results/table_groupCCEI_withRA.tex", replace ///
    b(%9.3f) se(%9.3f) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    keep(*ccei* mathscore_max mathscore_dist endline end_max end_dist RA_max RA_dist corner_ratio_max corner_ratio_dist oneside_friendship mutual_friendship) label ///
    nogap fragment nomtitles nonumbers compress collabels(none) nolines

reg ccei_g ccei_max ccei_dist endline end_max end_dist i.class $group_char $friend_char corner_ratio_max corner_ratio_dist, r cluster(class)


* (New) Table A3 - Replicate col 4 of table 2 using other CCEI measures 


la var f_ccei_max "$\text{FGARP}_{\text{max},gt}$"
la var f_ccei_dist "$\text{FGARP}_{\text{dist},gt}$"
la var f_end_max "$\text{Endline} \times \text{FGARP}_{\text{max},gt}$ "
la var f_end_dist "$\text{Endline} \times \text{FGARP}_{\text{dist},gt}$"

la var one_minus_mpimax_max "$\text{(1 - Max MPI)}_{\text{max},gt}$"
la var one_minus_mpimax_dist "$\text{(1 - Max MPI)}_{\text{dist},gt}$"
la var mpi_end_max "$\text{Endline} \times (1 - \text{Max MPI})_{\text{max},gt}$"
la var mpi_end_dist "$\text{Endline} \times (1 - \text{Max MPI})_{\text{dist},gt}$"

eststo clear
eststo: reg f_ccei_g f_ccei_max f_ccei_dist endline f_end_max f_end_dist i.class $group_char $friend_char, r cluster(class)
eststo: reg max_mpi_g one_minus_mpimax_max one_minus_mpimax_dist endline mpi_end_max mpi_end_dist i.class $group_char $friend_char, r cluster(class)

esttab using "results/table_groupCCEI_other.tex", replace ///
    b(%9.3f) se(%9.3f) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    keep(f_ccei_max f_ccei_dist endline f_end_max f_end_dist one_minus_mpimax_max one_minus_mpimax_dist mpi_end_max mpi_end_dist oneside_friendship mutual_friendship) label ///
    nogap fragment nomtitles nonumbers compress collabels(none) nolines



* (New) Table A4 : shapley decomposition table

clear
use "data/finalized_panel_pbl_250827.dta", clear

tab malepair_co, gen(FE_malepair_)

global group_char = "mathscore_max mathscore_dist mathscore_max_missing mathscore_dist_missing height_max height_dist FE_malepair_1 FE_malepair_2 FE_malepair_3 FE_malepair_4 FE_malepair_5	outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_dist conscientious_max stable_max stable_dist big5_max_missing big5_dist_missing"
global friend_char = "oneside_friendship mutual_friendship inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist"

macro drop class
global class ""

capture drop class_id
egen class_id = group(class)
capture noisily levelsof class_id, local(cls)
local base : word 1 of `cls'

foreach c of local cls {
    if "`c'" != "`base'" {
        gen byte FE_class_`c' = (class_id==`c')
        label var FE_class_`c' "class FE: group `c'"
        global class "$class FE_class_`c'"
    }
}

display as result "$class"

local G_TIME   "endline end_max end_dist"
local G_CCEI   "ccei_max ccei_dist"
local G_OBS "$group_char $friend_char"   
local G_CLASS "$class"

local GROUPS "`G_TIME', `G_CCEI', `G_OBS', `G_CLASS'"

reg ccei_g ccei_max ccei_dist endline end_max end_dist $class $group_char $friend_char, r cluster(class) 

estimates store FULL

estimates restore FULL
shapley2, stat(r2) group("`GROUPS'")


**************************************************************

/*

* Table A2

reg f_ccei_g ///
    f_ccei_max f_ccei_dist ///
    , r cluster(class)
outreg2 using "results/tableA2.xls", replace se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg f_ccei_g ///
    f_ccei_max f_ccei_dist ///
	endline f_end_max f_end_dist ///
    , r cluster(class)
outreg2 using "results/tableA2.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
reg f_ccei_g ///
    f_ccei_max f_ccei_dist ///
	RA_max RA_dist ///
	i.class ///
	mathscore_max mathscore_dist ///
    mathscore_max_missing mathscore_dist_missing ///
	height_max height_dist ///
	i.malepair_co ///
	inclass_n_friends_max inclass_n_friends_dist ///
	inclass_popularity_max inclass_popularity_dist ///
	outgoing_max outgoing_dist big5_max_missing big5_dist_missing ///
    , r cluster(class)
outreg2 using "results/tableA2.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg f_ccei_g ///
    f_ccei_max f_ccei_dist ///
	endline f_end_max f_end_dist ///
	RA_max RA_dist ///
	i.class ///
	mathscore_max mathscore_dist ///
    mathscore_max_missing mathscore_dist_missing ///
	height_max height_dist ///
	i.malepair_co ///
	inclass_n_friends_max inclass_n_friends_dist ///
	inclass_popularity_max inclass_popularity_dist ///
	outgoing_max outgoing_dist big5_max_missing big5_dist_missing ///
    , r cluster(class)
outreg2 using "results/tableA2.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")	
	
**************************************************************

* Table A3

reg one_minus_maxmpi_g ///
    one_minus_mpimax_max one_minus_mpimax_dist ///
    , r cluster(class)
outreg2 using "results/tableA3.xls", replace se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg one_minus_maxmpi_g ///
    one_minus_mpimax_max one_minus_mpimax_dist ///
	endline mpi_end_max mpi_end_dist ///
    , r cluster(class)
outreg2 using "results/tableA3.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
reg one_minus_maxmpi_g ///
    one_minus_mpimax_max one_minus_mpimax_dist ///
	RA_max RA_dist ///
	i.class ///
	mathscore_max mathscore_dist ///
    mathscore_max_missing mathscore_dist_missing ///
	height_max height_dist ///
	i.malepair_co ///
	inclass_n_friends_max inclass_n_friends_dist ///
	inclass_popularity_max inclass_popularity_dist ///
	outgoing_max outgoing_dist big5_max_missing big5_dist_missing ///
    , r cluster(class)
outreg2 using "results/tableA3.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg one_minus_maxmpi_g ///
    one_minus_mpimax_max one_minus_mpimax_dist ///
	endline mpi_end_max mpi_end_dist ///
	RA_max RA_dist ///
	i.class ///
	mathscore_max mathscore_dist ///
    mathscore_max_missing mathscore_dist_missing ///
	height_max height_dist ///
	i.malepair_co ///
	inclass_n_friends_max inclass_n_friends_dist ///
	inclass_popularity_max inclass_popularity_dist ///
	outgoing_max outgoing_dist big5_max_missing big5_dist_missing ///
    , r cluster(class)
outreg2 using "results/tableA3.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

**************************************************************

* Table 3

use "data/finalized_panel_individual_250825.dta", clear

reg RA_ig  ///
    HighCCEI ///
    , r cluster(class)
outreg2 using "results\table3.xls", replace se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")	

reg RA_ig  ///
    HighCCEI post HighCCEI_post ///
    , r cluster(class)
outreg2 using "results\table3.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")	

reg RA_ig  ///
    HighCCEI ///
	mover  ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results\table3.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")	
	
	
reg RA_ig  ///
    HighCCEI post HighCCEI_post ///
	mover  ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i  i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results\table3.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
	
**************************************************************

* Table 4

use "data/finalized_panel_individual_250825.dta", clear

reg new2_I_ig ///
    HighCCEI ///
    , r cluster(class)
outreg2 using "results/table4.xls", replace se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_I_ig ///
    HighCCEI post HighCCEI_post ///
    , r cluster(class)
outreg2 using "results/table4.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_I_ig ///
    HighCCEI post HighCCEI_post ///
    ccei_i RA_i ///
	, r cluster(class)
outreg2 using "results/table4.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
reg new2_I_ig ///
    HighCCEI mover ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/table4.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_I_ig ///
    HighCCEI post HighCCEI_post ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/table4.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_I_ig ///
    HighCCEI post HighCCEI_post ///
    ccei_i RA_i ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
	, r cluster(class)
outreg2 using "results/table4.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
**************************************************************

* Table A4

use "data/finalized_panel_individual_250825.dta", clear

reg f_new2_i ///
    f_high ///
    , r cluster(class)
outreg2 using "results/tableA4.xls", replace se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg f_new2_i ///
    f_high post High_End_FGARP ///
    , r cluster(class)
outreg2 using "results/tableA4.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg f_new2_i ///
    f_high post High_End_FGARP ///
    f_ccei_i RA_i ///
	, r cluster(class)
outreg2 using "results/tableA4.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
reg f_new2_i ///
    f_high mover ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/tableA4.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg f_new2_i ///
    f_high post High_End_FGARP mover ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/tableA4.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg f_new2_i ///
    f_high post High_End_FGARP mover ///
    f_ccei_i RA_i ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
	, r cluster(class)
outreg2 using "results/tableA4.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
**************************************************************

* Table A5

use "data/finalized_panel_individual_250825.dta", clear

reg new2_max_MPI_ig ///
    max_mpi_high ///
    , r cluster(class)
outreg2 using "results/tableA5.xls", replace se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_max_MPI_ig ///
    max_mpi_high post High_End_MPI ///
    , r cluster(class)
outreg2 using "results/tableA5.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_max_MPI_ig ///
    max_mpi_high post High_End_MPI ///
    one_minus_maxmpi_i RA_i ///
	, r cluster(class)
outreg2 using "results/tableA5.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
reg new2_max_MPI_ig ///
    max_mpi_high mover ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/tableA5.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_max_MPI_ig ///
    max_mpi_high post High_End_MPI mover ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/tableA5.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_max_MPI_ig ///
    max_mpi_high post High_End_MPI mover ///
    one_minus_maxmpi_i RA_i ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
	, r cluster(class)
outreg2 using "results/tableA5.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

**************************************************************

* Table A6

use "data/finalized_panel_individual_250825.dta", clear

gen selected = (random_i > random_j)

* Note: In the heterogeneity analysis, we use one randomly selected individual from each group, instead of using the full individual-level dataset.
* Variables random_i, random_j are generated in R and then merged here.

eststo clear
eststo: reg new2_I_ig HighCCEI post HighCCEI_post ///
    ccei_i RA_i i.class if selected==1, r cluster(class)
eststo: reg new2_I_ig HighCCEI post HighCCEI_post ///
	mf fm ff ///
    mf_High fm_High ff_High ///
    mf_Post  fm_Post  ff_Post  ///
    mf_High_Post fm_High_Post ff_High_Post ///
    ccei_i RA_i i.class if selected==1, r cluster(class)
eststo: reg new2_I_ig HighCCEI post HighCCEI_post ///
    math_diff ///
    math_High ///
    math_Post ///
    math_High_Post ///
    ccei_i RA_i i.class if selected==1, r cluster(class)
eststo: reg new2_I_ig HighCCEI post HighCCEI_post ///
    height_diff ///
    height_High ///
    height_Post ///
    height_High_Post ///
    ccei_i RA_i i.class if selected==1, r cluster(class)	
eststo: reg new2_I_ig HighCCEI post HighCCEI_post ///
    mover ///
    mover_HighCCEI ///
    mover_post  ///
	mover_HighCCEI_post ///
    ccei_i RA_i i.class if selected==1, r cluster(class)
eststo: reg new2_I_ig HighCCEI post HighCCEI_post ///
    inclass_n_diff inclass_n_HighCCEI inclass_n_post inclass_n_HighCCEI_post ///
    ccei_i RA_i i.class if selected==1, r cluster(class)	
eststo: reg new2_I_ig HighCCEI post HighCCEI_post ///
    inclass_pop_diff inclass_pop_HighCCEI inclass_pop_post inclass_pop_HighCCEI_post ///
    ccei_i RA_i i.class if selected==1, r cluster(class)	
esttab, drop(*.class*)

**************************************************************

* Table A7

use "data/finalized_panel_individual_250825.dta", clear

macro drop class
global class ""

capture drop class_id
egen class_id = group(class)
capture noisily levelsof class_id, local(cls)
local base : word 1 of `cls'

foreach c of local cls {
    if "`c'" != "`base'" {
        gen byte FE_class_`c' = (class_id==`c')
        label var FE_class_`c' "class FE: group `c'"
        global class "$class FE_class_`c'"
    }
}

display as result "$class"

local G_TIME   "post"
local G_CCEI   "HighCCEI HighCCEI_post ccei_i"
local G_RISK   "RA_i"
local G_FRIEND    "inclass_n_diff inclass_n_HighCCEI inclass_n_post inclass_n_HighCCEI_post inclass_pop_diff inclass_pop_HighCCEI inclass_pop_post inclass_pop_HighCCEI_post"
local G_DEMO_COG_NCOG "mf fm ff mf_High fm_High ff_High mf_Post fm_Post ff_Post mf_High_Post fm_High_Post ff_High_Post" "math_diff math_High math_Post math_High_Post" "height_diff height_High height_Post height_High_Post" "outgoing_diff outgoing_HighCCEI outgoing_post outgoing_H_post opened_diff opened_HighCCEI opened_post opened_H_post agreeable_diff agreeable_HighCCEI agreeable_post agreeable_H_post conscientious_diff conscientious_HighCCEI conscientious_post conscientious_H_post stable_diff stable_HighCCEI stable_post stable_H_post"
local G_MOVER  "mover mover_HighCCEI mover_post mover_HighCCEI_post"
local G_ATT   "mathscore_dist_missing"
local G_CLASS "$class"

local GROUPS "`G_TIME', `G_CCEI', `G_RISK', `G_FRIEND', `G_DEMO_COG_NCOG', `G_MOVER', `G_ATT', `G_CLASS'"

reg new2_I_ig post HighCCEI HighCCEI_post ccei_i RA_i ///
    inclass_n_diff inclass_n_HighCCEI inclass_n_post inclass_n_HighCCEI_post ///
    inclass_pop_diff inclass_pop_HighCCEI inclass_pop_post inclass_pop_HighCCEI_post ///
    mf fm ff mf_High fm_High ff_High mf_Post fm_Post ff_Post mf_High_Post fm_High_Post ff_High_Post ///
    math_diff math_High math_Post math_High_Post ///
    height_diff height_High height_Post height_High_Post ///
    mover mover_HighCCEI mover_post mover_HighCCEI_post ///
    outgoing_diff outgoing_HighCCEI outgoing_post outgoing_H_post ///
    opened_diff opened_HighCCEI opened_post opened_H_post ///
    agreeable_diff agreeable_HighCCEI agreeable_post agreeable_H_post ///
    conscientious_diff conscientious_HighCCEI conscientious_post conscientious_H_post ///
    stable_diff stable_HighCCEI stable_post stable_H_post ///
    mathscore_dist_missing ///
    $class

estimates store FULL

estimates restore FULL
shapley2, stat(r2) group("`GROUPS'")

preserve
clear
set obs 8

gen str20 block = ""
replace block = "Time"             in 1
replace block = "CCEI"             in 2
replace block = "Risk"             in 3
replace block = "Friendship"       in 4
replace block = "Demographic/cognitive/non-cognitive" in 5
replace block = "Mover"            in 6
replace block = "SurveyAttrition"  in 7
replace block = "Class"  in 8

tempname Sh Sr
matrix `Sh' = e(shapley)
matrix `Sr' = e(shapley_rel)

svmat double `Sh', names(col)
capture confirm variable c1
if !_rc rename c1 shapley_val
capture confirm variable Sh1
if !_rc rename Sh1 shapley_val

svmat double `Sr', names(col)
capture confirm variable c1
if !_rc rename c1 shapley_share
capture confirm variable Sr1
if !_rc rename Sr1 shapley_share

gen shapley_pct = 100*shapley_share

export excel block shapley_val shapley_pct using "results/tableA7_Index.xlsx", ///
    firstrow(variables) replace

putexcel set "results/tableA7_Index.xlsx", modify

local n = _N
putexcel B2:B`=1+`n'', nformat(number_d3)
putexcel C2:C`=1+`n'', nformat(number_d3)

restore



use "data/finalized_panel_individual_250825.dta", clear

macro drop class
global class ""

capture drop class_id
egen class_id = group(class)
capture noisily levelsof class_id, local(cls)
local base : word 1 of `cls'

foreach c of local cls {
    if "`c'" != "`base'" {
        gen byte FE_class_`c' = (class_id==`c')
        label var FE_class_`c' "class FE: group `c'"
        global class "$class FE_class_`c'"
    }
}

display as result "$class"

local G_TIME   "post"
local G_CCEI   "HighCCEI HighCCEI_post ccei_i"
local G_RISK   "RA_i"
local G_FRIEND    "inclass_n_diff inclass_n_HighCCEI inclass_n_post inclass_n_HighCCEI_post inclass_pop_diff inclass_pop_HighCCEI inclass_pop_post inclass_pop_HighCCEI_post"
local G_DEMO_COG_NCOG "mf fm ff mf_High fm_High ff_High mf_Post fm_Post ff_Post mf_High_Post fm_High_Post ff_High_Post" "math_diff math_High math_Post math_High_Post" "height_diff height_High height_Post height_High_Post" "outgoing_diff outgoing_HighCCEI outgoing_post outgoing_H_post opened_diff opened_HighCCEI opened_post opened_H_post agreeable_diff agreeable_HighCCEI agreeable_post agreeable_H_post conscientious_diff conscientious_HighCCEI conscientious_post conscientious_H_post stable_diff stable_HighCCEI stable_post stable_H_post"
local G_MOVER  "mover mover_HighCCEI mover_post mover_HighCCEI_post"
local G_ATT   "mathscore_dist_missing"
local G_CLASS "$class"

local GROUPS "`G_TIME', `G_CCEI', `G_RISK', `G_FRIEND', `G_DEMO_COG_NCOG', `G_MOVER', `G_ATT', `G_CLASS'"


reg RA_ig post HighCCEI HighCCEI_post ccei_i RA_i ///
    inclass_n_diff inclass_n_HighCCEI inclass_n_post inclass_n_HighCCEI_post ///
    inclass_pop_diff inclass_pop_HighCCEI inclass_pop_post inclass_pop_HighCCEI_post ///
    mf fm ff mf_High fm_High ff_High mf_Post fm_Post ff_Post mf_High_Post fm_High_Post ff_High_Post ///
    math_diff math_High math_Post math_High_Post ///
    height_diff height_High height_Post height_High_Post ///
    mover mover_HighCCEI mover_post mover_HighCCEI_post ///
    outgoing_diff outgoing_HighCCEI outgoing_post outgoing_H_post ///
    opened_diff opened_HighCCEI opened_post opened_H_post ///
    agreeable_diff agreeable_HighCCEI agreeable_post agreeable_H_post ///
    conscientious_diff conscientious_HighCCEI conscientious_post conscientious_H_post ///
    stable_diff stable_HighCCEI stable_post stable_H_post ///
    mathscore_dist_missing ///
    $class

estimates store FULL

* Shapley
estimates restore FULL
shapley2, stat(r2) group("`GROUPS'")

preserve
clear
set obs 8

gen str20 block = ""
replace block = "Time"             in 1
replace block = "CCEI"             in 2
replace block = "Risk"             in 3
replace block = "Friendship"       in 4
replace block = "Demographic/cognitive/non-cognitive" in 5
replace block = "Mover"            in 6
replace block = "SurveyAttrition"  in 7
replace block = "Class"  in 8

tempname Sh Sr
matrix `Sh' = e(shapley)
matrix `Sr' = e(shapley_rel)

svmat double `Sh', names(col)
capture confirm variable c1
if !_rc rename c1 shapley_val
capture confirm variable Sh1
if !_rc rename Sh1 shapley_val

svmat double `Sr', names(col)
capture confirm variable c1
if !_rc rename c1 shapley_share
capture confirm variable Sr1
if !_rc rename Sr1 shapley_share

gen shapley_pct = 100*shapley_share

* Save Excel

export excel block shapley_val shapley_pct using "results/tableA7_Risk.xlsx", ///
    firstrow(variables) replace

putexcel set "results/tableA7_Risk.xlsx", modify

local n = _N
putexcel B2:B`=1+`n'', nformat(number_d3)
putexcel C2:C`=1+`n'', nformat(number_d3)

restore

**************************************************************

**************************************************************

**************************************************************

**************************************************************

**************************************************************
