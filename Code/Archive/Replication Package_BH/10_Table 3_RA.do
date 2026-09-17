************************************************************************
* WRITTEN BY Byunghun Hahn
* Tables
************************************************************************

set more off
set matsize 8000

capture mkdir "results"

use "data/panel_individual.dta", clear

capture confirm numeric variable class
if _rc {
    capture drop class_raw
    gen class_raw = class_i
    capture drop class
    encode class_raw, gen(class)
}

capture confirm numeric variable class
if !_rc {
    capture confirm string variable class
    if !_rc {
        capture drop class_raw
        gen class_raw = class
        capture drop class
        encode class_raw, gen(class)
    }
}

replace height_i = 182 if height_i > 182
replace height_i = 146 if height_i < 146

replace height_j = 182 if height_j > 182
replace height_j = 146 if height_j < 146

capture drop height_diff
capture drop math_diff
capture drop inclass_n_diff
capture drop inclass_pop_diff
capture drop outgoing_diff
capture drop opened_diff
capture drop agreeable_diff
capture drop conscientious_diff
capture drop stable_diff

gen height_diff = height_i - height_j
gen math_diff = mathscore_i - mathscore_j
gen inclass_n_diff = inclass_n_friends_i - inclass_n_friends_j
gen inclass_pop_diff = inclass_popularity_i - inclass_popularity_j
gen outgoing_diff = outgoing_i - outgoing_j
gen opened_diff = opened_i - opened_j
gen agreeable_diff = agreeable_i - agreeable_j
gen conscientious_diff = conscientious_i - conscientious_j
gen stable_diff = stable_i - stable_j

capture drop mathscore_dist_missing
capture drop outgoing_diff_missing
capture drop opened_diff_missing
capture drop agreeable_diff_missing
capture drop conscientious_diff_missing
capture drop stable_diff_missing

gen mathscore_dist_missing = missing(math_diff)
gen outgoing_diff_missing = missing(outgoing_diff)
gen opened_diff_missing = missing(opened_diff)
gen agreeable_diff_missing = missing(agreeable_diff)
gen conscientious_diff_missing = missing(conscientious_diff)
gen stable_diff_missing = missing(stable_diff)

replace math_diff = 0 if missing(math_diff)
replace outgoing_diff = 0 if missing(outgoing_diff)
replace opened_diff = 0 if missing(opened_diff)
replace agreeable_diff = 0 if missing(agreeable_diff)
replace conscientious_diff = 0 if missing(conscientious_diff)
replace stable_diff = 0 if missing(stable_diff)

capture drop high_dummy
capture drop f_high
capture drop max_mpi_high
capture drop f_new2_i
capture drop new2_max_MPI_ig
capture drop one_minus_maxmpi_i
capture drop malepair_co

gen high_dummy = HighCCEI
gen f_high = HighF_CCEI
gen max_mpi_high = HighMaxMPI
gen f_new2_i = new2_f_I_ig
gen new2_max_MPI_ig = new2_I_max_mpi_ig
gen one_minus_maxmpi_i = rev_max_mpi_i
gen malepair_co = malepair

sum height_diff math_diff inclass_n_diff inclass_pop_diff outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff
tab outgoing_diff_missing 
tab opened_diff_missing 
tab agreeable_diff_missing 
tab conscientious_diff_missing 
tab stable_diff_missing

capture drop int_high_time
capture drop RA_ig

gen int_high_time = high_dummy*time 
gen RA_ig = abs(RA_i - RA_g)

reg RA_ig  ///
    high_dummy ///
    , r cluster(class)
outreg2 using "results/reg_results_individual.xls", replace se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")	

reg RA_ig  ///
    high_dummy time int_high_time ///
    , r cluster(class)
outreg2 using "results/reg_results_individual.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")	

reg RA_ig  ///
    high_dummy ///
	mover  ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/reg_results_individual.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")	
	
	
reg RA_ig  ///
    high_dummy time int_high_time ///
	mover  ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i  i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/reg_results_individual.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
	
	
	
******************************************
* Table 4
******************************************

reg new2_I_ig ///
    high_dummy ///
    , r cluster(class)
outreg2 using "results/reg_results_individual.xls", replace se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_I_ig ///
    high_dummy time int_high_time ///
    , r cluster(class)
outreg2 using "results/reg_results_individual.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_I_ig ///
    high_dummy time int_high_time ///
    ccei_i RA_i ///
	, r cluster(class)
outreg2 using "results/reg_results_individual.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
reg new2_I_ig ///
    high_dummy mover ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/reg_results_individual.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_I_ig ///
    high_dummy time int_high_time mover ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/reg_results_individual.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_I_ig ///
    high_dummy time int_high_time mover ///
    ccei_i RA_i ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
	, r cluster(class)
outreg2 using "results/reg_results_individual.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
******************************************
* F GARP
******************************************

capture drop High_End_FGARP
gen High_End_FGARP = f_high * time


reg f_new2_i ///
    f_high ///
    , r cluster(class)
outreg2 using "results/reg_results_individual_fgarp.xls", replace se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg f_new2_i ///
    f_high time High_End_FGARP ///
    , r cluster(class)
outreg2 using "results/reg_results_individual_fgarp.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg f_new2_i ///
    f_high time High_End_FGARP ///
    f_ccei_i RA_i ///
	, r cluster(class)
outreg2 using "results/reg_results_individual_fgarp.xls", append se bdec(3) tdec(3) ///
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
outreg2 using "results/reg_results_individual_fgarp.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg f_new2_i ///
    f_high time High_End_FGARP mover ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/reg_results_individual_fgarp.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg f_new2_i ///
    f_high time High_End_FGARP mover ///
    f_ccei_i RA_i ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
	, r cluster(class)
outreg2 using "results/reg_results_individual_fgarp.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
******************************************
* Max MPI
******************************************

capture drop one_minus_maxmpi_i
capture drop High_End_MPI

gen one_minus_maxmpi_i = rev_max_mpi_i
gen High_End_MPI = max_mpi_high * time

reg new2_max_MPI_ig ///
    max_mpi_high ///
    , r cluster(class)
outreg2 using "results/reg_results_individual_mpi.xls", replace se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_max_MPI_ig ///
    max_mpi_high time High_End_MPI ///
    , r cluster(class)
outreg2 using "results/reg_results_individual_mpi.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_max_MPI_ig ///
    max_mpi_high time High_End_MPI ///
    one_minus_maxmpi_i RA_i ///
	, r cluster(class)
outreg2 using "results/reg_results_individual_mpi.xls", append se bdec(3) tdec(3) ///
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
outreg2 using "results/reg_results_individual_mpi.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_max_MPI_ig ///
    max_mpi_high time High_End_MPI mover ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
    , r cluster(class)
outreg2 using "results/reg_results_individual_mpi.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg new2_max_MPI_ig ///
    max_mpi_high time High_End_MPI mover ///
    one_minus_maxmpi_i RA_i ///
	math_diff mathscore_dist_missing ///
    height_diff ///
	inclass_n_diff inclass_pop_diff /// 
	outgoing_diff opened_diff agreeable_diff conscientious_diff stable_diff ///
	outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing ///
    i.male_i i.malepair_co ///
	i.class ///
	, r cluster(class)
outreg2 using "results/reg_results_individual_mpi.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")