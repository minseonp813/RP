************************************************************************
* WRITTEN BY Byunghun Hahn
* Tables
************************************************************************

set more off
set matsize 8000

capture mkdir "results"

use "data/panel_pbl.dta", clear

capture drop inclass_n_friends_max
capture drop inclass_n_friends_dist
gen inclass_n_friends_max = max(inclass_n_friends_1, inclass_n_friends_2)
gen inclass_n_friends_dist = abs(inclass_n_friends_1 - inclass_n_friends_2)

capture drop inclass_popularity_max
capture drop inclass_popularity_dist
gen inclass_popularity_max = max(inclass_popularity_1, inclass_popularity_2)
gen inclass_popularity_dist = abs(inclass_popularity_1 - inclass_popularity_2)

capture drop ccei_max
capture drop ccei_dist
gen ccei_max = max(ccei_1, ccei_2)
gen ccei_dist = abs(ccei_1 - ccei_2)

capture drop endline
gen endline = time

capture drop end_max
capture drop end_dist
gen end_max = ccei_max * time 
gen end_dist = ccei_dist * time

capture drop RA_max
capture drop RA_dist
gen RA_max = RA_ind_max
gen RA_dist = RA_ind_dist

capture drop malepair_co
gen malepair_co = malepair

capture drop mathscore_max_missing
capture drop mathscore_dist_missing
gen mathscore_max_missing = missing(mathscore_1) | missing(mathscore_2)
gen mathscore_dist_missing = missing(mathscore_1) | missing(mathscore_2)

capture drop big5_max_missing
capture drop big5_dist_missing
gen big5_max_missing = ///
    missing(outgoing_1) | missing(outgoing_2) | ///
    missing(opened_1) | missing(opened_2) | ///
    missing(agreeable_1) | missing(agreeable_2) | ///
    missing(conscientious_1) | missing(conscientious_2) | ///
    missing(stable_1) | missing(stable_2)

gen big5_dist_missing = big5_max_missing

capture drop height_max_missing
capture drop height_dist_missing
gen height_max_missing = missing(height_1) | missing(height_2)
gen height_dist_missing = missing(height_1) | missing(height_2)

reg ccei_g ///
    ccei_max ccei_dist ///
    , r cluster(class)
outreg2 using "results/reg_results_pool.xls", replace se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg ccei_g ///
    ccei_max ccei_dist ///
	endline end_max end_dist ///
    , r cluster(class)
outreg2 using "results/reg_results_pool.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")
	
reg ccei_g ///
    ccei_max ccei_dist ///
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
outreg2 using "results/reg_results_pool.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg ccei_g ///
    ccei_max ccei_dist ///
	endline end_max end_dist ///
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
outreg2 using "results/reg_results_pool.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")		

************************

capture confirm variable f_ccei_col
if !_rc {
    ren f_ccei_col f_ccei_g
}

capture drop f_end_max
capture drop f_end_dist
gen f_end_max = f_ccei_max * time 
gen f_end_dist = f_ccei_dist * time

reg f_ccei_g ///
    f_ccei_max f_ccei_dist ///
    , r cluster(class)
outreg2 using "results/reg_results_pool.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg f_ccei_g ///
    f_ccei_max f_ccei_dist ///
	endline f_end_max f_end_dist ///
    , r cluster(class)
outreg2 using "results/reg_results_pool.xls", append se bdec(3) tdec(3) ///
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
outreg2 using "results/reg_results_pool.xls", append se bdec(3) tdec(3) ///
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
outreg2 using "results/reg_results_pool.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")	
	
*******************************************

capture drop one_minus_maxmpi_1
capture drop one_minus_maxmpi_2
capture drop one_minus_maxmpi_g
gen one_minus_maxmpi_1 = rev_max_mpi_1
gen one_minus_maxmpi_2 = rev_max_mpi_2
gen one_minus_maxmpi_g = rev_max_mpi_g

capture drop one_minus_mpimax_max
capture drop one_minus_mpimax_dist
gen one_minus_mpimax_max = max(one_minus_maxmpi_1, one_minus_maxmpi_2)
gen one_minus_mpimax_dist = abs(one_minus_maxmpi_1 - one_minus_maxmpi_2)

capture drop mpi_end_max
capture drop mpi_end_dist
gen mpi_end_max = one_minus_mpimax_max * time 
gen mpi_end_dist = one_minus_mpimax_dist * time

reg one_minus_maxmpi_g ///
    one_minus_mpimax_max one_minus_mpimax_dist ///
    , r cluster(class)
outreg2 using "results/reg_results_pool.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")

reg one_minus_maxmpi_g ///
    one_minus_mpimax_max one_minus_mpimax_dist ///
	endline mpi_end_max mpi_end_dist ///
    , r cluster(class)
outreg2 using "results/reg_results_pool.xls", append se bdec(3) tdec(3) ///
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
outreg2 using "results/reg_results_pool.xls", append se bdec(3) tdec(3) ///
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
outreg2 using "results/reg_results_pool.xls", append se bdec(3) tdec(3) ///
    ctitle(Pooled OLS) addnote("Robust SE clustered by class")	