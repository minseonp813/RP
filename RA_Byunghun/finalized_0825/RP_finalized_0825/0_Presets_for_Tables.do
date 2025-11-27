************************************************************************
* WRITTEN BY Byunghun Hahn, 250825
* Presets for Tables : Generating Interaction Terms, Winsorizeing ...
************************************************************************

* We use three datasets. In this do-file, we 전처리 all three.
* (1) Panel Final: 652 rows, group-level. Each row contains both baseline and endline information.
* (2) Panel Individual: 652 × 4 rows, individual-level. Since there are two individuals per group and two time periods, the number of rows is four times larger.
* (3) Panel PBL: 652 × 2 rows, group-level. Each group is observed in two time periods, so the number of rows is doubled. This dataset was originally constructed for the PBL analysis. 

set more off
set matsize 8000

use "data/finalized_panel_final.dta", clear

replace height = 182 if height > 182
replace height = 146 if height < 146

replace height2 = 182 if height2 > 182
replace height2 = 146 if height2 < 146

save "data/finalized_panel_final_250825.dta", replace

*****************************************************************

use "data/finalized_panel_pbl.dta", clear

* Note: Height already winsorized here

gen inclass_n_friends_max = max(inclass_n_friends_1, inclass_n_friends_2)
gen inclass_n_friends_dist = abs(inclass_n_friends_1 - inclass_n_friends_2)

gen inclass_popularity_max = max(inclass_popularity_1, inclass_popularity_2)
gen inclass_popularity_dist = abs(inclass_popularity_1 - inclass_popularity_2)

gen ccei_max = max(ccei_1, ccei_2)
gen ccei_dist = abs(ccei_1 - ccei_2)

gen endline = time
gen end_max = ccei_max * time 
gen end_dist = ccei_dist * time

ren f_ccei_col f_ccei_g
gen f_end_max = f_ccei_max * time 
gen f_end_dist = f_ccei_dist * time

gen one_minus_maxmpi_1 = 1 - max_mpi_1 
gen one_minus_maxmpi_2 = 1 - max_mpi_2
gen one_minus_maxmpi_g = 1 - max_mpi_g

gen one_minus_mpimax_max = max(one_minus_maxmpi_1, one_minus_maxmpi_2)
gen one_minus_mpimax_dist = abs(one_minus_maxmpi_1 - one_minus_maxmpi_2)

gen mpi_end_max = one_minus_mpimax_max * time 
gen mpi_end_dist = one_minus_mpimax_dist * time

save "data/finalized_panel_pbl_250825.dta", replace

*****************************************************************

use "data/finalized_panel_individual.dta", clear

replace height_i = 182 if height_i > 182
replace height_i = 146 if height_i < 146

replace height_j = 182 if height_j > 182
replace height_j = 146 if height_j < 146

gen height_diff = height_i - height_j
gen math_diff = mathscore_i - mathscore_j
gen inclass_n_diff = inclass_n_friends_i - inclass_n_friends_j
gen inclass_pop_diff = inclass_popularity_i - inclass_popularity_j
gen outgoing_diff = outgoing_i - outgoing_j
gen opened_diff = opened_i - opened_j
gen agreeable_diff = agreeable_i - agreeable_j
gen conscientious_diff = conscientious_i - conscientious_j
gen stable_diff = stable_i - stable_j

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

gen RA_ig = abs(RA_i - RA_g)

gen High_End_FGARP = f_high * time

gen one_minus_maxmpi_i = 1 - max_mpi_i
gen High_End_MPI = max_mpi_high * time

ren high_dummy HighCCEI
ren time post
gen HighCCEI_post = HighCCEI * post

gen mm = (male_i == 1 & male_j == 1)
gen mf = (male_i == 1 & male_j == 0)
gen fm = (male_i == 0 & male_j == 1)
gen ff = (male_i == 0 & male_j == 0)

foreach g in mf fm ff {
    gen `g'_High    = `g' * HighCCEI
    gen `g'_Post     = `g' * post
    gen `g'_High_Post = `g' * HighCCEI * post
}

gen math_High = math_diff * HighCCEI
gen math_Post = math_diff * post
gen math_High_Post = math_diff * HighCCEI * post


gen height_High = height_diff * HighCCEI
gen height_Post = height_diff * post
gen height_High_Post = height_diff * HighCCEI * post

gen mover_HighCCEI = mover * HighCCEI
gen mover_post = mover * post
gen mover_HighCCEI_post = mover * HighCCEI * post

gen inclass_n_HighCCEI = inclass_n_diff * HighCCEI
gen inclass_n_post = inclass_n_diff * post
gen inclass_n_HighCCEI_post = inclass_n_diff * HighCCEI * post

gen inclass_pop_HighCCEI = inclass_pop_diff * HighCCEI
gen inclass_pop_post = inclass_pop_diff * post
gen inclass_pop_HighCCEI_post = inclass_pop_diff * HighCCEI * post

foreach var in outgoing opened agreeable conscientious stable {
    gen `var'_HighCCEI = `var'_diff * HighCCEI
    gen `var'_post = `var'_diff * post
    gen `var'_H_post = `var'_diff * HighCCEI * post
}

save "data/finalized_panel_individual_250825.dta", replace
