log using 1_DataLabeling_020920.log, replace

********************************************************************************
* WRITTEN BY MINSEON PARK 02-09-20
* Labe Main Data Sets
******************************************************************************** 

use Risk_ByPair.dta, clear
	
*** Labeling Variable Names	
	la var Iv_afriat_ind "Student 1 Iv_afriat"
	la var Iv_afriat_ind2 "Student 2 Iv_afriat"
	la var Iv_afriat_ind_max "Max of Indiv. Iv_afriats"
	la var Iv_afriat_ind_dist "Difference of Indiv. Iv_afriats"
	la var Iv_afriat_ind_ave "Average of Indiv. Iv_afriats"
	la var Iv_varian1_ind "Student 1 Iv_varian_min"
	la var Iv_varian1_ind2 "Student 2 Iv_varian_min"
	la var Iv_varian1_ind_max "Max of Indiv. Iv_varians_min"
	la var Iv_varian1_ind_dis "Difference of Indiv. Iv_varians_min"
	la var Iv_varian2_ind "Student 1 Iv_varian_mean"
	la var Iv_varian2_ind2 "Student 2 Iv_varian_mean"
	la var Iv_varian2_ind_max "Max of Indiv. Iv_varians_mean"
	la var Iv_varian2_ind_dis "Difference of Indiv. Iv_varians_mean"
	la var Iv_varian3_ind "Student 1 Iv_varian_avgssq"
	la var Iv_varian3_ind2 "Student 2 Iv_varian_avgssq"
	la var Iv_varian3_ind_max "Max of Indiv. Iv_varians_avgssq"
	la var Iv_varian3_ind_dis "Difference of Indiv. Iv_varians_avgssq"
	
	la var riskaversion_ind "Student 1 NonParam Risk Aversion"
	*la var riskaversion_ind2 "Student 2 NonParam Risk Aversion"
	la var riskaversion_ind_max "Max of Indiv. NonParam Risk Aversions"
	la var riskaversion_ind_dis "Difference of Indiv. NonParam Risk Aversions"
	la var riskaversion_ind_ave "Average of Indiv. NonParam Risk Aversions"
	la var riskpremium_ind "Student 1 Param Risk Premium (CARA)"
	*la var riskpremium_ind2 "Student 2 Param Risk Premium (CARA)"
	la var riskpremium_ind_max "Max of Indiv. Param Risk Premiums (CARA)"
	la var riskpremium_ind_dis "Difference of Indiv. Param Risk Premiums (CARA)"
	*la var riskpremium_ind_ave "Average of Indiv. Param Risk Premiums (CARA)"
	la var riskpremium_crra_ind "Student 1 Param Risk Premium (CRRA)"
	*la var riskpremium_ind2 "Student 2 Param Risk Premium (CRRA)"
	la var riskpremium_crra_ind_max "Max of Indiv. Param Risk Premiums (CRRA)"
	la var riskpremium_crra_ind_dis "Difference of Indiv. Param Risk Premiums (CRRA)"
	*la var riskpremium_ind_ave "Average of Indiv. Param Risk Premiums (CRRA)"
	la var Iv_afriat_col "Pair Iv_afriat"
	la var Iv_varian_col "Pair Iv_varian"
	la var riskaversionpair "Pair of Indiv. Risk Aversion"
	la var riskaversion_col "Pair NonParam Risk Aversion"
	la var riskpremium_col "Pair Param Risk Prevmium (CARA)"
	la var riskpremium_crra_col "Pair Param Risk Prevmium (CRRA)"
	la var uloss_col "Pair Utility Loss (Pareto Loss)"
	la var rdu_col "Pair RDU (EUT, RDU)"
	la var rdupair "Pair of Indiv. Risk Types (EUT, RDU)"
	la var prweightingpair "Pair of Risk Types (EUT, DA, EL)"
	la var prweighting_col "Pair Risk Types (EUT, DA, EL)"
	la var r_ind "Student 1 Rho"
	la var r_ind2 "Student 2 Rho"
	la var r_col "Collective Rho"
	la var rpair "Pair of Rho (CARA)"
	la var mathscore_max "Max of Indiv. Math Scores"
	la var mathscore_dist "Difference of Indiv. Math Scores"
	la var mathscore_ave "Average of Indiv. Math Scores"
	la var malepair "Pair of Male Dummy"
	la var friendship "Friendship within Pair"
	la var class "Class"
	
	
	
	la define prweighting 0 "EUT" 1 "Disappointment Averse" 2 "Elation Loving"
	la val prweighting_col prweighting
	
	
save Risk_ByPair.dta, replace

log c	
