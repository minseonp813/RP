********************************************************************************
* Written by Minseon Park, 04/25/20
*
********************************************************************************


set more off
set matsize 8000


cd "C:\Users\minseon\Dropbox\RP\Data"
global results_int = "C:\Users\minseon\Dropbox\RP\Data\Results_Internal"
global results_main = "C:\Users\minseon\Dropbox\RP\Data\Results_Main"
global results_app = "C:\Users\minseon\Dropbox\RP\Data\Results_Appendix"


*** Main Specification
global Ivcut = 0.1 
global Imcut = 0.2
global Iscut = 0.1

global Ivcutpair = "Iv_afriat_ind<=$Ivcut & Iv_afriat_ind2<=$Ivcut"
global Imcutpair = "Im_ind<=$Imcut & Im_ind2<=$Imcut"
global Iscutpair = "Is_ind<=$Iscut & Is_ind2<=$Iscut"

global cov1="mathscore_max mathscore_dist"
global cov2="mathscore_max mathscore_dist i.malepair i.friendship"


use Risk_ByPair.dta, clear

qui reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2 
outreg2 using "$results_main/PE_042520.xls", replace bdec(3) cdec(3) ctitle(all) label
estat esize
*matrix list r(esize)
qui reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2  if $Imcutpair
outreg2 using "$results_main/PE_042520.xls", bdec(3) cdec(3) ctitle(Iv<=0.1) label append 
estat esize
qui reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2  if $Ivcutpair
outreg2 using "$results_main/PE_042520.xls", bdec(3) cdec(3) ctitle(Im<=0.2) label append 	
estat esize
qui reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2  if $Iscutpair
outreg2 using "$results_main/PE_042520.xls", bdec(3) cdec(3) ctitle(Is<=0.1) label append 	
estat esize

log using PE_decomposition.log
pcorr  uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2 
pcorr  uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2  if $Imcutpair
pcorr  uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2  if $Ivcutpair
pcorr  uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2  if $Iscutpair
log c

******************
* simulation exercise
******************

clear all
set obs 5000
g u1 = rnormal()
g u2 = rnormal()
g u3 = rnormal()
g u4 = rnormal()

g x1 = 0.5*u1 + 0.5*u2
g x2 = 0.3*u1 + 0.7*u3
g x3 = 0.2*u1 + 0.8*u4

g y = 0.4 *x1 + 0.2 * x2 + 0.1 * x3 + rnormal()

reg y x1 x2 x3
estat esize

/*

-----------------------------------------------------------------
             Source | Eta-Squared     df     [95% Conf. Interval]
--------------------+--------------------------------------------
              Model |   .1189959       3     .1026093    .1352433
                    |
                 x1 |    .062254       1     .0499987    .0755001
                 x2 |    .021069       1     .0139162    .0295639
                 x3 |   .0090395       1     .0045622    .0149685
----------------------------------------------------------------- */


reg y x2 x3
predict uy1, resid
reg x1 x2 x3
predict ux1, resid

reg y x1 x3
predict uy2, resid
reg x2 x1 x3
predict ux2, resid

reg y x1 x2
predict uy3, resid
reg x3 x1 x2
predict ux3, resid

pwcorr uy1 ux1 uy2 ux2 uy3 ux3

corr uy1 ux1 uy2 ux2 uy3 ux3, c

pcorr y x1 x2 x3

0.2633
0.1405
0.0847
