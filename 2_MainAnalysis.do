log using 2_MainAnalysis_020920.log, replace

********************************************************************************
* WRITTEN BY MINSEON PARK, 020920
* MAIN ANALYSIS 
********************************************************************************

set more off
set matsize 8000


cd "C:\Users\hahn0\Dropbox\RP\Data"
global results_int = "C:\Users\hahn0\Dropbox\RP\Data\Results_Internal"
global results_main = "C:\Users\hahn0\Dropbox\RP\Data\Results_Main"
global results_app = "C:\Users\hahn0\Dropbox\RP\Data\Results_Appendix"


*** Main Specification
global Ivcut = 0.1 
global Imcut = 0.2
global Iscut = 0.1

global Ivcutpair = "Iv_afriat_ind<=$Ivcut & Iv_afriat_ind2<=$Ivcut"
global Imcutpair = "Im_ind<=$Imcut & Im_ind2<=$Imcut"
global Iscutpair = "Is_ind<=$Iscut & Is_ind2<=$Iscut"

global cov1="mathscore_max mathscore_dist"
global cov2="mathscore_max mathscore_dist i.malepair i.friendship"


*****
* TABLE. SUMMARY STATISTICS OF CCEI SCORES / RISK AVERSION / RISKPREMIUM
*****

use Risk_ByIndiv, clear
keep id groupid Iv_afriat_ind Iv_afriat_col riskaversion_ind riskaversion_col Im_ind Im_col riskpremium_ind riskpremium_col Is_ind Is_col rdu_ind rdu_col mover 
ren (Iv_afriat_ind Iv_afriat_col riskaversion_ind riskaversion_col Im_ind Im_col riskpremium_ind riskpremium_col Is_ind Is_col rdu_ind rdu_col) (Iv_afriat1 Iv_afriat2 riskaversion1 riskaversion2 Im1 Im2 riskpremium1 riskpremium2 Is1 Is2 rdu1 rdu2)
reshape long Iv_afriat riskaversion Im riskpremium Is rdu, i(id) j(type) // reshape to get the different # of obs for indiv and pair decision

drop if type==2 & mover =="t"

putexcel set "$results_main\Summary.xlsx", sheet("SummaryStat") replace
putexcel A1=("Game Type") B1=("Observations") C1=("Mean") D1=("Standard Deviation") E1=("p-value") 
local row=2
putexcel A`row' = ("Iv_Afriat")
local row=3
   ttest Iv_afriat, by(type)
putexcel A`row' = ("Individual Decision")
putexcel B`row' = (r(N_1))
putexcel C`row' = (r(mu_1))
putexcel D`row' = (r(sd_1))
putexcel E`row' = (r(p))
local row=4
putexcel A`row' = ("Collective Decision")
putexcel B`row' = (r(N_2))
putexcel C`row' = (r(mu_2))
putexcel D`row' = (r(sd_2))
local row=5
putexcel A`row' = ("Risk Aversion")
local row=6
	ttest riskaversion, by(type)
putexcel A`row' = ("Individual Decision")
putexcel B`row' = (r(N_1))
putexcel C`row' = (r(mu_1))
putexcel D`row' = (r(sd_1))
putexcel E`row' = (r(p))
local row=7
putexcel A`row' = ("Collective Decision")
putexcel B`row' = (r(N_2))
putexcel C`row' = (r(mu_2))
putexcel D`row' = (r(sd_2))
local row=8
putexcel A`row' = ("Risk Premium")
local row=9
putexcel A`row' = ("Complete Sample")
local row=10
	ttest riskpremium, by(type)
putexcel A`row' = ("Individual Decision")
putexcel B`row' = (r(N_1))
putexcel C`row' = (r(mu_1))
putexcel D`row' = (r(sd_1))
putexcel E`row' = (r(p))
local row=11
putexcel A`row' = ("Collective Decision")
putexcel B`row' = (r(N_2))
putexcel C`row' = (r(mu_2))
putexcel D`row' = (r(sd_2))
local row=12
putexcel A`row' = ("I_M <= 0.2")
local row=13
	ttest riskpremium if Im <= $Imcut, by(type)
putexcel A`row' = ("Individual Decision")
putexcel B`row' = (r(N_1))
putexcel C`row' = (r(mu_1))
putexcel D`row' = (r(sd_1))
putexcel E`row' = (r(p))
local row=14
putexcel A`row' = ("Collective Decision")
putexcel B`row' = (r(N_2))
putexcel C`row' = (r(mu_2))
putexcel D`row' = (r(sd_2))
local row=15
putexcel A`row' = ("I_V <= 0.1")
local row=16
	ttest riskpremium if Iv_afriat <= $Ivcut, by(type)
putexcel A`row' = ("Individual Decision")
putexcel B`row' = (r(N_1))
putexcel C`row' = (r(mu_1))
putexcel D`row' = (r(sd_1))
putexcel E`row' = (r(p))
local row=17
putexcel A`row' = ("Collective Decision")
putexcel B`row' = (r(N_2))
putexcel C`row' = (r(mu_2))
putexcel D`row' = (r(sd_2))
local row=18
putexcel A`row' = ("I_M-I_v <= 0.1")
local row=19
	ttest riskpremium if Is <= $Iscut, by(type)
putexcel A`row' = ("Individual Decision")
putexcel B`row' = (r(N_1))
putexcel C`row' = (r(mu_1))
putexcel D`row' = (r(sd_1))
putexcel E`row' = (r(p))
local row=20
putexcel A`row' = ("Collective Decision")
putexcel B`row' = (r(N_2))
putexcel C`row' = (r(mu_2))
putexcel D`row' = (r(sd_2))

local row=21
putexcel A`row' = ("Risk Type")
local row=22
putexcel A`row' = ("Complete Sample")
local row=23
	ttest rdu, by(type) 
putexcel A`row' = ("Individual Decision")
putexcel B`row' = (r(N_1))
putexcel C`row' = (r(mu_1))
putexcel D`row' = (r(sd_1))
putexcel E`row' = (r(p))
local row=24
putexcel A`row' = ("Collective Decision")
putexcel B`row' = (r(N_2))
putexcel C`row' = (r(mu_2))
putexcel D`row' = (r(sd_2))
local row=25
putexcel A`row' = ("I_M <= 0.2")
local row=26
ttest rdu if Im<=$Imcut , by(type)  
putexcel A`row' = ("Individual Decision")
putexcel B`row' = (r(N_1))
putexcel C`row' = (r(mu_1))
putexcel D`row' = (r(sd_1))
putexcel E`row' = (r(p))
local row=27
putexcel A`row' = ("Collective Decision")
putexcel B`row' = (r(N_2))
putexcel C`row' = (r(mu_2))
putexcel D`row' = (r(sd_2))
local row =28
putexcel A`row' = ("I_v<= 0.1 ")
local row= 29
ttest rdu if Iv_afriat<=$Ivcut , by(type)
putexcel A`row' = ("Individual Decision")
putexcel B`row' = (r(N_1))
putexcel C`row' = (r(mu_1))
putexcel D`row' = (r(sd_1))
putexcel E`row' = (r(p))
local row= 30
putexcel A`row' = ("Collective Decision")
putexcel B`row' = (r(N_2))
putexcel C`row' = (r(mu_2))
putexcel D`row' = (r(sd_2))
local row=31
putexcel A`row' = ("I_M-I_v <= 0.1 ")
local row= 32
ttest rdu if Is<=$Iscut, by(type)
putexcel A`row' = ("Individual Decision")
putexcel B`row' = (r(N_1))
putexcel C`row' = (r(mu_1))
putexcel D`row' = (r(sd_1))
putexcel E`row' = (r(p))
local row= 33
putexcel A`row' = ("Collective Decision")
putexcel B`row' = (r(N_2))
putexcel C`row' = (r(mu_2))
putexcel D`row' = (r(sd_2))



*****
* FIGURE. RATIONALITY EXTENSION
*****

use Risk_ByPair.dta, clear
g Iv_afriat_both_hi=(Iv_afriat_ind <= $Ivcut | Iv_afriat_ind2 <= $Ivcut)
replace Iv_afriat_both_hi=2 if $Ivcutpair

cdfplot Iv_afriat_col, by(Iv_afriat_both_hi) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "(High, High)" 2 "(Low, High)" 3 "(Low, Low)") col(1)) ///
xlabel(0(0.1)1) xtitle(Collective Inconsistency) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
gr export "$results_main\CDF_RationalityExtension.png", replace


*****
* TABLE. RATIONALITY EXTENTION-REGRESSION
*****

use Risk_ByPair.dta, clear

qui reg Iv_afriat_col Iv_afriat_ind_min Iv_afriat_ind_dist i.class, vce(robust)
outreg2 using "$results_main\Reg_RationalityExtension.tex", bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace
qui reg Iv_afriat_col Iv_afriat_ind_min Iv_afriat_ind_dist $cov1 i.class, vce(robust)
outreg2 using "$results_main\Reg_RationalityExtension.tex", bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append
qui reg Iv_afriat_col Iv_afriat_ind_min Iv_afriat_ind_dist $cov2 i.class, vce(robust)
outreg2 using "$results_main\Reg_RationalityExtension.tex", bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append


*****
* TABLE. PROBABILITY WEIGHTING TYPE AGGREGATION
*****

use Risk_ByPair.dta, clear

tab rdupair rdu_col, matcell(Tab) 
tab rdupair rdu_col if $Imcutpair & Im_col<=$Imcut , matcell(Tab) 
tab rdupair rdu_col if $Ivcutpair & Iv_afriat_col<=$Ivcut , matcell(Tab) 
tab rdupair rdu_col if $Iscutpair & Is_col<=$Iscut , matcell(Tab) 

qui reg rdu_col i.rdupair i.class, vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(All) addtext(Class FE, YES) replace
qui reg rdu_col i.rdupair $cov1 i.class, vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(All) addtext(Class FE, YES) append
qui reg rdu_col i.rdupair $cov2 i.class, vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(All) addtext(Class FE, YES) append

qui reg rdu_col i.rdupair i.class if $Imcutpair & Im_col<=$Imcut , vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(I_M<=0.2) addtext(Class FE, YES) append
qui reg rdu_col i.rdupair $cov1 i.class if $Imcutpair & Im_col<=$Imcut , vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(I_M<=0.2) addtext(Class FE, YES) append
qui reg rdu_col i.rdupair $cov2 i.class if $Imcutpair & Im_col<=$Imcut , vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(I_M<=0.2) addtext(Class FE, YES) append

qui reg rdu_col i.rdupair i.class if $Ivcutpair & Iv_afriat_col<=$Ivcut , vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(I_V<=0.1) addtext(Class FE, YES) append
qui reg rdu_col i.rdupair $cov1 i.class if $Ivcutpair & Iv_afriat_col<=$Ivcut , vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(I_V<=0.1) addtext(Class FE, YES) append
qui reg rdu_col i.rdupair $cov2 i.class if $Ivcutpair & Iv_afriat_col<=$Ivcut , vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(I_V<=0.1) addtext(Class FE, YES) append

qui reg rdu_col i.rdupair i.class if $Iscutpair & Is_col<=$Iscut , vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(I_V<=0.1) addtext(Class FE, YES) append
qui reg rdu_col i.rdupair $cov1 i.class if $Iscutpair & Is_col<=$Iscut , vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(I_V<=0.1) addtext(Class FE, YES) append
qui reg rdu_col i.rdupair $cov2 i.class if $Iscutpair & Is_col<=$Iscut , vce(robust)
outreg2 using "$results_main\Reg_RiskTypeAggregation.tex", bdec(3) cdec(3) drop(i.class) label ctitle(I_V<=0.1) addtext(Class FE, YES) append



*****
* FIGURE. RISK PREMIUM AGGREGATION
*****

use Risk_ByPair.dta, clear

cdfplot riskpremium_col if riskpremium_col>=0, by(riskpremiumpair) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "(Low, Low)" 2 "(Low, High)" 3 "(High, High)") col(1)) ///
xlabel(0(0.3)1.5) xtitle(Collective Risk Premium) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
gr export "$results_main\CDF_RiskPremiumAggregation.png", replace

cdfplot riskpremium_col if riskpremium_col>=0 & $Imcutpair & Im_col<=$Imcut , by(riskpremiumpair) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "(Low, Low)" 2 "(Low, High)" 3 "(High, High)") col(1)) ///
xlabel(0(0.3)1.5) xtitle(Collective Risk Premium) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
gr export "$results_main\CDF_RiskPremiumAggregation_ImCut.png", replace

cdfplot riskpremium_col if riskpremium_col>=0 & $Ivcutpair & Iv_afriat_col<=$Ivcut , by(riskpremiumpair) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "(Low, Low)" 2 "(Low, High)" 3 "(High, High)") col(1)) ///
xlabel(0(0.3)1.5) xtitle(Collective Risk Premium) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
gr export "$results_main\CDF_RiskPremiumAggregation_IvCut.png", replace

cdfplot riskpremium_col if riskpremium_col>=0 & $Iscutpair & Im_col<=$Iscut , by(riskpremiumpair) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "(Low, Low)" 2 "(Low, High)" 3 "(High, High)") col(1)) ///
xlabel(0(0.3)1.5) xtitle(Collective Risk Premium) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
gr export "$results_main\CDF_RiskPremiumAggregation_IsCut.png", replace

cdfplot riskaversion_col , by(riskaversionpair) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "(Low, Low)" 2 "(Low, High)" 3 "(High, High)") col(1)) ///
xlabel(0(0.2)1) xtitle(Collective Risk Aversion) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
gr export "$results_main\CDF_RiskAversionAggregation.png", replace


*****
* TABLE. RISK AVERSION AGGREGATION-REGRESSION
*****

use Risk_ByPair.dta, clear

reg riskaversion_col riskaversion_ind_max riskaversion_ind_dist i.class, vce(robust)
outreg2 using "$results_main\Reg_RiskAversionAggregation.xls", bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace
reg riskaversion_col riskaversion_ind_max riskaversion_ind_dist $cov1 i.class, vce(robust)
outreg2 using "$results_main\Reg_RiskAversionAggregation.xls", bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append
reg riskaversion_col riskaversion_ind_max riskaversion_ind_dist $cov2 i.class, vce(robust)
outreg2 using "$results_main\Reg_RiskAversionAggregation.xls", bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append


*****
* TABLE. RISK PREMIUM AGGREGATION-REGRESSION
*****

use Risk_ByPair.dta, clear

reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist i.class, vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(All) drop(i.class) label addtext(Class FE, YES) replace
reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist $cov1 i.class, vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(All) drop(i.class) label addtext(Class FE, YES) append
reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist $cov2 i.class, vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(All) drop(i.class) label addtext(Class FE, YES) append

reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist i.class if $Imcutpair & Im_col<=$Imcut , vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(I_M<=0.2) drop(i.class) label addtext(Class FE, YES) append
reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist $cov1 i.class if $Imcutpair & Im_col<=$Imcut , vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(I_M<=0.2) drop(i.class) label addtext(Class FE, YES) append
reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist $cov2 i.class if $Imcutpair & Im_col<=$Imcut , vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(I_M<=0.2) drop(i.class) label addtext(Class FE, YES) append

reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist i.class if $Ivcutpair & Iv_afriat_col<=$Ivcut , vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(I_V<=0.1) drop(i.class) label addtext(Class FE, YES) append
reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist $cov1 i.class if $Ivcutpair & Iv_afriat_col<=$Ivcut , vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(I_V<=0.1) drop(i.class) label addtext(Class FE, YES) append
reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist $cov2 i.class if $Ivcutpair & Iv_afriat_col<=$Ivcut , vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(I_V<=0.1) drop(i.class) label addtext(Class FE, YES) append

reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist i.class if $Iscutpair & Iv_afriat_col<=$Iscut , vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(I_M-I_V<=0.1) drop(i.class) label addtext(Class FE, YES) append
reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist $cov1 i.class if $Iscutpair & Iv_afriat_col<=$Iscut , vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(I_M-I_V<=0.1) drop(i.class) label addtext(Class FE, YES) append
reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist $cov2 i.class if $Iscutpair & Iv_afriat_col<=$Iscut , vce(robust)
outreg2 using "$results_main\Reg_RiskpremiumAggregation.tex", bdec(3) cdec(3) ctitle(I_M-I_V<=0.1) drop(i.class) label addtext(Class FE, YES) append


*****
* FIGURE. HISTOGRAM OF UTILITY LOSS
*****

use Risk_ByPair.dta, clear
twoway (histogram uloss, percent), xtitle(Welfare Loss) graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) )
gr export "$results_main/Hist_WelfareLoss.png", replace

twoway (histogram uloss if $Imcutpair, percent), xtitle(Welfare Loss) graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) )
gr export "$results_main/Hist_WelfareLoss_ImCut.png", replace

twoway (histogram uloss if $Ivcutpair, percent), xtitle(Welfare Loss) graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) )
gr export "$results_main/Hist_WelfareLoss_IvCut.png", replace

twoway (histogram uloss if $Iscutpair, percent), xtitle(Welfare Loss) graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) )
gr export "$results_main/Hist_WelfareLoss_IsCut.png", replace


*****
* FIGURE. UTILITY LOSS - COLLECTIVE RATIONALITY
*****

use Risk_ByPair.dta, clear

g Im_col_lo = cond(Im_col<=$Imcut ,1,0)

cdfplot uloss_col, by(Im_col_lo) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "High" 2 "Low") col(1)) ///
xlabel(0(0.1)1) xtitle(Welfare Loss) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
gr export "$results_main\CDF_WelfareLoss.png", replace

cdfplot uloss_col if $Imcutpair, by(Im_col_lo) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "High" 2 "Low") col(1)) ///
xlabel(0(0.1)1) xtitle(Welfare Loss) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
gr export "$results_main\CDF_WelfareLoss_ImCut.png", replace

cdfplot uloss_col if $Ivcutpair, by(Im_col_lo) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "High" 2 "Low") col(1)) ///
xlabel(0(0.1)1) xtitle(Welfare Loss) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
gr export "$results_main\CDF_WelfareLoss_IvCut.png", replace

cdfplot uloss_col if $Iscutpair, by(Im_col_lo) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "High" 2 "Low") col(1)) ///
xlabel(0(0.1)1) xtitle(Welfare Loss) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
gr export "$results_main\CDF_WelfareLoss_IsCut.png", replace

/*
g Iv_afriat_col_lo = cond(Iv_afriat_col<=$Ivcut ,1,0)

cdfplot uloss_col if $Ivcutpair, by(Iv_afriat_col_lo) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "High" 2 "Low") col(1)) ///
xlabel(0(0.1)1) xtitle(Welfare Loss) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
gr export "$results_main\CDF_WelfareLoss_IvCut.png", replace */


*****
* TABLE. UTILITY LOSS - REGRESSION
*****

use Risk_ByPair.dta, clear

reg uloss_col Iv_afriat_col, r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label replace 
reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair , r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label append
reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2 , r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label append 	

reg uloss_col Iv_afriat_col if $Imcutpair, r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label append
reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair  if $Imcutpair, r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label append
reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2  if $Imcutpair, r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label append 

reg uloss_col Iv_afriat_col if $Ivcutpair, r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label append
reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair  if $Ivcutpair, r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label append
reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2  if $Ivcutpair, r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label append 	

reg uloss_col Iv_afriat_col if $Iscutpair, r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label append
reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair  if $Iscutpair, r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label append
reg uloss_col Iv_afriat_col Iv_afriat_ind_max Iv_afriat_ind_dist riskaversion_ind_max riskaversion_ind_dist i.rdupair $cov2  if $Iscutpair, r cl(class)
outreg2 using "$results_main/Reg_UtilityLoss.tex", bdec(3) cdec(3) ctitle(All) label append 	

log c
