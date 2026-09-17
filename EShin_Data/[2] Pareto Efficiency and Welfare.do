
*-------------------------------------------------------------------------------
*             Utility Loss from Pareto Inefficient Collective Decision
*------------------------------------------------------------------------------;

 use Risk_ByPair.dta, clear 
 
 twoway (histogram uloss_col if ccei_both90==2, percent bin(25) ) , xlabel(0(0.05)1) ///
 note(`"Including only both individuals CCEIs>=0.9"') xtitle(Group Choices' Inefficiency) ///
 ytitle(Fraction of subjects(%))
 gr export Figure8.png, replace

  twoway (histogram uloss_col if ccei_both90==2, percent bin(18) fcolor(bluishgray) lcolor(navy)) , xlabel(0(0.2)1) ///
 subtitle(CARA 효용함수 가정) note(`"둘 모두의 개인 합리성이 0.9이상인 경우만 포함함"') xtitle(그룹별 효용손실)
 gr export uloss_col90.png, replace

 ksmirnov uloss_col if ccei_both90==2, by(ccei_col90)
 local d1=trim("`: display %9.2f r(D)'")
 local p1=trim("`: display %9.2f r(p_cor)'")

 cdfplot uloss_col if ccei_both90==2, by(ccei_col90) opt1( lc(teal olive navy) lp(solid dash dot) ) ///
 legend(order(1 "Collective CCEI<0.90(N=61)" 2 "Collective CCEI≥0.90(N=259)") col(1)) ///
 xtitle(Group Choices' Inefficiency)  xlabel(0(0.05)1) ///
 note(`"Kolmogorov-Smirnov Distance(corrected p-value):`d1'(`p1')"' )
 gr export Figure9.png, replace

ksmirnov uloss_col if riskpremiumpair~=2&ccei_both90==2, by(riskpremiumpair)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov uloss_col if riskpremiumpair~=1&ccei_both90==2, by(riskpremiumpair)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot uloss_col if ccei_both90==2, by(riskpremiumpair) opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Hetero Pairs(N=161)" 2 "Both Less Risk Averse Pairs(N=90)" 3 "Both Risk Averse Pairs(N=69)") col(1)) ///
xtitle(Average of Utility Loss By Pairs)  xlabel(0(0.1)1) ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Hetero=Both Risk Averse) `d1'(`p1') 2. (Hetero=Both Less Risk Averse) `d2'(`p2')"' `"Only included both CCEIs>=0.90"' ///
`"Being risk averse is defined as individual non-parametric risk attitude is below the median"')
gr export premium_uloss_col_rat.png, replace


g aggregation_ra=cond(riskpremiumpair==2&hiriskpremium_col==1|riskpremiumpair==1&hiriskpremium_col==0,2,.)
replace aggregation_ra=0 if riskpremiumpair==2&hiriskpremium_col==0|riskpremiumpair==1&hiriskpremium_col==1
replace aggregation_ra=1 if riskpremiumpair==0

ksmirnov uloss_col if riskpremiumpair~=2&ccei_both90==2, by(riskpremiumpair)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov uloss_col if riskpremiumpair~=1&ccei_both90==2, by(riskpremiumpair)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")


cdfplot uloss_col if ccei_both90==2, by(aggregation_ra) opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Not aggregated" 2 "Heterogenous" 3 "Aggregated") col(1)) ///
xtitle(Average of Utility Loss By Pairs)  xlabel(0(0.1)1) ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Hetero=Aggregated) `d1'(`p1') 2. (Hetero=Not aggregated) `d2'(`p2')"' `"Only included both CCEIs>=0.90"' ///
`"Being risk averse is defined as individual non-parametric risk attitude is below the median"')
gr export premiumaggregation_uloss_col_rat.png, replace


reg uloss_col ccei_col riskpref_ind_ave riskpref_ind_dist if ccei_both90==2, r cl(class)
outreg2 using Efficiency.xls, bdec(3) cdec(3) label replace 	

reg uloss_col ccei_col riskpref_ind_ave riskpref_ind_dist ///
	ib(1).sexpair_all mathscore_ave mathscore_dist i.friendship ///
	indegree_n_ave indegree_n_dist outdegree_n_ave outdegree_n_dist ///
	outgoing_ave outgoing_dist agreeable_ave agreeable_dist stable_ave stable_dist ///
	conscientious_ave conscientious_dist openness_ave openness_dist	if ccei_both90==2, r cl(class)
outreg2 using Efficiency.xls, bdec(3) cdec(3) label  	
	
reg uloss_col ccei_col riskpremium_ind_ave riskpremium_ind_dist if ccei_both90==2, r cl(class)
outreg2 using Efficiency.xls, bdec(3) cdec(3) label 
	
reg uloss_col ccei_col riskpremium_ind_ave riskpremium_ind_dist ///
	ib(1).sexpair_all mathscore_ave mathscore_dist i.friendship ///
	indegree_n_ave indegree_n_dist outdegree_n_ave outdegree_n_dist ///
	outgoing_ave outgoing_dist agreeable_ave agreeable_dist stable_ave stable_dist ///
	conscientious_ave conscientious_dist openness_ave openness_dist	if ccei_both90==2, r cl(class)
outreg2 using Efficiency.xls, bdec(3) cdec(3) label 

********************************************************************************
*                Individuals' Utility loss from Collective Decision
********************************************************************************

 use Risk_ByIndiv.dta, clear
 twoway (histogram uloss_ind if ccei_ind90==1, percent bin(18) fcolor(bluishgray) lcolor(navy)) , xlabel(0(0.2)1) ///
 subtitle(CARA Utility Function) note(`"Including individuals whose CCEI>=0.9"') xtitle(Average of Utility Loss From Collective Choice)
 gr export uloss_ind90.png, replace
 
 twoway (scatter ccei_com ccei_min), xtitle("min(CCEI_individual, CCEI_collective)") ytitle(CCEI_Combined)
 
 twoway (histogram ccei_dif, percent bin(18) fcolor(bluishgray) lcolor(navy)) ,  ///
 xtitle(Minimum CCEI-Combined CCEI)

 twoway (histogram difdif, percent bin(18) fcolor(bluishgray) lcolor(navy)) ,  ///
 xtitle(Difference in CCEI_min-CCEI_com)
 gr export difdif.png, replace

  use Risk_ByPair.dta, clear

  twoway (scatter uloss_ind uloss_ind2 if ccei_both90==2) (scatteri 1 1 0 0, recast(line) lcolor(eltblue) lwidth(thin)), ///
  xtitle(uloss_indiv 1) ytitle(uloss_indiv 2) legend(off) //
  subtitle(CARA Utility Function) note(`"Including only both individuals CCEIs>=0.9"') 
  gr export uloss_ind_scatter.png, replace
  
  twoway (histogram imbalance_p if ccei_both90==2, percent bin(18) fcolor(bluishgray) lcolor(navy)) , xlabel(0(0.2)1) ///
 subtitle(CARA Utility Function) note(`"Including only both individuals CCEIs>=0.9"')  xtitle(Imbalance of Group Decision_Parametric)
 gr export imbalance_parm.png, replace	
 
reg imbalance_p  ccei_col i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using Mechanism_Imbalance.xls, bdec(2) cdec(2) label ctitle(Imbalance)
reg imbalance_p i.riskprefpair  i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using Mechanism_Imbalance.xls, bdec(2) cdec(2) label ctitle(Imbalance)
reg imbalance_p i.riskprefpair ccei_col i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using Mechanism_Imbalance.xls, bdec(2) cdec(2) label ctitle(Imbalance)
reg imbalance_p i.riskpremiumpair ccei_col i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using Mechanism_Imbalance.xls, bdec(2) cdec(2) label ctitle(Imbalance)


  twoway (scatter ccei_dif ccei_dif2) (scatteri 1 1 0 0, recast(line) lcolor(eltblue) lwidth(thin)), ///
  xtitle(uloss_indiv 1) ytitle(uloss_indiv 2) legend(off) //

  
  twoway (histogram imbalance_np , percent bin(18) fcolor(bluishgray) lcolor(navy)) , xlabel(0(0.2)1) ///
 xtitle(Imbalance of Group Decision_NonParametric)

reg imbalance_np ccei_col i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair , r cl(class)
outreg2 using Mechanism_Imbalance.xls, bdec(2) cdec(2) label ctitle(Imbalance)
reg imbalance_np i.riskprefpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair , r cl(class)
outreg2 using Mechanism_Imbalance.xls, bdec(2) cdec(2) label ctitle(Imbalance)
reg imbalance_np i.riskprefpair ccei_col  i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair , r cl(class)
outreg2 using Mechanism_Imbalance.xls, bdec(2) cdec(2) label ctitle(Imbalance)
reg imbalance_np i.riskpremiumpair ccei_col  i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair , r cl(class)
outreg2 using Mechanism_Imbalance.xls, bdec(2) cdec(2) label ctitle(Imbalance)
