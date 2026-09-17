
clear
cls

*--------------------------------------------------------------------------
* Start
*--------------------------------------------------------------------------

use Data_Net_Homophily.dta, replace 

keep if attrition == 0

***** Balance Checks

preserve

*--------------------------------------------------------------------------
* Table 2: Descriptive Statistics and Baseline Balancing Test
* Panel C
*--------------------------------------------------------------------------

keep if attrition == 0 & t == 0 

reg psc treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, replace dec(3)

reg pnomination treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, append dec(3)

reg p00 treatment if schooltype == 1, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, append dec(3)

reg p10 treatment if schooltype == 1, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, append dec(3)

reg p01 treatment if schooltype == 1, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, append dec(3)

reg p11 treatment if schooltype == 1, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, append dec(3)

reg pnomination treatment if schooltype == 1, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, append dec(3)

restore

*--------------------------------------------------------------------------
* Table 5: PBL Impacts on Homophilic Network Formation
*--------------------------------------------------------------------------

********* Panel A: All schools

reg psc txtreatment t ///
female height weight ///
i.school ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)

reg pnomination txtreatment t ///
female height weight ///
i.school ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)


***** Panel B: Six co-ed schools with mixed gender classrooms

reg psc txtreatment t ///
female height weight ///
i.school ///
if schooltype == 1 ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)

reg p00  txtreatment t ///
female height weight ///
i.school ///
if schooltype == 1 ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)

reg p10  txtreatment t ///
female height weight ///
i.school ///
if schooltype == 1 ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)

reg p01  txtreatment t ///
female height weight ///
i.school ///
if schooltype == 1 ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)

reg p11  txtreatment t ///
female height weight ///
i.school ///
if schooltype == 1 ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)

reg pnomination txtreatment t ///
female height weight ///
i.school ///
if schooltype == 1 ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)





********* Panel B

reg p00  txtreatment t ///
female height weight ///
i.school ///
if schooltype == 2 ///
, cl(school)
outreg2 using Result_Homophily_Change_DD.xls, append dec(3)

reg p10  txtreatment t ///
female height weight ///
i.school ///
if schooltype == 2 ///
, cl(school)
outreg2 using Result_Homophily_Change_DD.xls, append dec(3)

reg psc  txtreatment t ///
female height weight ///
i.school ///
if schooltype == 2 ///
, cl(school)
outreg2 using Result_Homophily_Change_DD.xls, append dec(3)

reg pnomination txtreatment t ///
female height weight ///
i.school ///
if schooltype == 2 ///
, cl(school)
outreg2 using Result_Homophily_Change_DD.xls, append dec(3)

********* Panel C

reg psc txtreatment t ///
height weight ///
i.school ///
if schooltype == 3 ///
, cl(school)
outreg2 using Result_Homophily_Change_DD.xls, append dec(3)

reg pnomination txtreatment t ///
height weight ///
i.school ///
if schooltype == 3 ///
, cl(school)
outreg2 using Result_Homophily_Change_DD.xls, append dec(3)


