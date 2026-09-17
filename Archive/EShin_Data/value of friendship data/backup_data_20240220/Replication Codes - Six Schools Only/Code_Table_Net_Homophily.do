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

reg p00 treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, append dec(3)

reg p10 treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, append dec(3)

reg p01 treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, append dec(3)

reg p11 treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, append dec(3)

reg pnomination treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelC.xls, append dec(3)

restore

*--------------------------------------------------------------------------
* Table 5: PBL Impacts on Homophilic Network Formation
*--------------------------------------------------------------------------

reg psc txtreatment t ///
female height weight ///
i.school ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, replace dec(3)

reg p00  txtreatment t ///
female height weight ///
i.school ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)

reg p10  txtreatment t ///
female height weight ///
i.school ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)

reg p01  txtreatment t ///
female height weight ///
i.school ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)

reg p11  txtreatment t ///
female height weight ///
i.school ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)

reg pnomination txtreatment t ///
female height weight ///
i.school ///
, cl(school)
outreg2 using Table5_Homophily_Change_DD.xls, append dec(3)
