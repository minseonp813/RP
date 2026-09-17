**************************************************************************************

cls
clear

*--------------------------------------------------------------------------
* Read the data file
*--------------------------------------------------------------------------

use Data_DG.dta, replace

*--------------------------------------------------------------------------
* Attrition rate analysis
*--------------------------------------------------------------------------

* TBC...

*--------------------------------------------------------------------------
* Balance Checks
*--------------------------------------------------------------------------

* Panel A

preserve
keep if attrition == 0 & t == 0 
duplicates drop id, force

reg female treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, replace dec(3)

reg height treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg weight treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg motherage treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg fatherage treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg motheredu treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg fatheredu treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg birthorder treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg firstborn treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg mathscore treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg selfesteem treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg outgoing treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg agreeable treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg conscientious treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg stable treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg opened treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

restore

* Panel B

preserve
keep if attrition == 0 & t == 0 
*duplicates drop id, force
			
reg distance_1 treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg distance_2 treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg distance_3 treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg distance_4 treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg distance_5 treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg distance_6to11 treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg distance_12 treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

reg dt_share treatment, cl(school)
outreg2 using Result_BalanceChecks_PanelAB.xls, append dec(3)

restore

*--------------------------------------------------------------------------
* DD Analysis
*--------------------------------------------------------------------------

preserve

keep if attrition == 0

***** DD analysis 1
reg dt_share txtreatment t ///
i.school ///
, cl(id)
outreg2 using Result_SimpleDD.xls, replace dec(3)

***** DD analysis 2: + DEMO & SES
reg dt_share txtreatment t ///
female height weight motherage fatherage motheredu fatheredu firstborn ///
i.school ///
, cl(id)
outreg2 using Result_SimpleDD.xls, append dec(3)

***** DD analysis 3: + COG & NON-COG
reg dt_share txtreatment t ///
female height weight motherage fatherage motheredu fatheredu firstborn ///
mathscore selfesteem outgoing agreeable conscientious stable opened ///
i.school ///
, cl(id)
outreg2 using Result_SimpleDD.xls, append dec(3)

***** DD analysis 4: + Friendship and homophily
reg dt_share txtreatment t ///
female height weight motherage fatherage motheredu fatheredu firstborn ///
mathscore selfesteem outgoing agreeable conscientious stable opened ///
same_height same_weight same_classroom same_sex ///
i.school ///
, cl(id)
outreg2 using Result_SimpleDD.xls, append dec(3)

***** DD analysis 5: + distance_t 
replace distance_t = 12 if distance_t > 12

reg dt_share txtreatment t  /// 
female height weight motherage fatherage motheredu fatheredu firstborn ///
mathscore selfesteem outgoing agreeable conscientious stable opened ///
same_height same_weight same_classroom same_sex ///
distance_t ///
i.school ///
, cl(id)
outreg2 using Result_SimpleDD.xls, append dec(3)

restore

*--------------------------------------------------------------------------
* PBL Impacts by Social Distance
*--------------------------------------------------------------------------

* note: distance_time0과 distance_time1의 observation 숫자 확인할 것
* distance_time1 == . 인 경우는 사전에 매칭된 상대방이 사후에 전학간 경우 / 예 1110605
* 따라서 attrition과는 달리 사전 사후 둘다 남아 있는 학생들에 대해서 조사할 필요가 있다. 
* 아래 결과 일부 변화 because of sample size: 김부열 교수님과 재확인 
* 재 확인 후 아래 변수 저장하고 데이터 확정.

preserve

keep if attrition == 0

generate distance_01 = .
replace distance_01 = 0 if distance_time0 != .
replace distance_01 = 1 if distance_time0 == 1

generate distance_02 = .
replace distance_02 = 0 if distance_time0 != .
replace distance_02 = 1 if distance_time0 == 2

generate distance_03 = .
replace distance_03 = 0 if distance_time0 != .
replace distance_03 = 1 if distance_time0 == 3

generate distance_04 = .
replace distance_04 = 0 if distance_time0 != .
replace distance_04 = 1 if distance_time0 == 4

generate distance_05 = .
replace distance_05 = 0 if distance_time0 != .
replace distance_05 = 1 if distance_time0 == 5

generate distance_06to11 = .
replace distance_06to11 = 0 if distance_time0 != .
replace distance_06to11 = 1 if distance_time0 >= 6 & distance_time0 <= 11

***** DD analysis: 
reg dt_share txtreatment t /// 
female height weight motherage fatherage motheredu fatheredu firstborn ///
mathscore selfesteem outgoing agreeable conscientious stable opened ///
same_height same_weight same_classroom same_sex ///
i.school ///
distance_01 distance_02 distance_03 distance_04 distance_05 distance_06to11 ///
if distance_time1 == 1 ///
, cl(id)
outreg2 using Result_ImpactsbyDistance.xls, replace dec(3)

***** DD analysis: 
reg dt_share txtreatment t /// 
female height weight motherage fatherage motheredu fatheredu firstborn ///
mathscore selfesteem outgoing agreeable conscientious stable opened ///
same_height same_weight same_classroom same_sex ///
i.school ///
distance_01 distance_02 distance_03 distance_04 distance_05 distance_06to11 ///
if distance_time1 == 2 ///
, cl(id)
outreg2 using Result_ImpactsbyDistance.xls, append dec(3)

***** DD analysis: 
reg dt_share txtreatment t /// 
female height weight motherage fatherage motheredu fatheredu firstborn ///
mathscore selfesteem outgoing agreeable conscientious stable opened ///
same_height same_weight same_classroom same_sex ///
i.school ///
distance_01 distance_02 distance_03 distance_04 distance_05 distance_06to11 ///
if distance_time1 == 3 ///
, cl(id)
outreg2 using Result_ImpactsbyDistance.xls, append dec(3)

***** DD analysis: 
reg dt_share txtreatment t /// 
female height weight motherage fatherage motheredu fatheredu firstborn ///
mathscore selfesteem outgoing agreeable conscientious stable opened ///
same_height same_weight same_classroom same_sex ///
i.school ///
distance_01 distance_02 distance_03 distance_04 distance_05 distance_06to11 ///
if distance_time1 == 4 ///
, cl(id)
outreg2 using Result_ImpactsbyDistance.xls, append dec(3)

***** DD analysis: 
reg dt_share txtreatment t /// 
female height weight motherage fatherage motheredu fatheredu firstborn ///
mathscore selfesteem outgoing agreeable conscientious stable opened ///
same_height same_weight same_classroom same_sex ///
i.school ///
distance_01 distance_02 distance_03 distance_04 distance_05 distance_06to11 ///
if distance_time1 == 5 ///
, cl(id)
outreg2 using Result_ImpactsbyDistance.xls, append dec(3)

***** DD analysis 5:
reg dt_share txtreatment t /// 
female height weight motherage fatherage motheredu fatheredu firstborn ///
mathscore selfesteem outgoing agreeable conscientious stable opened ///
same_height same_weight same_classroom same_sex ///
i.school ///
distance_01 distance_02 distance_03 distance_04 distance_05 distance_06to11 ///
if distance_time1 >=6 & distance_time1 <= 11 ///
, cl(id)
outreg2 using Result_ImpactsbyDistance.xls, append dec(3)

***** DD analysis 5: 
reg dt_share txtreatment t /// 
female height weight motherage fatherage motheredu fatheredu firstborn ///
mathscore selfesteem outgoing agreeable conscientious stable opened ///
same_height same_weight same_classroom same_sex ///
i.school ///
distance_01 distance_02 distance_03 distance_04 distance_05 distance_06to11 ///
if distance_time1 > 11 ///
, cl(id)
outreg2 using Result_ImpactsbyDistance.xls, append dec(3)

***** Baseline averages
reg dt_share if t == 0 & treatment == 0 & distance_t == 1
matrix define m1 = e(b)[1,1]
reg dt_share if t == 0 & treatment == 0 & distance_t == 2
matrix define m2 = e(b)[1,1]
reg dt_share if t == 0 & treatment == 0 & distance_t == 3
matrix define m3 = e(b)[1,1]
reg dt_share if t == 0 & treatment == 0 & distance_t == 4
matrix define m4 = e(b)[1,1]
reg dt_share if t == 0 & treatment == 0 & distance_t == 5
matrix define m5 = e(b)[1,1]
reg dt_share if t == 0 & treatment == 0 & distance_t >=6 & distance_t <= 11
matrix define m6 = e(b)[1,1]
reg dt_share if t == 0 & treatment == 0 & distance_t > 11
matrix define m7 = e(b)[1,1]

matrix define ms = [m1\ m2\ m3\ m4\ m5\ m6\ m7]
matrix list ms

restore

***** Figure

preserve

keep if attrition == 0

*--------------------------------------------------------------------------
* bar chart by distance 
*--------------------------------------------------------------------------

replace distance_t = 6 if distance_t > 7 & distance_t <= 11
replace distance_t = 7 if distance_t > 11

gen m_dt_share=.
gen se_dt_share=.

forval z = 1/7{
  forval i = 0/1{
    forval j = 0/1{

      reg dt_share if t==`i' & treatment==`j' & distance_t==`z'

      replace m_dt_share=_b[_cons]   if t==`i' & treatment==`j' & distance_t==`z'
      replace se_dt_share=_se[_cons] if t==`i' & treatment==`j' & distance_t==`z'

    }
  }
}

gen high = m_dt_share + se_dt_share
gen low  = m_dt_share - se_dt_share

gen x = distance_t - 0.05     if treatment==1
replace x = distance_t + 0.05 if treatment==0

tw ///
(bar m_dt_share x if treatment==1 & distance_t<8 & t==0, barwidth(0.4) fcolor(gray%30) lcolor(black) lwidth(medium) ) ///
(rcap high low  x if treatment==1 & distance_t<8 & t==0, lcolor(black) msize(0) lwidth(medium) ) ///
(bar m_dt_share x if treatment==0 & distance_t<8 & t==0, barwidth(0.4) fcolor(none)    lcolor(red)   lwidth(medium) ) ///
(rcap high low  x if treatment==0 & distance_t<8 & t==0, lcolor(red) msize(0) lwidth(medium) ) ///
, ///
xtic(0.5/7.5) xlab(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6-11" 7 "Undefined" ) ///
ylab(0 (0.1) 0.5) ///
legend(row(1) order(1 "Treatment" 3 "Control")) ///
xtitle("Social distance") ytitle("Average offer as share of total") plotregion(fcolor(white)) graphregion(fcolor(white))
gr export Figure_1overd_time0.png, height(600) width(800) replace
window manage close graph

tw ///
(bar m_dt_share x if treatment==1 & distance_t<8 & t==1, barwidth(0.4) fcolor(gray%30) lcolor(black) lwidth(medium) ) ///
(rcap high low  x if treatment==1 & distance_t<8 & t==1, lcolor(black) msize(0) lwidth(medium) ) ///
(bar m_dt_share x if treatment==0 & distance_t<8 & t==1, barwidth(0.4) fcolor(none)    lcolor(red)   lwidth(medium) ) ///
(rcap high low  x if treatment==0 & distance_t<8 & t==1, lcolor(red) msize(0) lwidth(medium) ) ///
, ///
xtic(0.5/7.5) xlab(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6-11" 7 "Undefined" ) ///
ylab(0 (0.1) 0.5) ///
legend(row(1) order(1 "Treatment" 3 "Control")) ///
xtitle("Social distance") ytitle("Average offer as share of total") plotregion(fcolor(white)) graphregion(fcolor(white))
gr export Figure_1overd_time1.png, height(600) width(800) replace
window manage close graph

restore
