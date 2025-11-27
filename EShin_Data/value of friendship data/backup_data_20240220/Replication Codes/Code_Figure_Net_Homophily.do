
cls

clear

*--------------------------------------------------------------------------
* Start
*--------------------------------------------------------------------------

*--------------------------------------------------------------------------
* DATA 부르기 & 적절한 샘플만 남기
*--------------------------------------------------------------------------

use Data_Net_Homophily, replace 

keep if attrition == 0

***

preserve

reg p00  txtreatment t ///
female height weight ///
i.school ///
if schooltype == 1 ///
, cl(school)
gen m00 = _b[txtreatment] 
gen m00u = m00 + invttail(e(df_r),0.025)*_se[txtreatment]
gen m00l = m00 - invttail(e(df_r),0.025)*_se[txtreatment]

gen xlabs00 = 1

***

reg p10  txtreatment t ///
female height weight ///
i.school ///
if schooltype == 1 ///
, cl(school)
gen m10 = _b[txtreatment] 
gen m10u = m10 + invttail(e(df_r),0.025)*_se[txtreatment]
gen m10l = m10 - invttail(e(df_r),0.025)*_se[txtreatment]

gen xlabs10 = 3

***

reg p01  txtreatment t ///
female height weight ///
i.school ///
if schooltype == 1 ///
, cl(school)
gen m01 = _b[txtreatment] 
gen m01u = m01 + invttail(e(df_r),0.025)*_se[txtreatment]
gen m01l = m01 - invttail(e(df_r),0.025)*_se[txtreatment]

gen xlabs01 = 5

***

reg p11  txtreatment t ///
female height weight ///
i.school ///
if schooltype == 1 ///
, cl(school)
gen m11 = _b[txtreatment] 
gen m11u = m11 + invttail(e(df_r),0.025)*_se[txtreatment]
gen m11l = m11 - invttail(e(df_r),0.025)*_se[txtreatment]

gen xlabs11 = 7


***

gen mt00 = -0.814
gen mt00u = 2.738
gen mt00l = -4.008
gen xlabst00 = 11

gen mt10 = -0.870
gen mt10u = -0.752
gen mt10l = -1.120
gen xlabst10 = 13

gen mt01 = -1.289
gen mt01u = 2.411
gen mt01l = -5.064
gen xlabst01 = 15

gen mt11 = -0.989
gen mt11u = -0.489
gen mt11l = -1.214
gen xlabst11 = 17

***

keep if id == 1110102 /* This is to increase drawing speed */

twoway (scatter m00 xlabs00 , msymbol(o) mcolor(black) yaxis(1) ) ///
       (rcap m00u m00l xlabs00, lcolor(red) msize(0) lwidth(medium) yaxis(1) ) ///
	   (scatter m10 xlabs10 , msymbol(o) mcolor(black)  yaxis(1) ) ///
       (rcap m10u m10l xlabs10, lcolor(red) msize(0) lwidth(medium) yaxis(1) ) ///
	   (scatter m01 xlabs01 , msymbol(o) mcolor(black)  yaxis(1) ) ///
       (rcap m01u m01l xlabs01, lcolor(red) msize(0) lwidth(medium) yaxis(1) ) ///
	   (scatter m11 xlabs11 , msymbol(o) mcolor(black)  yaxis(1) ) ///
       (rcap m11u m11l xlabs11, lcolor(red) msize(0) lwidth(medium) yaxis(1) ) ///
	   , ytitle("Change of Friendship Composition", axis(1)) ysc(axis(1) r(-0.1 0.1)) ///
	     ylabel(-0.1(0.05)0.1, axis(1)) ///
	     xlabel(1  "P(dg,dc)"  3 "P(sg,dc)" 5 "P(dg,sc)"  7 "P(sg,sc)"  ///
11  "{&theta}(dg,dc)"  13 "{&theta}(sg,dc)" 15 "{&theta}(dg,sc)"  17 "{&theta}(sg,sc)", labsize(small)) xtitle("") xscale(axis(1) range(0 18)) ///
		 legend(row(2) order(1 "Difference-in-Differences Estimate" 2 "95% Confidence Interval")) || ///
	   (scatter mt00 xlabst00 , msymbol(o) mcolor(black) yaxis(2) ) ///
       (rcap mt00u mt00l xlabst00, lcolor(red) msize(0) lwidth(medium) yaxis(2) ) ///
	   (scatter mt10 xlabst10 , msymbol(o) mcolor(black) yaxis(2) ) ///
       (rcap mt10u mt10l xlabst10, lcolor(red) msize(0) lwidth(medium) yaxis(2) ) ///
	   (scatter mt01 xlabst01 , msymbol(o) mcolor(black) yaxis(2) ) ///
       (rcap mt01u mt01l xlabst01, lcolor(red) msize(0) lwidth(medium) yaxis(2) ) ///
	   (scatter mt11 xlabst11 , msymbol(o) mcolor(black) yaxis(2) ) ///
       (rcap mt11u mt11l xlabst11, lcolor(red) msize(0) lwidth(medium) yaxis(2) ) ///
	   , ytitle("Change of Link Formation Cost Parameter", axis(2)) ysc(axis(2) r(-5 5)) ///
	     ylabel(-5(2.5)5, axis(2)) ///
		 yline(0, axis(2) lcolor(gray) lstyle(grid)) ///
		 plotregion(fcolor(white)) graphregion(fcolor(white))

gr export Figure_Homophily_DD.png, height(600) width(800) replace
*window manage close graph

restore

***

preserve

reg psc txtreatment t ///
female height weight ///
i.school ///
, cl(school)

gen m00 = _b[txtreatment] 
gen m00u = m00 + invttail(e(df_r),0.025)*_se[txtreatment]
gen m00l = m00 - invttail(e(df_r),0.025)*_se[txtreatment]

gen xlabs00 = 1

***

gen mt00 = -0.452
gen mt00u = -0.248
gen mt00l = -0.592
gen xlabst00 = 3

gen mt11 = -0.653
gen mt11u = -0.253
gen mt11l = -1.047
gen xlabst11 = 4

***

keep if id == 1110102 /* This is to increase drawing speed */

twoway (scatter m00 xlabs00 , msymbol(o) mcolor(black) yaxis(1) ) ///
       (rcap m00u m00l xlabs00, lcolor(red) msize(0) lwidth(medium) yaxis(1) ) ///
	   , ytitle("Change of Friendship Composition", axis(1)) ysc(axis(1) r(-0.1 0.1)) ///
	     ylabel(-0.08(0.04)0.08, axis(1)) ///
	     xlabel(1  "P(sc)" 3  "{&theta}(dc)" 4 "{&theta}(sc)", labsize(small)) xtitle("") xscale(axis(1) range(0 5)) ///
		 legend(row(2) order(1 "Difference-in-Differences Estimate" 2 "95% Confidence Interval")) || ///
	   (scatter mt00 xlabst00 , msymbol(o) mcolor(black) yaxis(2) ) ///
       (rcap mt00u mt00l xlabst00, lcolor(red) msize(0) lwidth(medium) yaxis(2) ) ///
	   (scatter mt11 xlabst11 , msymbol(o) mcolor(black) yaxis(2) ) ///
       (rcap mt11u mt11l xlabst11, lcolor(red) msize(0) lwidth(medium) yaxis(2) ) ///
	   , ytitle("Change of Link Formation Cost Parameter", axis(2)) ysc(axis(2) r(-1.2 1.2)) ///
	     ylabel(-1.0(0.5)1.0, axis(2)) ///
		 yline(0, axis(2) lcolor(gray) lstyle(grid)) ///
		 plotregion(fcolor(white)) graphregion(fcolor(white))

gr export Figure_Homophily_DD2.png, height(600) width(800) replace

restore
