
cls

clear

*--------------------------------------------------------------------------
* Start
*--------------------------------------------------------------------------

*--------------------------------------------------------------------------
* DATA 부르기 & 적절한 샘플만 남기
*--------------------------------------------------------------------------

use Data_DG.dta, replace 

keep if attrition == 0

*--------------------------------------------------------------------------
* bar chart by distance 
*--------------------------------------------------------------------------

replace distance_t = 6 if distance_t > 7 & distance_t <= 11
replace distance_t = 7 if distance_t > 11

preserve

* 그림 1: 사전 데이터 & C & T 따로 작업 & 모든 학교 사용

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

gen high = m_dt_share + invttail(e(df_r),0.025)*se_dt_share
gen low  = m_dt_share - invttail(e(df_r),0.025)*se_dt_share

gen x = distance_t - 0.05     if treatment==1
replace x = distance_t + 0.05 if treatment==0

duplicates drop distance_t treatment t, force

tw ///
(bar m_dt_share x if treatment==1 & distance_t<8 & t==0, barwidth(0.4) fcolor(black%30) lcolor(black) lwidth(medium) ) ///
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
(bar m_dt_share x if treatment==1 & distance_t<8 & t==1, barwidth(0.4) fcolor(black%30) lcolor(black) lwidth(medium) ) ///
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

*** Simple average comparisons

preserve

forval i = 0/1{
	forval j = 0/1{
		reg dt_share if treatment == `i' & t == `j'
		gen trt`i't`j' = _b[_cons]
		gen trt`i't`j'low = _b[_cons] - invttail(e(df_r),0.025)*_se[_cons]
		gen trt`i't`j'high = _b[_cons] + invttail(e(df_r),0.025)*_se[_cons]
	}
}

gen x = 1.2     
replace x = 1.8 if t == 1

gen y = 3.2     
replace y = 3.8 if t == 1

duplicates drop treatment t, force

* draw
tw ///
(bar trt0t0 x if treatment == 0 & t == 0, barwidth(0.4) fcolor(red%30) lcolor(black) lwidth(medium) ) ///
(rcap trt0t0high trt0t0low  x if treatment == 0 & t == 0, lcolor(red) msize(0) lwidth(medium) ) ///
(bar trt0t1 x if treatment == 0 & t == 1, barwidth(0.4) fcolor(red%90)    lcolor(black)   lwidth(medium) ) ///
(rcap trt0t1high trt0t1low  x if treatment == 0 & t == 1, lcolor(red) msize(0) lwidth(medium) ) ///
(bar trt1t0 y if treatment == 1 & t == 0, barwidth(0.4) fcolor(black%30)    lcolor(black)   lwidth(medium) ) ///
(rcap trt1t0high trt1t0low  y if treatment == 1 & t == 0, lcolor(black) msize(0) lwidth(medium) ) ///
(bar trt1t1 y if treatment == 1 & t == 1, barwidth(0.4) fcolor(black%90)    lcolor(black)   lwidth(medium) ) ///
(rcap trt1t1high trt1t1low  y if treatment == 1 & t == 1, lcolor(black) msize(0) lwidth(medium) ) ///
, ///
xlab(1.5 "Control" 3.5 "Treatment") ///
ylab(0.2 (0.05) 0.3) ///
legend(row(2) order(1 "Control Before" 5 "Treatment Before" 3 "Control After" 7 " Treatment After")) ///
ytitle("Average offer as share of total") plotregion(fcolor(white)) graphregion(fcolor(white)) 
gr export Figure_DG_Share_DD.png, height(600) width(800) replace
*window manage close graph
