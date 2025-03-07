
log using 1_ParetoOptimalChoices_020920.log, replace

********************************************************************************
* WRITTEN BY minseonON PARK, 12-23-2019
* TO UNDERSTAND All BIZARRE CASES WITH THE ORDER OF UTILITIES
* WITHOUT ANY ERROR, THE ORDER SHOULD BE uii <= uipec <= uic <= uiw
* TWO MAIN CASES: 1) PRECISION PROBLEM. NOT A BIG DEAL BECAUSE THE LOSS VARIABLE 
* WILL BE VERY SMALL 2) OBSERVED JOINT CHOICE VIOLATES FOSD. THEN FOR BOTH 
* CARA, CRRA, NUMERICAL SEARCH SHOULD BE TAKED PLACE TO FIND A PARETO OPTIMAL
* DECISION.
********************************************************************************

set more off
set matsize 8000

cd "C:\Users\hahn0\Dropbox\RP\Data"
global mmi = "C:\Users\hahn0\Dropbox\RP\Estimation_Money Metric Index\Code Package"
global source = "C:\Users\hahn0\Dropbox\RP\Data\SourceDataSets"
global mmi_post = "C:\Users\hahn0\Dropbox\RP\Estimation_Money Metric Index\Post Estimation_MS"

*** 1. uii < uipec |  uii < uic 
*** no case where uii < uiw
*** conclusion: the difference is very minor. coming from loosing prevision when Stata saves something

sum uii, detail
sum uipec, detail
sum uiw, detail

use Risk_ByChoice.dta, clear
keep if uii < uipec |  uii < uic // N=6

tw (scatter optx_ind x_pe if uii < uipec )  (function y=x, range(500 1700)),  ///
	xlabel(500(200)1700) xtitle("Pareto Optimal x") ///
	ylabel(500(200)1700) ytitle("Observed x")
	
tw (scatter optx_ind coord_x if uii < uic )  (function y=x, range(500 1700)),  ///
	xlabel(500(200)1700) xtitle("Pareto Optimal x") ///
	ylabel(500(200)1700) ytitle("Observed x")

*** 2. uipec < uic 
*** no case where uipec < uiw | uic < uiw
*** conclusion (1): most of the cases is coming from precision problem
*** conclusion (2): observed choice violate fosd + elation loving -> numerical PE solution

use Risk_ByChoice.dta, clear
keep if (uipec < uic) & pe==0 // N=186 cases
	   
tw (scatter x_pe coord_x)  (function y=x, range(500 1700)),  ///
	xlabel(500(200)1700) xtitle("Pareto Optimal x") ///
	ylabel(500(200)1700) ytitle("Observed x")
	
global trembling = 50

g relconsmp_uipec = y_pe/(x_pe + y_pe)

twoway (scatter relconsmp lnrelprice if uipec < uic & abs(x_pe-coord_x)>$trembling, msize(large) msymbol(circle_hollow) mcolor(red) ) ///
	   (scatter relconsmp_uipec lnrelprice if uipec < uic & abs(x_pe-coord_x)>$trembling, msize(large) msymbol(triangle_hollow) mcolor(blue) ), ///
	   legend(order(1 "Observed Choice" 2 "Pareto Optimal") ///
		col(1) position(8) ring(0) size(4)) ///
	   xlabel(-2(0.5)2, labsize(4)) xtitle("Log price ratio (log(p{subscript:2}/p{subscript:1}))", size(4.5) height(4)) ///
       ylabel(0(0.2)1, labsize(4)) ytitle("Share for the second security (x{subscript:2}/(x{subscript:2}+x{subscript:1}))", size(4.5) height(4)) ///
	   xline(2, lcolor(black)) /// *box
	   yline(1, lcolor(black)) /// *box
	   xline(0, lcolor(blue) lstyle(grid)) yline(0.5, lcolor(blue)) /// 
	   graphregion(color(white) lcolor(black)  ) ///
       plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 	   
	   
su a_ind if abs(x_pe-coord_x)>$trembling, de	   


*** 3. uii_crra < uipec_crra 
*** no case where  uii_crra < uic_crra | uii_crra < uiw_crra
*** conclusion: precision problem

use Risk_ByChoice.dta, clear
keep if (uii_crra < uipec_crra) & pe_crra ==0 // N=2

tw (scatter optx_crra_ind x_pe_crra)  (function y=x, range(0 3000)),  ///
	xlabel(0(500)3000) xtitle("Pareto Optimal x") ///
	ylabel(0(500)3000) ytitle("Observed x")
	   
	   
*** 4. uipec_crra < uic_crra
*** Conclusion: all observed choice violate fosd + elation loving -> PE numerical

use Risk_ByChoice.dta, clear
keep if (uipec_crra < uic_crra) & pe_crra ==0 // N=153
tab fosd

tw (scatter x_pe_crra coord_x)  (function y=x, range(0 3000)),  ///
	xlabel(0(500)3000) xtitle("Pareto Optimal x") ///
	ylabel(0(500)3000) ytitle("Observed x")

g relconsmp_uipec = y_pe_crra/(x_pe_crra + y_pe_crra)
	
twoway (scatter relconsmp lnrelprice if uipec_crra < uic_crra & abs(x_pe_crra-coord_x)>$trembling, msize(large) msymbol(circle_hollow) mcolor(red) ) ///
	   (scatter relconsmp_uipec lnrelprice if uipec_crra < uic_crra & abs(x_pe_crra-coord_x)>$trembling, msize(large) msymbol(triangle_hollow) mcolor(blue) ), ///
	   legend(order(1 "Observed Choice" 2 "Pareto Optimal") ///
		col(1) position(8) ring(0) size(4)) ///
	   xlabel(-2(0.5)2, labsize(4)) xtitle("Log price ratio (log(p{subscript:2}/p{subscript:1}))", size(4.5) height(4)) ///
       ylabel(0(0.2)1, labsize(4)) ytitle("Share for the second security (x{subscript:2}/(x{subscript:2}+x{subscript:1}))", size(4.5) height(4)) ///
	   xline(2, lcolor(black)) /// *box
	   yline(1, lcolor(black)) /// *box
	   xline(0, lcolor(blue) lstyle(grid)) yline(0.5, lcolor(blue)) /// 
	   graphregion(color(white) lcolor(black)  ) ///
       plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 	   	   

count if a_crra>=0.5 // N= 93
	   
	   
*** 5. uipec_crra < uiw_crra
*** no case where uipec_crra < uiw_crra	   
*** conclusion: all observed choice violate fosd + elation loving -> PE numerical

use Risk_ByChoice.dta, clear
keep if (uipec_crra < uiw_crra) & pe_crra ==0 // N=13

log c
