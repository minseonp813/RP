* Author: Minseon Park 06/07/20
* By choice at each group level data

	set more off
	set matsize 8000

	cd "C:\Users\hahn0\Dropbox\RP\Data"
	global results_int = "C:\Users\hahn0\Dropbox\RP\Data\Results_Internal\99_loss_by_round"
	global results_main = "C:\Users\hahn0\Dropbox\RP\Data\Results_Main"
	global results_app = "C:\Users\hahn0\Dropbox\RP\Data\Results_Appendix"


	use Risk_ByChoice.dta, clear

	
********************************************************************************
*									 I. Prep
********************************************************************************


	* Keep relavant variables
	# delimit;
	keep id groupid mover round_number pe pe_crra uloss* 
		Im Iv_afriat Iv_varian3 riskaversion riskpremium rdu*
		point pointed mutual male mathscore 
		outgoing opened agreeable conscientious stable 
		height height_gr indegree outdegree 
		risk_q1 risk_q2 risk_q3;
	# delimit cr
	
	
	* Mean utility loss
	sort groupid round_number mover
	bysort groupid round_number: egen uloss_col =mean(uloss)
	
	
	* Individual-level variables
	foreach x in Im Iv_afriat Iv_varian3 riskaversion riskpremium rdu {
	g `x'_ind = `x' if round_number<=18
	sort id round_number
	bysort id: replace `x'_ind = `x'_ind[1] if `x'_ind==.
	}
	
	foreach x of varlist Im_ind Iv_afriat_ind Iv_varian3_ind riskaversion_ind riskpremium_ind mathscore {
	su `x', de 
	g `x'_hi=cond(`x'>=r(p50),1,0)
	}
	
	drop if round_number<=18

	
	* Group-level variables
	la de pair 0 "Both Low" 1 "One Low/One High" 2 "Both High"
	
	recode male (2=0)
	
	foreach x in Im_ind_hi Iv_afriat_ind_hi Iv_varian3_ind_hi ///
			rdu_ind riskaversion_ind_hi riskpremium_ind_hi mathscore_hi ///
			male {
	bysort groupid round_number: egen `x'_mean = mean(`x')

	g `x'_pair=cond(`x'_mean==0,0,.)
	replace `x'_pair=2 if `x'_mean==1
	replace `x'_pair=1 if `x'_mean==0.5


	la val `x'_pair pair
	}
	
	
	drop if mover=="t"
	
	g friendship=cond(point==1&pointed==1,2,0)
	replace friendship=1 if pointed==1&point==0
	replace friendship=1 if pointed==0&point==1
	replace friendship=0 if pointed==0&point==0
	la de friendship 2 "Mutual" 1 "One sided" 0 "Nothing"
	la val friendship friendship 
	
	
********************************************************************************
*								II. Regressions
********************************************************************************
	
	
	* Homogenous effect
	g second_half = (round_number>26)
	
	reg uloss second_half 
	outreg2 using "$results_int\99_loss_by_round.xls", bdec(3) cdec(3) drop(i.class) label replace
	reg pe second_half  
	outreg2 using "$results_int\99_loss_by_round.xls", bdec(3) cdec(3) drop(i.class) label append
	
	reg uloss round_number 
	outreg2 using "$results_int\99_loss_by_round.xls", bdec(3) cdec(3) drop(i.class) label append
	reg pe round_number 
	outreg2 using "$results_int\99_loss_by_round.xls", bdec(3) cdec(3) drop(i.class) label append
	
	reg uloss i.round_number 
	outreg2 using "$results_int\99_loss_by_round.xls", bdec(3) cdec(3) drop(i.class) label append
	coefplot, vertical keep(*round_number*) omitted baselevels yline(0) ///
	recast(connected) ciopts(recast(rcap)) ///
	xtitle("round") xlabel(3 "3" 6 "6" 9 "9" 12 "12" 15 "15" 18 "18")
	gr export "$results_int\99_loss_by_round\uloss.png", replace
	
	reg pe i.round_number 
	outreg2 using "$results_int\99_loss_by_round.xls", bdec(3) cdec(3) drop(i.class) label append
	coefplot, vertical keep(*round_number*) omitted baselevels yline(0) ///
	recast(connected) ciopts(recast(rcap)) xlabel(3 "3" 6 "6" 9 "9" 12 "12" 15 "15" 18 "18")
	gr export "$results_int\99_loss_by_round\pe.png", replace
	
	xtset groupid 
	xtreg uloss i.round_number , fe
	outreg2 using "$results_int\99_loss_by_round.xls", bdec(3) cdec(3) drop(i.class) label append addtext("Group FE" "Yes")	
	coefplot, vertical keep(*round_number*) omitted baselevels yline(0) ///
	recast(connected) ciopts(recast(rcap)) xlabel(3 "3" 6 "6" 9 "9" 12 "12" 15 "15" 18 "18")
	
	xtreg pe i.round_number , fe
	outreg2 using "$results_int\99_loss_by_round.xls", bdec(3) cdec(3) drop(i.class) label append addtext("Group FE" "Yes")
	coefplot, vertical keep(*round_number*) omitted baselevels yline(0) ///
	recast(connected) ciopts(recast(rcap)) xlabel(3 "3" 6 "6" 9 "9" 12 "12" 15 "15" 18 "18") 
	
	
	* Heterogenous effect
	foreach x in  Im_ind_hi_pair Iv_afriat_ind_hi_pair riskaversion_ind_hi_pair mathscore_hi_pair male_pair friendship {
	cap erase coefs2.dta coefs1.dta coefs0.dta

	reg uloss i.round_number if `x'==2
	regsave using coefs2, ci replace	

	reg uloss i.round_number if `x'==1
	regsave using coefs1, ci replace	

	reg uloss i.round_number if `x'==0
	regsave using coefs0, ci replace	

	
	preserve
	use coefs2, clear
	ren * *2
	ren var2 var
	merge 1:1 var using coefs1
	drop _merge
	ren (coef stderr ci_lower ci_upper N r2) (coef1 stderr1 ci_lower1 ci_upper1 N1 r11)
	merge 1:1 var using coefs0
	ren (coef stderr ci_lower ci_upper N r2) (coef0 stderr0 ci_lower0 ci_upper0 N0 r00)
	
	keep if _n<=18

	gen evtime=substr(var, 1,2) 
	destring evtime, replace	
	
	twoway (scatter coef2 ci_lower2 ci_upper2 evtime, c(3 l l) cmissing(y n n) msym(o i i) lcolor(gray gray gray)	///
		lpattern(solid dash dash) lwidth(medthick medthick medthick) yline(0, lcolor(black))) ///
		(scatter coef1 ci_lower1 ci_upper1 evtime, c(3 l l) cmissing(y n n) msym(o i i) lcolor(gray gray gray)	///
		lpattern(solid shortdash shortdash) lwidth(medthick medthick medthick) yline(0, lcolor(black))) ///
		(scatter coef0 ci_lower0 ci_upper0 evtime, c(3 l l) cmissing(y n n) msym(o i i) lcolor(gray gray gray)	///
		lpattern(solid longdash longdash) lwidth(medthick medthick medthick) yline(0, lcolor(black))) ///
		, xline(-1, lcolor(black)) subtitle("`y'", size(small) j(left) pos(11)) ///
		ylabel(, nogrid angle(horizontal) labsize(small)) xtitle("Round", size(small)) ///
		graphregion(color(white)) legend(col(1) pos(1) region(lcolor(white)) ///
		order(1 "Both High" 4 "One High/One Low" 7 "Both Low"))
	graph export "$results_int/uloss_`x'.png", replace
	restore
	}
	
	foreach x in  Im_ind_hi_pair Iv_afriat_ind_hi_pair riskaversion_ind_hi_pair mathscore_hi_pair male_pair friendship {
	cap erase coefs2.dta coefs1.dta coefs0.dta

	reg pe i.round_number if `x'==2
	regsave using coefs2, ci replace	

	reg pe i.round_number if `x'==1
	regsave using coefs1, ci replace	

	reg pe i.round_number if `x'==0
	regsave using coefs0, ci replace	

	
	preserve
	use coefs2, clear
	ren * *2
	ren var2 var
	merge 1:1 var using coefs1
	drop _merge
	ren (coef stderr ci_lower ci_upper N r2) (coef1 stderr1 ci_lower1 ci_upper1 N1 r11)
	merge 1:1 var using coefs0
	ren (coef stderr ci_lower ci_upper N r2) (coef0 stderr0 ci_lower0 ci_upper0 N0 r00)
	
	keep if _n<=18

	gen evtime=substr(var, 1,2) 
	destring evtime, replace	
	
	twoway (scatter coef2 ci_lower2 ci_upper2 evtime, c(3 l l) cmissing(y n n) msym(o i i) lcolor(gray gray gray)	///
		lpattern(solid dash dash) lwidth(medthick medthick medthick) yline(0, lcolor(black))) ///
		(scatter coef1 ci_lower1 ci_upper1 evtime, c(3 l l) cmissing(y n n) msym(o i i) lcolor(gray gray gray)	///
		lpattern(solid shortdash shortdash) lwidth(medthick medthick medthick) yline(0, lcolor(black))) ///
		(scatter coef0 ci_lower0 ci_upper0 evtime, c(3 l l) cmissing(y n n) msym(o i i) lcolor(gray gray gray)	///
		lpattern(solid longdash longdash) lwidth(medthick medthick medthick) yline(0, lcolor(black))) ///
		, xline(-1, lcolor(black)) subtitle("`y'", size(small) j(left) pos(11)) ///
		ylabel(, nogrid angle(horizontal) labsize(small)) xtitle("Round", size(small)) ///
		graphregion(color(white)) legend(col(1) pos(1) region(lcolor(white)) ///
		order(1 "Both High" 4 "One High/One Low" 7 "Both Low"))
	graph export "$results_int/pe_`x'.png", replace
	restore
	}
