log using 0_DataPrep_BH.log, replace

********************************************************************************
* WRITTEN BY MINSEON PARK 02-09-20
* Basic Prep for Analysis
* Iv_afriat using Halevy code, RDU specification following Halevy et al. (2018)
********************************************************************************

set more off
set matsize 8000

cd "C:/Users/hahn0/Dropbox/RP/Data"
global mmi = "C:/Users/hahn0/Dropbox/RP/Estimation_Money Metric Index/Code Package"
global source = "C:/Users/hahn0/Dropbox/RP/Data/SourceDataSets"
global mmi_post = "C:/Users/hahn0/Dropbox/RP/Estimation_Money Metric Index/Post Estimation_MS"
global omega = 0.001 // value from HPZ_No_Corners.m


**********
* I. Cleaning Raw Data Sets
* II. Cleaning Estimated Data Sets
* III. Data Merge
* IV. Generate Variables
* V. Individual- and Group-level Data Sets
**********



********************************************************************************
* 						I. Cleaning Raw Data Sets
********************************************************************************

*****
* CHOICE DATA
*****

use "$source/Risk_Raw.dta", clear
ren id_new id
keep id game_type round_number coord_x coord_y intercept_x intercept_y partner_both mover

tab mover // t and f ? (BH)

* Error
count if (coord_x>intercept_x) | (coord_y>intercept_y) // 1 observation, only can happen when error occurs
drop if id==1110601 & round_number==1 

* Drop samples with only individual decision
ren partner_both partner
gsort id -partner
bysort id: replace partner=partner[1] if partner==0
drop if partner==id | partner==0 //subjects who played only indiv. game

* Etc
replace game_type ="1" if game_type=="individual"
replace game_type ="2" if game_type=="collective"
destring game_type, replace 
egen groupid=rowmax(id partner)

save Risk_Raw_Cleaned.dta, replace

* round_number별 개수 확인 (BH)
bysort groupid round_number: gen count = _N
egen total_rounds = total(count), by(groupid)
tab total_rounds
drop if total_rounds == 144
tab id
tab round_number
keep if (game_type == 1 & round_number == 1)

*****
*Cleaning survey
*****

use "$source/survey_all_long.dta", clear
keep if t==0

#delimit ;
drop rdu_rr	mover prweightin~r uloss_rr alpha uloss_ind alpha_std     
ccei varian rho rho_std riskpremium rdu pe prweighting uloss         
alpha_rr alpha_rr_std rho_rr rho_rr_std riskpremiu~r game_type pe_rr ; 
#delimit cr

save Survey_Cleaned.dta, replace 

tab treatment
tab txtreatment
tab grade // 1학년 2823, 2학년 602
tab class
tab male // 1이 남성 2가 여성?
sum weight, detail // height 200 weight 44 같은 이상한 값들도 있는듯

winsor weight, p(0.01) generate(winsor_weight)
summarize weight winsor_weight, detail

winsor height, p(0.01) generate(winsor_height)
summarize height winsor_height, detail



*****
* Export data for parameter estimation
*****

* For joint decisions, export only one member's information to avoid doublecounting
use Risk_Raw_Cleaned.dta, clear
keep if game_type==1 | (game_type==2 & groupid~=id)
keep id round_number coord* intercept* game_type
order id round_number coord* intercept* game_type
sort id round_number
export excel using "$mmi/Data_daegu_2016.xls", replace firstrow(variables) 



********************************************************************************
* 				  	II. Cleaning Estimated Data Sets
********************************************************************************

*****
* GARP
*****

import delimited "$mmi/Indices-Results-31-Jan-2020-819293.csv", clear
g id=floor(subject/10)
g game_type=mod(subject,10)

ren (afriat_index varian_index_min varian_index_mean varian_index_avgssq) (Iv_afriat Iv_varian1 Iv_varian2 Iv_varian3)

keep id game_type Iv_afriat-isexact
order id game_type Iv_afriat-isexact

save GARPResults.dta, replace

tab isexact
sum Iv_afriat, detail
sum Iv_varian?, detail

*****
* Cleaning Function Estimstion Results
*****

*** CARA-DA
import delimited "$mmi/MMIResults-CARA-12-20-2019-819251-DA.csv", clear
g id=floor(subject/10)
g game_type=mod(subject,10)

# delimit ;
ren (a simulated_sd_a simulated_low_95_a simulated_high_95_a mme_criterion_avgsumofsquareswas)
	(r r_sd r_low95 r_hi95 Im) ;
# delimit cr

g a=1/(2+beta)
g a_low95 =1/(2+simulated_low_95_beta)
g a_hi95 = 1/(2+simulated_high_95_beta)
g a_sd = simulated_sd_beta / (2+beta)^4 // Delta method

keep id game_type a* r* Im
order id game_type a* r* Im
save Im_CARA.dta, replace

*** CARA-EUT
import delimited "$mmi/MMIResults-CARA-01-03-2020-819265-EUT.csv",  clear
g id=floor(subject/10)
g game_type=mod(subject,10)

# delimit ;
ren (a simulated_sd_a simulated_low_95_a simulated_high_95_a mme_criterion_avgsumofsquareswas)
	(r r_sd r_low95 r_hi95 Im) ;
# delimit cr

g a= 1/(2+beta)
g a_low95 =1 /(2+simulated_low_95_beta)
g a_hi95 = 1/(2+simulated_high_95_beta)
g a_sd = simulated_sd_beta/ (2+beta)^4 // Delta method

keep id game_type a* r* Im
order id game_type a* r* Im
ren * *_eut
ren (id_eut game_type_eut) (id game_type)
save Im_CARA_EUT.dta, replace

*** CRRA-DA
import delimited "$mmi/MMIResults-CRRA-12-23-2019-819254-DA.csv", clear
g id=floor(subject/10)
g game_type=mod(subject,10)

# delimit ;
ren (r simulated_sd_r simulated_low_95_r simulated_high_95_r mme_criterion_avgsumofsquareswas) 
	(r_crra r_sd_crra r_low95_crra r_hi95_crra Im_crra) ;
# delimit cr

g a_crra=1/(2+beta)
g a_low95_crra =1/(2+simulated_low_95_beta)
g a_hi95_crra = 1/(2+simulated_high_95_beta)
g a_sd_crra = simulated_sd_beta / (2+beta)^4

keep id game_type a* r* Im_crra
order id game_type a* r* Im_crra
save Im_CRRA.dta, replace

*** CRRA-EUT
import delimited "$mmi/MMIResults-CRRA-01-01-2020-819263-EUT.csv", clear
g id=floor(subject/10)
g game_type=mod(subject,10)

# delimit ;
ren (r simulated_sd_r simulated_low_95_r simulated_high_95_r mme_criterion_avgsumofsquareswas) 
	(r_crra r_sd_crra r_low95_crra r_hi95_crra Im_crra) ;
# delimit cr

g a_crra= 1/(2+beta)
g a_low95_crra =1/(2+simulated_low_95_beta)
g a_hi95_crra = 1/(2+simulated_high_95_beta)
g a_sd_crra = simulated_sd_beta / (2+beta)^4

keep id game_type a* r* Im_crra
order id game_type a* r* Im_crra
ren * *_eut
ren (id_eut game_type_eut) (id game_type)
save Im_CRRA_EUT.dta, replace



********************************************************************************
*                       	III. Data Merge
********************************************************************************

*****
* MERGING
*****

use Risk_Raw_Cleaned.dta, clear
merge m:1 id game_type using GARPResults.dta
drop if _merge==2 // only individual game
drop _merge
merge m:1 id game_type using Im_CARA.dta
drop _merge
merge m:1 id game_type using Im_CARA_EUT.dta
drop _merge
merge m:1 id game_type using Im_CRRA.dta
drop _merge
merge m:1 id game_type using Im_CRRA_EUT.dta
drop _merge
merge m:1 id using Survey_Cleaned.dta
drop if _merge==2 // only individual game
drop _merge
merge m:1 id partner using "$source/network.dta"
drop _merge


* Add joint decision estimation result to the other member
mdesc Iv_afriat-Im_crra_eut
gsort groupid id -round_number
foreach x of varlist Iv_afriat-Im_crra_eut { 
bysort groupid: replace `x'=`x'[1] if `x'==. 
} 


*****
* Unidentified cases 
*****

* 1) All coners : risk neutral or optimism (alpha=0), low r
* 2) All middle point : extreme risk aversion or pessimism (alpha=1), high r 
g relconsmp = coord_y/(coord_x+coord_y)
g lnrelprice=log(intercept_x/intercept_y)

g corner=cond(relconsmp>0.95 | relconsmp<0.05,1,0)
g middle=cond(relconsmp>0.45 & relconsmp<0.55,1,0)

bysort id game_type: egen corner_sum=sum(corner)
bysort id game_type: egen middle_sum=sum(middle)

/* egen tag=tag(id game_type)

su r if tag==1, de // summary stat of r of all
su r if corner_sum==18 & tag==1, de // summary stat of r of all corner cases
su r if middle_sum==18 & tag==1, de // summary stat of r of all middle cases
su r_eut if tag==1, de // summary stat of r of all
su r_eut if corner_sum==18 & tag==1, de // summary stat of r of all corner cases
su r_eut if middle_sum==18 & tag==1, de // summary stat of r of all middle cases

twoway (scatter a r if tag==1, msymbol(dh)) ///
	(scatter a r if corner_sum==18 & tag==1, mcolor(red) msymbol(oh)) ///
	(scatter a r if middle_sum==18 & tag==1, mcolor(forest_green) msymbol(x)), ///
	ytitle({&alpha}) xtitle({&rho}) legend(order(1 "All" 2 "All Corners" 3 "All Middle Points"))

twoway (scatter a_eut r_eut if tag==1, msymbol(dh)) ///
	(scatter a_eut r_eut if corner_sum==18 & tag==1, mcolor(red) msymbol(oh)) ///
	(scatter a_eut r_eut if middle_sum==18 & tag==1, mcolor(forest_green) msymbol(x)), ///
	ytitle({&alpha}) xtitle({&rho}) legend(order(1 "All" 2 "All Corners" 3 "All Middle Points"))

su r_crra if tag==1, de // summary stat of r of all
su r_crra if corner_sum==18 & tag==1, de // summary stat of r of all corner cases
su r_crra if middle_sum==18 & tag==1, de // summary stat of r of all middle cases
su r_crra_eut if tag==1, de // summary stat of r of all
su r_crra_eut if corner_sum==18 & tag==1, de // summary stat of r of all corner cases
su r_crra_eut if middle_sum==18 & tag==1, de // summary stat of r of all middle cases

twoway (scatter a_crra r_crra if tag==1, msymbol(dh)) ///
	(scatter a_crra r_crra if corner_sum==18 & tag==1, mcolor(red) msymbol(oh)) ///
	(scatter a_crra r_crra if middle_sum==18 & tag==1, mcolor(forest_green) msymbol(x)), ///
	ytitle({&alpha}) xtitle({&rho}) legend(order(1 "All" 2 "All Corners" 3 "All Middle Points"))

twoway (scatter a_crra_eut r_crra_eut if tag==1, msymbol(dh)) ///
	(scatter a_crra_eut r_crra_eut if corner_sum==18 & tag==1, mcolor(red) msymbol(oh)) ///
	(scatter a_crra_eut r_crra_eut if middle_sum==18 & tag==1, mcolor(forest_green) msymbol(x)), ///
	ytitle({&alpha}) xtitle({&rho}) legend(order(1 "All" 2 "All Corners" 3 "All Middle Points")) */


*****
* EUT vs. RDU
*****

*** 1. more than 10% change in Im: after estimating Iv_afriats

g rdu = cond( (Im_eut - Im)/(Im - Iv_afriat) >=0.1 ,1,0)
g rdu_crra = cond( (Im_crra_eut - Im_crra)/(Im_crra - Iv_afriat) >=0.1 ,1,0)


*** 2. all corner, all middle point cases
replace rdu = 0 if (corner_sum==18 | middle_sum==18) & rdu==1 // N=2,446 -> 137 subjects
replace rdu_crra = 0 if (corner_sum==18 | middle_sum==18) & rdu_crra==1 // N=2,142 -> 119 subjects

*** 3. change estimates following rdu/eut classfication
foreach x of varlist a-Im {
replace `x'=`x'_eut if rdu==0
} 

foreach x of varlist a_crra-Im_crra {
replace `x'=`x'_eut if rdu_crra==0
}
 
drop *_eut


*****
* Mover information
*****

sort id round_number
bysort id: replace mover=mover[19]

save Risk_Merged.dta, replace


*****
* Export Data for Optimal Demand Estimation
*****

use Risk_Merged.dta, clear

preserve
sort id round_number
foreach x of varlist a r a_crra r_crra {
g `x'_ind=`x' if game_type==1
bysort id: replace `x'_ind=`x'_ind[1] if `x'_ind==.
}

keep if game_type==2

keep id round_number intercept_x intercept_y a r a_ind r_ind a_crra r_crra a_crra_ind r_crra_ind 
order id round_number intercept_x intercept_y a r a_ind r_ind a_crra r_crra a_crra_ind r_crra_ind

foreach x of varlist a r a_ind r_ind a_crra r_crra a_crra_ind r_crra_ind { 
format %10.9f `x' 
} 
export delimited using "$mmi_post/data_daegu_collective.csv",  replace
restore 


*****
* Merge Estimated Optimal Demand Results
*****

import delimited "$mmi_post/EstDemand.csv",  clear

#delimit ;
ren (v1-v10) (id round_number optx_col opty_col optx_ind opty_ind 
	optx_crra_col opty_crra_col optx_crra_ind opty_crra_ind) ;

foreach x of varlist optx_col opty_col optx_ind opty_ind 
	optx_crra_col opty_crra_col optx_crra_ind opty_crra_ind { ;
format %20.0g `x' ;
} ; 	
#delimit cr

save EstDemand.dta, replace 



********************************************************************************
*              III. Generate Variables and Preperation for Analysis
********************************************************************************

*****
* Risk Measures, Pareto Efficiency
*****

use Risk_Merged.dta, clear
merge 1:1 id round_number using EstDemand.dta
drop _merge // only half is matched because the optimal optand is only for joint decision

* Risk Aversion
g riskpremium = 2*(1-a)-1+r*2*a*(1-a)*1
g riskpremium_crra= 2*(1-a_crra)-1+r_crra*2*a_crra*(1-a_crra)

g riskaversion = coord_y/(coord_x+coord_y) if intercept_x>intercept_y 
replace riskaversion = coord_x/(coord_x+coord_y) if intercept_x<intercept_y 
replace riskaversion = coord_x/(coord_x+coord_y) if intercept_x==intercept_y & coord_x<coord_y
replace riskaversion = coord_y/(coord_x+coord_y) if intercept_x==intercept_y & coord_x>coord_y //only 8/57,858 cases with intercpet_x= intercept_y 

* Detailed Risk Type
g prweighting=rdu
replace prweighting = 2 if a > 0.5
g prweighting_crra=rdu_crra
replace prweighting_crra = 2 if a > 0.5

* Pareto Efficiency - binary
foreach x in optx_ind opty_ind optx_crra_ind opty_crra_ind {
bysort groupid round: egen double `x'_min = min(`x')
bysort groupid round: egen double `x'_max = max(`x')
}

g pe = cond(coord_x>=round(optx_ind_min,1) & coord_x<=round(optx_ind_max,1),1,0)
g pe_crra = cond(coord_x>=round(optx_crra_ind_min,1) & coord_x<=round(optx_crra_ind_max,1),1,0)

* Pareto Efficiency - Loss
g double coord_y_real=(1-coord_x/intercept_x)*intercept_y
g double x_pe=cond(abs(optx_ind_min-coord_x) < abs(optx_ind_max-coord_x), optx_ind_min,optx_ind_max)
g double y_pe=cond(abs(opty_ind_min-coord_y_real) < abs(opty_ind_max-coord_y_real), opty_ind_min,opty_ind_max)

g double x_pe_crra=cond(abs(optx_crra_ind_min-coord_x) < abs(optx_crra_ind_max-coord_x), optx_crra_ind_min,optx_crra_ind_max)
g double y_pe_crra=cond(abs(opty_crra_ind_min-coord_y_real) < abs(opty_crra_ind_max-coord_y_real), opty_crra_ind_min,opty_crra_ind_max)

g double x_w=cond(intercept_x>=intercept_y,0,intercept_x)
g double y_w=cond(intercept_x>=intercept_y,intercept_y,0)
g double x_w2= 1/(1/intercept_x + 1/intercept_y) // for elation loving case


sort id round_number
foreach var in a a_crra r r_crra {
g `var'_ind=`var'
bysort id: replace `var'_ind=`var'[1]
} 
 
#delimit ; 
g fosd=cond((intercept_x<intercept_y&coord_x>coord_y_real)
				|(intercept_x>intercept_y&coord_x<coord_y_real),1,0) ; 
 
g double uic= cond(coord_x<coord_y_real,
			-a_ind*exp(-r_ind*coord_x)-(1-a_ind)*exp(-r_ind*coord_y_real), 
			-a_ind*exp(-r_ind*coord_y_real)-(1-a_ind)*exp(-r_ind*coord_x)) ;

g double uii= cond(optx_ind<opty_ind,
		-a_ind*exp(-r_ind*optx_ind)-(1-a_ind)*exp(-r_ind*opty_ind), 
		-a_ind*exp(-r_ind*opty_ind)-(1-a_ind)*exp(-r_ind*optx_ind)) ;
		
g double uipec= cond(x_pe<y_pe,
		-a_ind*exp(-r_ind*x_pe)-(1-a_ind)*exp(-r_ind*y_pe),
		-a_ind*exp(-r_ind*y_pe)-(1-a_ind)*exp(-r_ind*x_pe)) ;
		
g double uiw= cond(x_w<y_w,
		-a_ind*exp(-r_ind*x_w)-(1-a_ind)*exp(-r_ind*y_w),
		-a_ind*exp(-r_ind*y_w)-(1-a_ind)*exp(-r_ind*x_w)) ;
#delimit cr
		
g double uiw2= -exp(-r_ind*x_w2) 
replace uiw=uiw2 if uiw>uiw2 

#delimit ;  //what if r_crra==1?
g double uic_crra= cond(coord_x<coord_y_real,
		(a_crra_ind*((coord_x+$omega )^(1-r_crra_ind))+(1-a_crra_ind)*((coord_y_real+$omega )^(1-r_crra_ind))) / (1-r_crra_ind),
		(a_crra_ind*((coord_y_real+$omega )^(1-r_crra_ind))+(1-a_crra_ind)*((coord_x+$omega )^(1-r_crra_ind))) / (1-r_crra_ind) ) ;
		
g double uii_crra= cond(optx_crra_ind<opty_crra_ind,
		(a_crra_ind*((optx_crra_ind+$omega )^(1-r_crra_ind))+(1-a_crra_ind)*((opty_crra_ind+$omega )^(1-r_crra_ind))) / (1-r_crra_ind),
		(a_crra_ind*((opty_crra_ind+$omega )^(1-r_crra_ind))+(1-a_crra_ind)*((optx_crra_ind+$omega )^(1-r_crra_ind))) / (1-r_crra_ind)) ;
		
g double uipec_crra=cond(x_pe_crra<y_pe_crra,
		(a_crra_ind*((x_pe_crra+$omega )^(1-r_crra_ind))+(1-a_crra_ind)*((y_pe_crra+$omega )^(1-r_crra_ind))) / (1-r_crra_ind),
		(a_crra_ind*((y_pe_crra+$omega )^(1-r_crra_ind))+(1-a_crra_ind)*((x_pe_crra+$omega )^(1-r_crra_ind))) / (1-r_crra_ind)) ;
		
g double uiw_crra=cond(x_w<y_w, 
		(a_crra_ind*((x_w+$omega )^(1-r_crra_ind))+(1-a_crra_ind)*((y_w+$omega )^(1-r_crra_ind))) / (1-r_crra_ind),
		(a_crra_ind*((y_w+$omega )^(1-r_crra_ind))+(1-a_crra_ind)*((x_w+$omega )^(1-r_crra_ind))) / (1-r_crra_ind)) ;
#delimit cr

g double uiw2_crra= ((x_w2+$omega )^(1-r_crra_ind)) /(1-r_crra_ind)
replace uiw_crra=uiw2_crra if uiw_crra>uiw2_crra

replace x_w = x_w2 if uiw_crra>uiw2_crra 
replace y_w = x_w2 if uiw_crra>uiw2_crra

save Risk_ByChoice.dta, replace
 
 
***** 
* Numerical Search for Pareto Optimal Choice
*****
*** See C:/Users/minse/Dropbox/RP/Notes/Pareto Efficient Choice Numerical Search.pdf for detail

use Risk_ByChoice.dta, clear

preserve
keep if (uipec < uic ) & pe==0 // N=186, all violate fosd, no case where uipec < uiw 
keep id round_number a_ind r_ind coord* intercept* x_w2
export delimited using "$mmi_post/data_PE_numerical.csv",  replace
restore

preserve
keep if (uipec_crra < uic_crra | uipec_crra < uiw_crra) & pe_crra ==0 // N=153 + 13 = 166 
keep id round_number a_ind r_ind coord* intercept* x_w2
export delimited using "$mmi_post/data_PE_numerical_crra.csv",  replace
restore

preserve
import excel "$mmi_post/numericalPE.xls", sheet("Sheet1") clear
ren (A B C D) (id round_number x_pi y_pi)
save PE_numerical.dta, replace
restore

preserve
import excel "$mmi_post/numericalPE_crra.xls", sheet("Sheet1") clear
ren (A B C D) (id round_number x_pi y_pi)
save PE_numerical_crra.dta, replace
restore


*****
* Merge numerical search result
*****

use Risk_ByChoice.dta, clear

* replace CARA cases
merge 1:1 id round_number using PE_numerical.dta
drop _merge
sort groupid round_number x_pi
bysort groupid round_number: replace x_pi=x_pi[1]
bysort groupid round_number: replace y_pi=y_pi[1]
 
#delimit ; 
replace uipec=-a_ind*exp(-r_ind*x_pi)
		-(1-a_ind)*exp(-r_ind*y_pi) if (x_pi~=. & y_pi~=.) & (x_pi < y_pi) ;
replace uipec=-a_ind*exp(-r_ind*y_pi)
		-(1-a_ind)*exp(-r_ind*x_pi) if (x_pi~=. & y_pi~=.) & (x_pi >= y_pi) ;
#delimit cr 

* replace CRRA cases
drop *_pi
merge 1:1 id round_number using PE_numerical_crra.dta
drop _merge
sort groupid round_number x_pi
bysort groupid round_number: replace x_pi=x_pi[1]
bysort groupid round_number: replace y_pi=y_pi[1]
 
#delimit ; 
replace uipec_crra= (a_crra_ind*((x_pi+$omega )^(1-r_crra_ind))+(1-a_crra_ind)*((y_pi+$omega )^(1-r_crra_ind))) / (1-r_crra_ind) 
					if (x_pi~=. & y_pi~=.) & (x_pi < y_pi) ;
replace uipec_crra= (a_crra_ind*((y_pi+$omega )^(1-r_crra_ind))+(1-a_crra_ind)*((x_pi+$omega )^(1-r_crra_ind))) / (1-r_crra_ind) 
					if (x_pi~=. & y_pi~=.) & (x_pi >= y_pi) ;
#delimit cr 

g double uloss= cond(pe==0,(uipec-uic)/(uipec-uiw),0)
replace uloss=0 if (uipec < uic) & pe==0 // N= 24 with numerical search

g double uloss_crra= cond(pe_crra==0,(uipec_crra-uic_crra)/(uipec_crra-uiw_crra),0)
replace uloss_crra=0 if (uipec_crra < uic_crra | uipec_crra < uiw_crra) & pe_crra ==0 // N=60 without numerical search

g double uloss_ind= (uii-uic)/(uii-uiw)
replace uloss_ind=0 if uloss_ind<0 //for all 25 cases, x_c=x*_i, but uloss_ind<0 for minor computation error in Stata

g double uloss_crra_ind= (uii_crra-uic_crra)/(uii_crra-uiw_crra)
replace uloss_crra_ind=0 if uloss_crra_ind<0

save Risk_ByChoice.dta, replace


 
********************************************************************************
*                     IV. Individual and Pair Unit Data Sets
********************************************************************************

*****
* Individual-level
*****

use Risk_ByChoice.dta, clear

* Collapse to the individual-level
# delimit ;
collapse (mean) groupid partner Iv_afriat Iv_varian* riskaversion 
a r Im a_crra r_crra Im_crra rdu rdu_crra riskpremium riskpremium_crra prweighting 
pe pe_crra uloss uloss_crra uloss_ind 
school class mathscore male outgoing opened agreeable conscientious stable
height height_gr point pointed mutual distance Outdegree Indegree 
risk_q1 risk_q2 risk_q3, by(id game_type mover) ;

order id groupid partner game_type mover Iv_afriat Iv_varian* riskaversion 
a r Im a_crra r_crra Im_crra rdu rdu_crra riskpremium riskpremium_crra prweighting 
pe pe_crra uloss uloss_crra uloss_ind 
school class mathscore male outgoing opened agreeable conscientious stable
height height_gr point pointed mutual distance Outdegree Indegree 
risk_q1 risk_q2 risk_q3;
# delimit cr

ren (Indegree Outdegree) (indegree outdegree)


* Misspecification
g Is = Im-Iv_afriat
g Is_crra = Im_crra - Iv_afriat


* Reshape
gsort id -game_type
foreach var of varlist uloss uloss_crra uloss_ind pe pe_crra {
bysort id: replace `var'=`var'[1]
}

reshape wide Iv_afriat-prweighting Is*, i(id) j(game_type)
ren *2 *_col
ren *1 *_ind
ren (risk_q_ind risk_q_col) (risk_q1 risk_q2)

foreach x of varlist riskaversion_ind riskpremium_ind riskpremium_crra_ind r_ind {
su `x', de 
g `x'_hi=cond(`x'>=r(p50),1,0)
}

* Covariates
g coed=cond(school==11|school==14|school==16|school==21|school==24|school==26,1,0) 

replace risk_q1=6-risk_q1
recode risk_q3 (2=1) (1=2)

egen tag=tag(class)
gsort school -tag class
bysort school: g experimenter=_n if tag==1
sort class experimenter
bysort class: replace experimenter=experimenter[1]
replace experimenter=5 if (class==12202 | class==24101)

la define experimenter 1 "shin" 2 "hwang" 3 "gu" 4 "lee" 5 "park"
la val experimenter experimenter
drop tag

bysort class: g classsize=_N
g indegree_n=indegree/classsize
g outdegree_n=outdegree/classsize


save Risk_ByIndiv.dta, replace



*****
* Group-level
*****

use Risk_ByIndiv.dta, clear

* Reshape
sort groupid mover

# delimit ;
	foreach var of varlist a_ind r_ind Im_ind a_crra_ind r_crra_ind Im_crra_ind Iv_afriat_ind Iv_varian*_ind 
	riskaversion_ind riskpremium_ind riskpremium_crra_ind rdu_ind rdu_crra_ind 
	riskaversion_ind_hi riskpremium_ind_hi riskpremium_crra_ind_hi prweighting_ind r_ind_hi
	male mathscore outgoing opened agreeable conscientious stable height height_gr indegree_n outdegree_n 
	risk_q1 risk_q2 risk_q3 uloss uloss_crra uloss_ind Is_ind Is_crra_ind { ;
	g `var'2=`var' if mover=="f" ;
	bysort group: replace `var'2=`var'2[1] ;
	} ;
	drop if mover=="f" ;

	foreach x of varlist Iv_afriat_ind Iv_varian*_ind riskaversion_ind riskpremium_ind 
	riskpremium_crra_ind r_ind mathscore  
	indegree_n outdegree_n { ;
	egen `x'_max=rowmax(`x' `x'2) ;
	egen `x'_min=rowmin(`x' `x'2) ;
	egen `x'_ave=rowmean(`x' `x'2) ;
	g `x'_dist=abs(`x'-`x'2) ;
	} ;
# delimit cr


* Group-level variables
	g rdupair=cond(rdu_ind==0&rdu_ind2==0,0,.)
	replace rdupair=1 if rdu_ind==0&rdu_ind2==1 | rdu_ind==1&rdu_ind2==0
	replace rdupair=2 if rdu_ind==1&rdu_ind2==1
	la de rdupair 0 "Both EUT" 1 "EUT and RDU" 2 "Both RDU"
	la val rdupair rdupair

	g rdu_crrapair=cond(rdu_crra_ind==0&rdu_crra_ind2==0,0,.)
	replace rdu_crrapair=1 if rdu_crra_ind==0&rdu_crra_ind2==1 | rdu_crra_ind==1&rdu_crra_ind2==0
	replace rdu_crrapair=2 if rdu_crra_ind==1&rdu_crra_ind2==1
	la val rdu_crrapair rdupair

	g prweightingpair=rdupair if rdupair==0
	replace prweightingpair= 1 if (prweighting_ind==0 & prweighting_ind2==1) |(prweighting_ind==1 & prweighting_ind2==0)
	replace prweightingpair= 2 if (prweighting_ind==0 & prweighting_ind2==2) |(prweighting_ind==2 & prweighting_ind2==0)
	replace prweightingpair= 3 if (prweighting_ind==1 & prweighting_ind2==1) 
	replace prweightingpair= 4 if (prweighting_ind==1 & prweighting_ind2==2) |(prweighting_ind==2 & prweighting_ind2==1)
	replace prweightingpair= 5 if (prweighting_ind==2 & prweighting_ind2==2) 
	la de pr 0 "Both EUT" 1 "EUT and Disappointment Averse" 2 "EUT and Elation Loving" 3 "Both Disappointment Avers" 4 "ElationLoving and Disppointment Averse" 5 "Both Elation Loving"
	la val prweightingpair pr
	
	g riskaversionpair =(riskaversion_ind_hi ==1 | riskaversion_ind_hi2==1)
	replace riskaversionpair =2 if (riskaversion_ind_hi ==1 & riskaversion_ind_hi2==1)
	la de rapair 0 "Both Neutral" 1 "One Neutral and One Averse" 2 "Both Averse"
	la val riskaversionpair rapair
	
	g riskpremiumpair =(riskpremium_ind_hi ==1 | riskpremium_ind_hi2==1)
	replace riskpremiumpair =2 if (riskpremium_ind_hi ==1 & riskpremium_ind_hi2==1)
	la de rppair 0 "Both Neutral" 1 "One Neutral and One Averse" 2 "Both Averse"
	la val riskpremiumpair rppair
	
	g rpair =(r_ind_hi ==1 | r_ind_hi2==1)
	replace rpair =2 if (r_ind_hi ==1 & r_ind_hi2==1)
	la de rpair 0 "Both Low Rhos" 1 "One Low and One High Rho" 2 "Both High Rhos"
	la val rpair rpair
	
	egen uloss_col=rowmean(uloss uloss2)
    
	g malepair = 2 if male==1 & male2==1
	replace malepair = 0 if male==2 & male2==2
	replace malepair = 1 if malepair==.
	la de malepair 0 "Both Girls" 1 "Boy and Girl" 2 "Both Boys"
	la val malepair malepair
	
	g friendship=cond(point==1&pointed==1,2,0)
	replace friendship=1 if pointed==1&point==0
	replace friendship=1 if pointed==0&point==1
	replace friendship=0 if pointed==0&point==0
	la de friendship 2 "Mutual" 1 "One sided" 0 "Nothing"
	la val friendship friendship 

	# delimit  ;
	order id Iv* riskaversion* riskpremium* a_* r_* Im* rdu* prweighting*
	mathscore*  malepair friendship class ;
	#delimit cr

	save Risk_ByPair.dta, replace


log c



