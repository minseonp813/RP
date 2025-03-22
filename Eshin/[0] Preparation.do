*-------------------------------------------------------------------------------
*                            Cleaning Each DataSets
*------------------------------------------------------------------------------;

***Clearing the raw and exporting for estimation

cd "C:\Users\hahn0\Dropbox\RP\EShin_Data"

use Risk_Raw.dta, clear
ren id id
keep id game_type round_number coord_x coord_y intercept_x intercept_y partner_both mover

br if (coord_x>intercept_x) | (coord_y>intercept_y)
drop if id==1110601 & round_number==1 

ren partner_both partner

gsort id -partner
bysort id: replace partner=partner[1] if partner==0

replace mover="1" if mover=="t"
replace mover="0" if mover=="f"
destring mover, replace
sort id mover
bysort id: replace mover=mover[1]

drop if partner==id //subjects who played only indiv. game
drop if partner==0

egen groupid=rowmax(id partner)

replace game_type ="1" if game_type=="individual"
replace game_type ="2" if game_type=="collective"
destring game_type, replace

save Risk_Raw_Cleaned.dta, replace

keep if game_type==1 | (game_type==2 & groupid~=id)
export excel using "C:₩Users₩minseon₩Dropbox₩RP₩Data₩data_daegu.xls", replace firstrow(variables)


***Cleaning GARP results

use GARPResults_newid.dta, clear
drop subjectid
ren id id
replace ccei_ind=0.964111 if id==1110601
replace varian_ind=0.90633 if id==1110601
ren *_ind *1
ren *_col *2

reshape long ccei varian, i(id) j(game_type)

save GARPResults_newid_Cleaned.dta, replace 

use GARPResults_combined.dta, clear
ren id id
save GARPResults_combined_Cleaned.dta, replace


***Cleaning Function Estimstion Results

clear all
import excel "C:₩Users₩minseon₩Dropbox₩RP₩Estimation₩1.RP_CARA₩results_Pre.xlsx", sheet("Sheet1") clear
ren (A B C D E F G) (id game_type a_nl a_std_nl r_nl r_std_nl ssr_nl)
save NLLS_CARA.dta, replace

clear all
import excel "C:₩Users₩minseon₩Dropbox₩RP₩Estimation₩2.RP_CRRA₩results_Pre.xlsx", sheet("Sheet1") clear
ren (A B C D E F G) (id game_type a_crra_nl a_std_crra_nl r_crra_nl r_std_crra_nl ssr_crra_nl)
save NLLS_CRRA.dta, replace 

clear all
import excel "C:₩Users₩minseon₩Dropbox₩RP₩Money Metric Index₩MMI_CARA.xlsx", sheet("Sheet1") firstrow clear
g id=floor(Subject/10)
g game_type=mod(Subject,10)
g a_mmi=(1+Beta)/(2+Beta)

#delimit ; 

ren (A Simulated_SD_Beta Simulated_SD_A Simulated_Low_95_Beta Simulated_High_95_Beta 
     Simulated_Low_95_A Simulated_High_95_A MME_Value_MaxWastes MME_Value_MeanWastes
	 MME_Criterion_AvgSumOfSquaresW)
    (r_mmi a_std_mmi r_std_mmi a_95low_mmi a_95hi_mmi r_95low_mmi r_95hi_mmi
	mmi_max mmi_mean mmi_ssq) ;

	
keep id game_type a_mmi r_mmi a_std_mmi r_std_mmi a_95low_mmi a_95hi_mmi r_95low_mmi r_95hi_mmi
	mmi_max mmi_mean mmi_ssq;
order id game_type a_mmi r_mmi a_std_mmi r_std_mmi a_95low_mmi a_95hi_mmi r_95low_mmi r_95hi_mmi
	mmi_max mmi_mean mmi_ssq;
	
#delimit cr	
save MMI_CARA.dta, replace

clear all
import excel "C:₩Users₩minseon₩Dropbox₩RP₩Money Metric Index₩MMI_CRRA.xlsx", sheet("Sheet1") firstrow clear
destring *, replace
g id=floor(Subject/10)
g game_type=mod(Subject,10)
g a_crra_mmi=(1+Beta)/(2+Beta)

#delimit ; 

ren (Rho Simulated_SD_Beta Simulated_SD_Rho Simulated_Low_95_Beta Simulated_High_95_Beta 
     Simulated_Low_95_Rho Simulated_High_95_Rho MME_Value_MaxWastes MME_Value_MeanWastes
	 MME_Criterion_AvgSumOfSquaresW)
    (r_crra_mmi a_std_crra_mmi r_std_crra_mmi a_95low_crra_mmi a_95hi_crra_mmi r_95low_crra_mmi r_95hi_crra_mmi
	crra_mmi_max crra_mmi_mean crra_mmi_ssq) ;

keep id game_type a_crra_mmi r_crra_mmi a_std_crra_mmi r_std_crra_mmi a_95low_crra_mmi a_95hi_crra_mmi r_95low_crra_mmi r_95hi_crra_mmi
	crra_mmi_max crra_mmi_mean crra_mmi_ssq;
order id game_type a_crra_mmi r_crra_mmi a_std_crra_mmi r_std_crra_mmi a_95low_crra_mmi a_95hi_crra_mmi r_95low_crra_mmi r_95hi_crra_mmi
	crra_mmi_max crra_mmi_mean crra_mmi_ssq;
	
#delimit cr	
save MMI_CRRA.dta, replace


***Cleaning survey

use survey_all_long.dta, clear
keep if t==0

#delimit ;
drop 
rdu_rr        mover         prweightin~r  riskaversion      uloss_rr      alpha         uloss_ind     alpha_std     
ccei          varian        rho rho_std   riskpremium   rdu           pe            prweighting   uloss         
alpha_rr     alpha_rr_std   rho_rr        rho_rr_std    riskpremiu~r  game_type     pe_rr        ; 
#delimit cr

save survey_all_long_Cleaned.dta, replace


*-------------------------------------------------------------------------------
*                                   Merging
*------------------------------------------------------------------------------;

use Risk_Raw_Cleaned.dta, clear
merge m:1 id game_type using GARPResults_newid_Cleaned.dta
rename _merge _ccei
merge m:1 id using GARPResults_combined_Cleaned.dta
rename _merge _ccei_com
merge m:1 id game_type using NLLS_CARA.dta
ren _merge _nlls_cara
drop if _nlls_cara==2
merge m:1 id game_type using NLLS_CRRA.dta
ren _merge _nlls_crra
drop if _nlls_crra==2
merge m:1 id game_type using MMI_CARA.dta
ren _merge _mmi_cara
drop if _mmi_cara==2
merge m:1 id game_type using MMI_CRRA.dta
ren _merge _mmi_crra
drop if _mmi_crra==2
merge m:1 id using survey_all_long_Cleaned.dta
ren _merge _survey
drop if _survey==2
merge m:1 id partner using network.dta
ren _merge _network1
drop if coord_x==.

gsort groupid id -round_number


#delimit ;

foreach x of varlist a_nl a_std_nl r_nl r_std_nl ssr_nl a_crra_nl a_std_crra_nl
r_crra_nl r_std_crra_nl ssr_crra_nl a_mmi r_mmi a_std_mmi r_std_mmi a_95low_mmi 
a_95hi_mmi r_95low_mmi r_95hi_mmi mmi_max mmi_mean mmi_ssq a_crra_mmi r_crra_mmi 
a_std_crra_mmi r_std_crra_mmi a_95low_crra_mmi a_95hi_crra_mmi r_95low_crra_mmi 
r_95hi_crra_mmi crra_mmi_max crra_mmi_mean crra_mmi_ssq { ;
bysort groupid: replace `x'=`x'[1] if `x'==. ;
} ; //Estimated group decision-parameter only for one person

#delimit cr

g relconsmp = coord_y/(coord_x+coord_y)
g lnrelprice=log(intercept_x/intercept_y)

g ext=cond(relconsmp>0.95 | relconsmp<0.05,1,0)
g hed=cond(relconsmp>0.45 & relconsmp<0.55,1,0)

bysort id game_type: egen ext_sum=sum(ext)
bysort id game_type: egen hed_sum=sum(hed)

foreach x of varlist a_nl a_crra_nl a_mmi a_crra_mmi {
replace `x'=0.5 if ext_sum==18 | hed_sum==18
} 
 
foreach x of varlist r_nl r_crra_nl r_mmi r_crra_mmi {
su `x' if `x'~=0
local xmin=r(min)
local xmax=r(max)
replace `x'=`xmin' if `x'==0
replace `x'=`xmin' if ext_sum==18
replace `x'=`xmax' if hed_sum==18 
}
replace r_crra_mmi=10 if r_crra_mmi>10
 
g riskpremium = 2*a_mmi-1+r_mmi*2*a_mmi*(1-a_mmi)*1
g riskpremium_crra= 2*a_crra_mmi-1+r_crra_mmi*2*a_crra_mmi*(1-a_crra_mmi)

save Risk_merged.dta, replace

***Exporting data for estimating optimal choices
preserve

sort id round_number
foreach x of varlist a_nl a_crra_nl a_mmi a_crra_mmi r_nl r_crra_nl r_mmi r_crra_mmi {
g `x'_ind=`x' if game_type==1
bysort id: replace `x'_ind=`x'_ind[1] if `x'_ind==.
}

keep if game_type==2

#delimit ;
keep id round_number coord_x coord_y intercept_x intercept_y a_nl r_nl a_nl_ind
	r_nl_ind a_crra_nl r_crra_nl a_crra_nl_ind r_crra_nl_ind a_mmi r_mmi 
	a_mmi_ind r_mmi_ind a_crra_mmi r_crra_mmi a_crra_mmi_ind r_crra_mmi_ind ;
order id round_number coord_x coord_y intercept_x intercept_y a_nl r_nl a_nl_ind
	r_nl_ind a_crra_nl r_crra_nl a_crra_nl_ind r_crra_nl_ind a_mmi r_mmi 
	a_mmi_ind r_mmi_ind a_crra_mmi r_crra_mmi a_crra_mmi_ind r_crra_mmi_ind ;

foreach x of varlist a_nl r_nl a_nl_ind 
	r_nl_ind a_crra_nl r_crra_nl a_crra_nl_ind r_crra_nl_ind a_mmi r_mmi 
	a_mmi_ind r_mmi_ind a_crra_mmi r_crra_mmi a_crra_mmi_ind r_crra_mmi_ind { ;
format %10.9f `x' ;
} ;
#delimit cr
export excel using "C:₩Users₩minseon₩Dropbox₩RP₩Estimation₩1.RP_CARA₩data_daegu_collective.xls",  replace
restore


***Merge estimated optimal demand results
import excel "C:₩Users₩minseon₩Dropbox₩RP₩Estimation₩1.RP_CARA₩estdemand.xls", sheet("Sheet1")  clear

#delimit ;
ren (A B C D E F G H I J K L M N O P Q R) (id round_number demx_nl_col demy_nl_col 
     demx_nl_ind demy_nl_ind demx_crra_nl_col demy_crra_nl_col
	 demx_crra_nl_ind demy_crra_nl_ind demx_mmi_col demy_mmi_col
	 demx_mmi_ind demy_mmi_ind demx_crra_mmi_col demy_crra_mmi_col
	 demx_crra_mmi_ind demy_crra_mmi_ind) ;
#delimit cr

save estdemand.dta, replace

use Risk_merged.dta, clear
merge 1:1 id round_number using estdemand.dta
ren _merge _estdem

drop _* pbl*

g demerror_nl=((demx_nl_col-coord_x)^2+(demy_nl_col-coord_y)^2)^(1/2)
g demerror_crra_nl=((demx_crra_nl_col-coord_x)^2+(demy_crra_nl_col-coord_y)^2)^(1/2)
g demerror_mmi=((demx_mmi_col-coord_x)^2+(demy_mmi_col-coord_y)^2)^(1/2)
g demerror_crra_mmi=((demx_crra_mmi_col-coord_x)^2+(demy_crra_mmi_col-coord_y)^2)^(1/2)

*** Fit of each estimation method - distribution of the error 
hist(demerror_nl),percent xtitle(error) ylabel(0(10)80) subtitle(CARA_NLLS) saving(temp1, replace)
hist(demerror_crra_nl), percent xtitle(error) ylabel(0(10)80)  subtitle(CRRA_NLLS) saving(temp2, replace)
hist(demerror_mmi), percent xtitle(error) ylabel(0(10)80) subtitle(CARA_MMI) saving(temp3, replace)
hist(demerror_crra_mmi), percent xtitle(error) ylabel(0(10)80) subtitle(CRRA_MMI) saving(temp4, replace)
gr combine temp1.gph temp2.gph temp3.gph temp4.gph, col(2) 
gr export "C:₩Users₩minseon₩Dropbox₩RP₩Graphs₩estdemerror.png", replace 

hist(demerror_nl) if ccei>=0.9, percent xtitle(error) ylabel(0(10)80) subtitle(CARA_NLLS) saving(temp1, replace)
hist(demerror_crra_nl) if ccei>=0.9, percent xtitle(error) ylabel(0(10)80)  subtitle(CRRA_NLLS) saving(temp2, replace)
hist(demerror_mmi) if ccei>=0.9, percent xtitle(error) ylabel(0(10)80) subtitle(CARA_MMI) saving(temp3, replace)
hist(demerror_crra_mmi) if ccei>=0.9, percent xtitle(error) ylabel(0(10)80) subtitle(CRRA_MMI) saving(temp4, replace)
gr combine temp1.gph temp2.gph temp3.gph temp4.gph, col(2) 
gr export "C:₩Users₩minseon₩Dropbox₩RP₩Graphs₩estdemerror_ccei90.png", replace 

*-------------------------------------------------------------------------------
*                  Generate Variables and Preperation for Analysis
*------------------------------------------------------------------------------;

g riskaversion = .
replace riskaversion = coord_y/(coord_x+coord_y) if intercept_x>intercept_y 
replace riskaversion = coord_x/(coord_x+coord_y) if intercept_x<intercept_y 
replace riskaversion = coord_x/(coord_x+coord_y) if intercept_x==intercept_y & coord_x<coord_y
replace riskaversion = coord_y/(coord_x+coord_y) if intercept_x==intercept_y & coord_x>coord_y
*note: there are only 8 cases where intercpet_x= intercept_y in 57958 obs* 

g rdu=cond(a_95low_mmi<=0.5 & a_95hi_mmi>=0.5,0,1)
la define rdu 0 "EUT" 1 "RDU"
la val rdu rdu
g rdu_crra=cond(a_95low_crra_mmi<=0.5 & a_95hi_crra_mmi>=0.5,0,1)
la val rdu_crra rdu
g prweighting=rdu
replace prweighting = 2 if a_95hi_mmi<0.5
la define prweighting 0 "EUT" 1 "Optimistic" 2 "Pesimistic"
la val prweighting prweighting
g prweighting_crra=rdu_crra
replace prweighting_crra = 2 if a_95hi_crra_mmi<0.5
la val prweighting_crra prweighting

g coed=cond(school==11|school==14|school==16|school==21|school==24|school==26,1,0) 

g double coord_y_real=(1-coord_x/intercept_x)*intercept_y

foreach x of varlist demx_mmi_ind demy_mmi_ind demx_crra_mmi_ind demy_crra_mmi_ind {
g double `x'2=`x' if mover==0 & game_type==2
sort groupid round mover
bysort groupid round: replace `x'2=`x'2[1]
g double `x'3=`x' if mover==1 & game_type==2
gsort groupid round -mover
bysort groupid round: replace `x'3=`x'3[1]
replace `x'2=`x'3 if mover==0
drop `x'3
egen double min_`x'=rowmin(`x' `x'2)
egen double max_`x'=rowmax(`x' `x'2)
}
g pe = cond(coord_x>=round(min_demx_mmi_ind,1) & coord_x<=round(max_demx_mmi_ind,1),1,0)
g pe_crra = cond(coord_x>=round(min_demx_crra_mmi_ind,1) & coord_x<=round(max_demx_crra_mmi_ind,1),1,0)

g double x_pe=demx_mmi_ind
replace x_pe=demx_mmi_ind2 if abs(demx_mmi_ind-coord_x)>abs(demx_mmi_ind2-coord_x)
g double y_pe=demy_mmi_ind
replace y_pe=demy_mmi_ind2 if abs(demy_mmi_ind-coord_y_real)>abs(demy_mmi_ind2-coord_y_real)

g double x_pe_crra=demx_crra_mmi_ind
replace x_pe_crra=demx_crra_mmi_ind2 if abs(demx_crra_mmi_ind-coord_x)>abs(demx_crra_mmi_ind2-coord_x)
g double y_pe_crra=demy_crra_mmi_ind
replace y_pe_crra=demy_crra_mmi_ind2 if abs(demy_crra_mmi_ind-coord_y_real)>abs(demy_crra_mmi_ind2-coord_y_real)

sort id round_number
foreach var in a_mmi a_crra_mmi r_mmi r_crra_mmi {
g `var'_ind=`var'
bysort id: replace `var'_ind=`var'[1]
} 
 
g double x_w=cond(intercept_x>=intercept_y,0,intercept_x)
g double y_w=cond(intercept_x>=intercept_y,intercept_y,0)
g double x_w2= 1/(1/intercept_x + 1/intercept_y)

#delimit ; 
g mistake=cond((intercept_x<intercept_y&coord_x>coord_y_real)
				|(intercept_x>intercept_y&coord_x<coord_y_real),1,0) ; 
 
g double uic=-a_mmi_ind*exp(-r_mmi_ind*coord_x)
			-(1-a_mmi_ind)*exp(-r_mmi_ind*coord_y_real) if coord_x<coord_y_real ;
replace uic=-a_mmi_ind*exp(-r_mmi_ind*coord_y_real)
			-(1-a_mmi_ind)*exp(-r_mmi_ind*coord_x) if coord_x>=coord_y_real ;
g double uii=-a_mmi_ind*exp(-r_mmi_ind*demx_mmi_ind)
		-(1-a_mmi_ind)*exp(-r_mmi_ind*demy_mmi_ind) if demx_mmi_ind<demy_mmi_ind ;
replace uii=-a_mmi_ind*exp(-r_mmi_ind*demy_mmi_ind)
		-(1-a_mmi_ind)*exp(-r_mmi_ind*demx_mmi_ind) if demx_mmi_ind>=demy_mmi_ind ;
g double uipec=-a_mmi_ind*exp(-r_mmi_ind*x_pe)
		-(1-a_mmi_ind)*exp(-r_mmi_ind*y_pe) if x_pe<y_pe ;
replace uipec=-a_mmi_ind*exp(-r_mmi_ind*y_pe)
		-(1-a_mmi_ind)*exp(-r_mmi_ind*x_pe) if x_pe>=y_pe ;
g double uiw=-a_mmi_ind*exp(-r_mmi_ind*x_w)
		-(1-a_mmi_ind)*exp(-r_mmi_ind*y_w) if x_w<y_w ;
replace uiw=-a_mmi_ind*exp(-r_mmi_ind*y_w)
		-(1-a_mmi_ind)*exp(-r_mmi_ind*x_w) if x_w>=y_w ;
g double uiw2= -exp(-r_mmi_ind*x_w2) ;
replace uiw=uiw2 if uiw>uiw2 ;
drop uiw2 ;
 
g double uic_crra=a_crra_mmi_ind*((coord_x+0.01)^(1-r_crra_mmi_ind))
	+(1-a_crra_mmi_ind)*((coord_y_real+0.01)^(1-r_crra_mmi_ind)) if coord_x<coord_y_real ;
replace uic_crra=a_crra_mmi_ind*((coord_y_real+0.01)^(1-r_crra_mmi_ind))
	+(1-a_crra_mmi_ind)*((coord_x+0.01)^(1-r_crra_mmi_ind)) if coord_x>=coord_y_real ;
g double uii_crra=a_crra_mmi_ind*((demx_crra_mmi_ind+0.01)^(1-r_crra_mmi_ind))
	+(1-a_crra_mmi_ind)*((demy_crra_mmi_ind+0.01)^(1-r_crra_mmi_ind)) if demx_crra_mmi_ind<demy_crra_mmi_ind ;
replace uii_crra=a_crra_mmi_ind*((demy_crra_mmi_ind+0.01)^(1-r_crra_mmi_ind))
	+(1-a_crra_mmi_ind)*((demx_crra_mmi_ind+0.01)^(1-r_crra_mmi_ind)) if demx_crra_mmi_ind>=demy_crra_mmi_ind ;
g double uipec_crra=a_crra_mmi_ind*((x_pe_crra+0.01)^(1-r_crra_mmi_ind))
	+(1-a_crra_mmi_ind)*((y_pe_crra+0.01)^(1-r_crra_mmi_ind)) if x_pe_crra<y_pe_crra ;
replace uipec_crra=a_crra_mmi_ind*((y_pe_crra+0.01)^(1-r_crra_mmi_ind))
	+(1-a_crra_mmi_ind)*((x_pe_crra+0.01)^(1-r_crra_mmi_ind)) if x_pe_crra>=y_pe_crra ;
g double uiw_crra=a_crra_mmi_ind*((x_w+0.01)^(1-r_crra_mmi_ind))
		+(1-a_crra_mmi_ind)*((y_w+0.01)^(1-r_crra_mmi_ind)) if x_w<y_w ;
replace uiw_crra=a_crra_mmi_ind*((y_w+0.01)^(1-r_crra_mmi_ind))
		+(1-a_crra_mmi_ind)*((x_w+0.01)^(1-r_crra_mmi_ind)) if x_w>=y_w ;
g double uiw_crra2= (x_w2+0.01)^(1-r_crra_mmi_ind) ;
replace uiw_crra=uiw_crra2 if uiw_crra>uiw_crra2 ;
drop uiw_crra2 ;
#delimit cr

 
*** For numerical search of Pareto improvement
preserve
keep if (uiw>uipec & pe==0) | (uipec<uic & pe==0)
keep id round_number a_mmi_ind r_mmi_ind coord* intercept* x_w2
export excel using "C:₩Users₩minseon₩Dropbox₩RP₩Estimation₩1.RP_CARA₩data_PE_numerical.xls", sheetreplace

import excel "C:₩Users₩minseon₩Dropbox₩RP₩Estimation₩1.RP_CARA₩numericalPE.xls", sheet("Sheet1") clear
ren (A B C D) (id round_number x_pi y_pi)
save PE_numerical.dta, replace
restore

*** Merge numerical search result
merge 1:1 id round_number using PE_numerical.dta
ren _merge _PE
sort groupid round_number x_pi
bysort groupid round_number: replace x_pi=x_pi[1]
bysort groupid round_number: replace y_pi=y_pi[1]
 
#delimit ; 
replace uipec=-a_mmi_ind*exp(-r_mmi_ind*x_pi)
		-(1-a_mmi_ind)*exp(-r_mmi_ind*y_pi) if (x_pi~=. & y_pi~=.) & (x_pi < y_pi) ;
replace uipec=-a_mmi_ind*exp(-r_mmi_ind*y_pi)
		-(1-a_mmi_ind)*exp(-r_mmi_ind*x_pi) if (x_pi~=. & y_pi~=.) & (x_pi >= y_pi) ;
delimit cr 
 
g double uloss= cond(pe==0,(uipec-uic)/(uipec-uiw),0)
replace uloss=. if uipec<uic&pe==0|uipec<uiw&pe==0
g double uloss_crra= cond(pe_crra==0,(uipec_crra-uic_crra)/(uipec_crra-uiw_crra),0)

g double uloss_ind=(uii-uic)/(uii-uiw)
replace uloss_ind=0 if uloss_ind<0 //for all 25 cases, x_c=x*_i, but uloss_ind<0 for minor computation ecrraor in Stata

cap drop uic uimin uipi

replace risk_q1=6-risk_q1
recode risk_q3 (2=1) (1=2)
 
save Risk_merged.dta, replace


*-------------------------------------------------------------------------------
*                               Data Reformulation
*------------------------------------------------------------------------------;

use Risk_merged.dta, clear

collapse (mean) groupid partner mover ccei ccei_com varian riskaversion ///
a_mmi r_mmi a_crra_mmi r_crra_mmi rdu rdu_crra riskpremium riskpremium_crra ///
a_nl r_nl a_crra_nl r_crra_nl ///
pe pe_crra uloss uloss_crra uloss_ind ///
class mathscore male coed outgoing opened agreeable conscientious stable ///
height height_gr point pointed mutual distance Outdegree Indegree ///
risk_q1 risk_q2 risk_q3 , by(id game_type)

ren (Indegree Outdegree) (indegree outdegree)

sort id game_type
bysort id: replace uloss=uloss[2]
bysort id: replace uloss_crra=uloss_crra[2]

gsort id -game_type
foreach var of varlist pe pe_crra uloss uloss_crra uloss_ind {
bysort id: replace `var'=`var'[1]
}

replace ccei_com=0.935271 if id==1610627
reshape wide ccei varian riskaversion riskpremium riskpremium_crra ///
a_mmi r_mmi a_crra_mmi r_crra_mmi a_nl r_nl a_crra_nl r_crra_nl ///
rdu rdu_crra , i(id) j(game_type)
ren *2 *_col
ren *1 *_ind
ren (risk_q_ind risk_q_col) (risk_q1 risk_q2)

g ccei_ind99=cond(ccei_ind>=0.99,1,0)
g ccei_ind95=cond(ccei_ind>=0.95,1,0)
g ccei_ind90=cond(ccei_ind>=0.90,1,0)
g ccei_col99=cond(ccei_col>=0.99,1,0)
g ccei_col95=cond(ccei_col>=0.95,1,0)
g ccei_col90=cond(ccei_col>=0.90,1,0)

g varian_ind99=cond(varian_ind>=0.99,1,0)
g varian_ind95=cond(varian_ind>=0.95,1,0)
g varian_ind90=cond(varian_ind>=0.90,1,0)
g varian_col99=cond(varian_col>=0.99,1,0)
g varian_col95=cond(varian_col>=0.95,1,0)
g varian_col90=cond(varian_col>=0.90,1,0)

foreach x of varlist riskaversion_ind riskpremium_ind riskpremium_crra_ind {
su `x', de 
g `x'_hi=cond(`x'>=r(p50),1,0)
}

egen ccei_min=rowmin(ccei_ind ccei_col)
g ccei_dif=ccei_min-ccei_com

g school=floor(id/10000)
egen tag=tag(class)
gsort school -tag class
bysort school: g experimenter=_n if tag==1
sort class experimenter
bysort class: replace experimenter=experimenter[1]
replace experimenter=5 if (class==12202 | class==24101)
la define experimenter 1 "shin" 2 "hwang" 3 "gu" 4 "lee" 5 "park"
la val experimenter experimenter

bysort class: g classnum=_N
g indegree_n=indegree/classnum
g outdegree_n=outdegree/classnum
save Risk_ByIndiv.dta, replace

use Risk_ByIndiv.dta, clear

sort groupid mover
foreach var of varlist a_mmi_ind r_mmi_ind a_crra_mmi_ind r_crra_mmi_ind ///
ccei_ind ccei_dif ccei_ind99 ccei_ind95 ccei_ind90 ///
varian_ind varian_ind99 varian_ind95 varian_ind90 ///
riskaversion_ind riskpremium_ind riskpremium_crra_ind rdu_ind rdu_crra_ind ///
riskaversion_ind_hi riskpremium_ind_hi riskpremium_crra_ind_hi ///
male mathscore outgoing opened agreeable conscientious stable ///
height height_gr indegree_n outdegree_n ///
risk_q1 risk_q2 risk_q3 uloss uloss_crra uloss_ind {
g `var'2=`var' if mover==0
bysort group: replace `var'2=`var'2[1]
}
drop if mover==0

foreach x of varlist ccei_ind varian_ind riskaversion_ind riskpremium_ind ///
riskpremium_crra_ind ///
height_gr mathscore outgoing opened agreeable conscientious stable ///
indegree_n outdegree_n {
egen `x'_max=rowmax(`x' `x'2)
g `x'_dist=abs(`x'-`x'2)
}

g malepair=cond(male==1&male2==1,2,0)
replace malepair=1 if male==1&male2==2 | male==2&male2==1
replace malepair=3 if male==2&male2==2
replace malepair=. if malepair==0
la de malepair 1 "Hetero" 2 "Boys" 3 "Girls"
la val malepair malepair
tab malepair

g malepair_co=cond(coed==0,0,.)
replace malepair_co=1 if coed==0&malepair==3
replace malepair_co=2 if coed==1&malepair==1
replace malepair_co=3 if coed==1&malepair==2
replace malepair_co=4 if coed==1&malepair==3

la de malepair_co 0 "Non Coed_Boys" 1 "Non Coed_Girls" 2 "Coed_Hetero" 3 "Coed_Boys" 4 "Coed_Girls"
la val malepair_co malepair_co

g friendship=cond(point==1&pointed==1,2,0)
replace friendship=1 if pointed==1&point==0
replace friendship=1 if pointed==0&point==1
replace friendship=0 if pointed==0&point==0
la de friendship 2 "Mutual" 1 "One sided" 0 "Nothing"
la val friendship friendship 
tab friendship

foreach t in 99 95 90 {
g ccei_both`t'=cond(ccei_ind`t'==1|ccei_ind`t'2==1,1,0)
replace ccei_both`t' =2 if ccei_ind`t'==1&ccei_ind`t'2==1

la define ccei_both`t' 0 "Both Irrational" 1 "Only One is Rational" 2 "Both Rational"
la val ccei_both`t' ccei_both`t'
}
foreach t in 99 95 90 {
g varian_both`t'=cond(varian_ind`t'==1|varian_ind`t'2==1,1,0)
replace varian_both`t' =2 if varian_ind`t'==1&varian_ind`t'2==1

la define varian_both`t' 0 "Both Irrational" 1 "Only One is Rational" 2 "Both Rational"
la val varian_both`t' varian_both`t'
}

foreach x in riskaversion riskpremium riskpremium_crra {
g `x'pair=cond(`x'_ind_hi==1 & `x'_ind_hi2==1,2,1)
replace `x'pair=0 if `x'_ind_hi==0 & `x'_ind_hi2 ==0
}

la de riskaversionpair 0 "Both Less Risk Averse" 1 "Hetero" 2 "Both Risk Averse"
la val riskaversionpair riskaversionpair
la val riskpremiumpair riskaversionpair
la val riskpremium_crrapair riskaversionpair

g rdupair=cond(rdu_ind==0&rdu_ind2==0,0,.)
replace rdupair=1 if rdu_ind==0&rdu_ind2==1 | rdu_ind==1&rdu_ind2==0
replace rdupair=2 if rdu_ind==1&rdu_ind2==1
la de rdupair 0 "Both EUT" 1 "EUT and RDU" 2 "Both RDU"
la val rdupair rdupair

g rdu_crrapair=cond(rdu_crra_ind==0&rdu_crra_ind2==0,0,.)
replace rdu_crrapair=1 if rdu_crra_ind==0&rdu_crra_ind2==1 | rdu_crra_ind==1&rdu_crra_ind2==0
replace rdu_crrapair=2 if rdu_crra_ind==1&rdu_crra_ind2==1
la val rdu_crrapair rdupair

gsort -riskpremium_col
g group=_n
egen riskpremium_lo=rowmin(riskpremium_ind riskpremium_ind2)
egen riskpremium_hi=rowmax(riskpremium_ind riskpremium_ind2)

gsort -riskpremium_crra_col
g group_rr=_n
egen riskpremium_crra_lo=rowmin(riskpremium_crra_ind riskpremium_crra_ind2)
egen riskpremium_crra_hi=rowmax(riskpremium_crra_ind riskpremium_crra_ind2)

g premiumaggre=.
recode premiumaggre (.=4) if riskpremium_col>=riskpremium_lo & riskpremium_col<=riskpremium_hi & abs(riskpremium_hi-riskpremium_col)<abs(riskpremium_lo-riskpremium_col)
recode premiumaggre (.=2) if riskpremium_col>=riskpremium_lo & riskpremium_col<=riskpremium_hi & abs(riskpremium_hi-riskpremium_col)>abs(riskpremium_lo-riskpremium_col)
recode premiumaggre (.=5) if riskpremium_col>riskpremium_hi
recode premiumaggre (.=1) if riskpremium_col<riskpremium_lo 
replace premiumaggre = 3 if riskpremium_lo ==riskpremium_hi == riskpremium_col

la de premiumaggre 4 "Risk Averse Leader" 2 "Risk Neutral Leader" 5 "Heading Aversion" 1 "Heading Loving" 3 "Homogeneous"
la val premiumaggre premiumaggre 
tab premiumaggre
tab premiumaggre if ccei_ind>=0.95&ccei_ind2>=0.95

g premium_rraggre=.
recode premium_rraggre (.=4) if riskpremium_crra_col>=riskpremium_crra_lo & riskpremium_crra_col<=riskpremium_crra_hi & abs(riskpremium_crra_hi-riskpremium_crra_col)<abs(riskpremium_crra_lo-riskpremium_crra_col)
recode premium_rraggre (.=2) if riskpremium_crra_col>=riskpremium_crra_lo & riskpremium_crra_col<=riskpremium_crra_hi & abs(riskpremium_crra_hi-riskpremium_crra_col)>abs(riskpremium_crra_lo-riskpremium_crra_col)
recode premium_rraggre (.=5) if riskpremium_crra_col>riskpremium_crra_hi
recode premium_rraggre (.=1) if riskpremium_crra_col<riskpremium_crra_lo 
replace premium_rraggre = 3 if riskpremium_crra_lo ==riskpremium_crra_hi == riskpremium_crra_col

la de premium_rraggre 4 "Risk Averse Leader" 2 "Risk Neutral Leader" 5 "Heading Aversion" 1 "Heading Loving" 3 "Homogeneous"
la val premium_rraggre premium_rraggre 
tab premium_rraggre
tab premium_rraggre if ccei_ind>=0.95&ccei_ind2>=0.95

egen riskq1=rowmean(risk_q1 risk_q12)
egen riskq2=rowmean(risk_q2 risk_q22)

g riskq3=.
replace riskq3=1 if risk_q3==1& risk_q32==1
replace riskq3=2 if risk_q3==2& risk_q32==3 | risk_q3==3& risk_q32==2
replace riskq3=3 if risk_q3==1& risk_q32==2 | risk_q3==2& risk_q32==1
replace riskq3=3 if risk_q3==1& risk_q32==3 | risk_q3==3& risk_q32==1
replace riskq3=3 if risk_q3==1& risk_q32==4 | risk_q3==4& risk_q32==1
replace riskq3=4 if risk_q3==2& risk_q32==2 
replace riskq3=4 if risk_q3==2& risk_q32==4 | risk_q3==4& risk_q32==2
replace riskq3=4 if risk_q3==3& risk_q32==3 
replace riskq3=4 if risk_q3==3& risk_q32==4 | risk_q3==4& risk_q32==3
replace riskq3=4 if risk_q3==4& risk_q32==4 

la define riskq3 1 "Both reflected" 2 "One takes initiative" 3 "Either thinks both reflected" 4 "The rest"
la val riskq3 riskq3

egen uloss_col=rowmean(uloss uloss2)

la var varian_col ColVarian
la var ccei_col ColCCEI
la var riskaversion_col ColRA
la var riskpremium_col ColRP
la var riskpremium_crra_col ColRP_CRRA
la var rdu_col ColRDU

g varian_max_nonmover=cond(varian_ind_max==varian_ind2,varian_ind2,0)
g ccei_max_nonmover=cond(ccei_ind_max==ccei_ind2,ccei_ind2,0)
g riskaversion_max_nonmover=cond(riskaversion_ind_max==riskaversion_ind2,riskaversion_ind2,0)
g riskpremium_max_nonmover=cond(riskpremium_ind_max==riskpremium_ind2,riskpremium_ind2,0)
g riskpremium_crra_max_nonmover=cond(riskpremium_crra_ind_max==riskpremium_crra_ind2,riskpremium_crra_ind2,0)
g rdu_nonmover=cond(rdu_ind2==1,rdu_ind2,0)
g rdu_crra_nonmover=cond(rdu_crra_ind2==1,rdu_crra_ind2,0)

save Risk_ByPair.dta, replace

*-------------------------------------------------------------------------------
*                    Relative Price and Consumption Graph
*------------------------------------------------------------------------------;

use Risk_merged.dta 
egen temp22=tag(groupid)
sort temp22 groupid
egen temp3=rank(groupid) if temp22==1
gsort groupid -temp22
bysort groupid : replace temp3=temp3[1]

form ccei_col %9.2f
form ccei_ind %9.2f
form riskaversion %9.2f

quietly: su temp3
local M= r(max)
forvalues i=284/284 {
quietly: su id if temp3==`i' & mover==1
local id_mover = r(mean)
quietly: su id if temp3==`i' & mover==0
local id_stayer = r(mean)

quietly: su ccei_ind if temp3==`i' & mover==0
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskaversion if temp3==`i' & mover==0 & game_type==1
local RP = trim("`: display %9.2f r(mean)'") 

twoway (scatter relconsmp lnrelprice if temp3==`i'& mover==0 & game_type==1, msize(large) msymbol(circle_hollow))  ///
       (scatter relconsmp lnrelprice if temp3==`i'& mover==0 & game_type==2, msize(large) msymbol(+)), ///
	   legend(order(1 "Individual" 2 "collective")) xlabel(-2(1)2) xtitle("log(p2/p1)") ylabel(0(0.2)1) ytitle("x2/(x1+x2)") saving(temp1, replace) /// 
	   note("Individual CCEI=`C'" "Risk Preference=`RP', EUT") caption(Id=`id_stayer')

quietly: su ccei_ind if temp3==`i' & mover==1
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskaversion if temp3==`i' & mover==1 & game_type==1
local RP = trim("`: display %9.2f r(mean)'") 

twoway (scatter relconsmp lnrelprice if temp3==`i'& mover==1 & game_type==1, msize(large) msymbol(circle_hollow))  ///
       (scatter relconsmp lnrelprice if temp3==`i'& mover==1 & game_type==2, msize(large) msymbol(+)), ///
	   legend(order(1 "Individual" 2 "collective")) xlabel(-2(1)2) xtitle("log(p2/p1)") ylabel(0(0.2)1) ytitle("x2/(x1+x2)") saving(temp2, replace) ///   
	   note("Individual CCEI=`C'" "Risk Preference=`RP', RDU") caption(Id=`id_mover')
	   
quietly: su ccei_col if temp3==`i' & id== `id_mover'  
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskaversion if temp3==`i' & id== `id_mover' & game_type==2
local RP = trim("`: display %9.2f r(mean)'") 

gr combine temp1.gph temp2.gph, title(Group ID=`i') ///
subtitle("Collective CCEI=`C'" "Risk Preference =`RP', RDU") note("Risk Preference=(1/18)%XA2%XB2(x_cheaper/x1+x2)")
gr export relconsumption`i'.png, replace
}

preserve

*Why so many cases where alpha<0.05?
keep if alpha<0.05 //

replace alpha=alpha_cor 
keep if alpha_cor<0.05

egen tag1=tag(id) if game_type=="individual" & ccei_ind>=0.9
egen temp4=rank(id) if game_type=="individual" & tag1==1
gsort id -tag
bysort id: replace temp4 = temp4[1]

egen tag2=tag(temp3) 
egen temp5=rank(temp3) if game_type=="collective" & tag2==1 & ccei_col>=0.9
gsort temp3 -tag2
bysort temp3: replace temp5 = temp5[1]

su temp4 
local M=r(max)
forvalues i=1/`M' {
quietly: su id if temp4==`i'
local id = r(mean)

quietly: su ccei_ind if temp4==`i' 
local C = trim("`: display %9.2f r(mean)'") 
quietly: su alpha if temp4==`i' 
local alpha = trim("`: display %9.2f r(mean)'") 
quietly: su rho if temp4==`i' 
local rho = trim("`: display %9.2f r(mean)'") 

twoway (scatter relconsmp lnrelprice if temp4 ==`i' & game_type=="individual", msize(large) msymbol(circle_hollow)), ///
	   legend(order(1 "Individual")) xlabel(-2(1)2) xtitle("log(p_y/p_x)") ylabel(0(0.2)1) ytitle("y/(x+y)") /// 
	   note(Individual CCEI(Alpha)(Rho)=`C'(`alpha')(`rho')) caption(Id=`id')
gr export a_mmi_ind`i'_cor2.png, replace
}

su temp5
local M=r(max)
forvalues i=1/`M' {
quietly: su id if temp5==`i'
local id_min = r(min)
local id_max = r(max)

su temp3 if temp5==`i'
local group= r(mean)

quietly: su ccei_col if temp5==`i' 
local C = trim("`: display %9.2f r(mean)'") 
quietly: su alpha if temp5==`i' 
local alpha = trim("`: display %9.2f r(mean)'") 
quietly: su rho if temp5==`i' 
local rho = trim("`: display %9.2f r(mean)'") 


twoway (scatter relconsmp lnrelprice if temp5 ==`i' & game_type=="collective", msize(large) msymbol(circle_hollow)), ///
	   legend(order(1 "collective")) xlabel(-2(1)2) xtitle("log(p_y/p_x)") ylabel(0(0.2)1) ytitle("y/(x+y)") /// 
	   note(Collective CCEI(Alpha)(Rho)=`C'(`alpha')(`rho')) caption(Group Id=`group' Ind. Id=`id_min' and`id_max')
gr export alpha_col`i'_cor2.png, replace
}
restore
