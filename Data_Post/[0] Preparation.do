*-------------------------------------------------------------------------------
*                        DATA cleaning and Merging Data sets
*------------------------------------------------------------------------------;
cd "C:\Users\master\Dropbox\RP\Data_Post"

use Risk_Post_Raw.dta, clear
cap ren subsessiongame_type game_type
cap replace game_type="1" if game_type=="individual"
cap replace game_type="2" if game_type=="collective"
destring *, replace
merge m:1 participantlabel game_type using GARPResults_Post.dta
ren _merge _GARP

ren (player* participant*) (* *)
format %10.0g id
sort id partner
bysort id: replace partner=partner[36]
cap ren partner_both partner
sort id is_mover
bysort id: replace is_mover=is_mover[1]

cap drop id_in_session code _is_bot _index_in_pages _max_page_index _current_app_name _round_number _current_page_name ip_address time_started exclude_from_data_ana visited mturk_worker_id mturk_assignment_id id_in_group  payoff groupid_in_subsession  sessionlabel sessionexperimenter_name sessiontime_scheduled sessiontime_started sessionmturk_hitid sessionmturk_hitgroupid sessioncomment sessionis_demo temp temp1 temp2

order id, before(coord_x)
ren (e ev subsession*) (ccei varian *)
g t=1
ren id id_new

merge m:m label sessioncode using survey_post.dta
ren (player* participant*) (* *)
ren cq* playercq*
cap drop id_in_session code _is_bot _index_in_pages _max_page_index _current_app_name _round_number _current_page_name ip_address time_started exclude_from_data_ana visited mturk_worker_id mturk_assignment_id id_in_group  payoff groupid_in_subsession  sessionlabel sessionexperimenter_name sessiontime_scheduled sessiontime_started sessionmturk_hitid sessionmturk_hitgroupid sessioncomment sessionis_demo temp temp1 temp2
drop id_in_session code _is_bot _index_in_pages _max_page_index _current_app_name _round_number _current_page_name ip_address time_started exclude_from_data_an visited mturk_worker_id mturk_assignment_id id_in_group 
drop groupid_in_subsession subsessionround_number sessionlabel sessionexperimenter_name sessiontime_scheduled sessiontime_started sessionmturk_HITId sessionmturk_HITGroupId sessioncomment sessionis_demo temp1
drop if _merge==2
ren _merge _survey
save Risk_merged_post.dta, replace

use GARPResults_Post.dta, clear
keep participantlabel id
ren participantlabel partner
ren id id_partner
format %10.0g id_partner
bysort id_partner: g temp=_n
keep if temp==1
drop temp
merge 1:m partner using Risk_merged_post.dta
drop if _merge==1

replace id_partner=id_new if id_partner==.
sort id_partner is_mover
bysort id_partner: replace id_partner=id_new[1] if id_partner==id_new
drop if id_new==id_partner
ren _merge id_parter
merge m:1 id_new game_type using CARA_estimation_post.dta
ren _merge _cara
drop if _cara==2
merge m:1 id_new game_type using CRRA_estimation_post.dta
ren _merge _crra
drop if _crra==2

egen id_max=rowmax(id_new id_partner)
g temp=cond(alpha~=.,1,0)
gsort id_max -temp -round_number

foreach var of varlist alpha alpha_std rho rho_std ssr alpha_rr alpha_rr_std rho_rr rho_rr_std ssr_rr{
bysort id_max: replace `var'=`var'[1] if `var'==.
}

save Risk_merged_post.dta, replace

/***************DATA FOR ESTIMATION OF RISK PARAMETER
keep id coord* intercept* game_type partner is_mover *round_number
save data_daegu_post.dta, replace
g bothgame=cond(id_partner~=id,1,0)

sort id game_type subsessionround_number
drop if is_mover==1 & game_type==2
keep id subsessionround_number coord* intercept* game_type 
order id subsessionround_number coord* intercept* game_type */
/**************DATA FOR ESTIMATION OF OPTIMAL CHOICES

foreach var in alpha alpha_rr rho rho_rr {
g `var'_ind=`var' if game_type==1
sort id_new round_number
bysort id_new: replace `var'_ind=`var'_ind[1]
}
keep id_new round_number coord* intercept* alpha rho alpha_rr rho_rr alpha_ind rho_ind alpha_rr_ind rho_rr_ind
keep if round_number>=19
sort id_new round_number
order round_number, after(id_new)
order rho_ind, after(alpha_ind) */

*-------------------------------------------------------------------------------
*                  Generate Variables and Preperation for Analysis
*------------------------------------------------------------------------------;
use Risk_merged_post.dta, clear

g ccei_ind=ccei if game_type==1
g varian_ind=varian if game_type==1
g ccei_col=ccei if game_type==2
g varian_col=varian if game_type==2
sort id_new game_type
bysort id_new : replace ccei_ind=ccei_ind[1]
bysort id_new : replace varian_ind=varian_ind[1]
gsort id_new -game_type
bysort id_new : replace ccei_col=ccei_col[1]
bysort id_new : replace varian_col=varian_col[1]
drop ccei varian

g riskpref = .
replace riskpref = coord_y/(coord_x+coord_y) if intercept_x<intercept_y 
replace riskpref = coord_x/(coord_x+coord_y) if intercept_x>intercept_y 
replace riskpref = coord_x/(coord_x+coord_y) if intercept_x==intercept_y & coord_x>coord_y
replace riskpref = coord_y/(coord_x+coord_y) if intercept_x==intercept_y & coord_x<coord_y
la var riskpref RiskPreference

g lnrelprice=log(intercept_x/intercept_y)
g relconsmp = coord_y/(coord_x+coord_y)

drop partner
ren (id_partner is_mover) (partner mover)
drop if id_new==partner

 replace alpha=0.5 if alpha<=0.0000001
 replace rho=0 if alpha<=0.0000001
 
 su rho if rho~=0
 local rhomin=r(min)
 replace rho=`rhomin' if rho==0

g rdu=cond(alpha>=0.5-alpha_std*1.96&alpha<=0.5+alpha_std*1.96,0,1)
g rdu_rr=cond(alpha_rr>=0.5-alpha_rr_std*1.96&alpha_rr<=0.5+alpha_rr_std*1.96,0,1)
la define rdu 0 "EUT" 1 "RDU"
la val rdu rdu
la define rdu_rr 0 "EUT" 1 "RDU"
la val rdu_rr rdu_rr

g prweighting=rdu
replace prweighting=2 if alpha>0.5+alpha_std*1.96
g prweighting_rr=rdu_rr
replace prweighting_rr=2 if alpha_rr>0.5+alpha_rr_std*1.9
la define prweighting 0 "EUT" 1 "Optimistic" 2 "Pessimstic"
la val prweighting prweighting
la define prweighting_rr 0 "EUT" 1 "Optimistic" 2 "Pessimstic"
la val prweighting_rr prweighting_rr

g cq1=cond(playercq1==3,1,0)
g cq2=cond(playercq2==1,1,0)
g cq3=cond(playercq3==5,1,0)
g cq4=cond(playercq4==2,1,0)
g cq5=cond(playercq5==5,1,0)
egen mathscore=rowtotal(cq1 cq2 cq3 cq4 cq5)

g school=floor(id_new/100000)
g coed=cond(school==11|school==14|school==16|school==21|school==24|school==26,1,0)
 
 cap drop Playercq* cq* temp* init_coord_x init_coord_y Participantcode Participanttime_started Sessioncode  _ccei _ccei_com _cara _crra _network _network1

merge 1:1 id_new round_number using estdemand_Post.dta
ren _merge _estdem

foreach var of varlist est_ind_x est_ind_y est_rr_ind_x est_rr_ind_y {
g double `var'2=`var' if mover==0 & game_type==2
sort id_max round mover
bysort id_max round: replace `var'2=`var'2[1]
g double `var'3=`var' if mover==1 & game_type==2
gsort id_max round -mover
bysort id_max round: replace `var'3=`var'3[1]
replace `var'2=`var'3 if mover==0
drop `var'3
}

 g double coord_y_real=(1-coord_x/intercept_x)*intercept_y
 egen double minestind_x=rowmin(est_ind_x est_ind_x2)
 egen double maxestind_x=rowmax(est_ind_x est_ind_x2)
 egen double maxestrrind_x=rowmax(est_rr_ind_x est_rr_ind_x2)
 egen double minestrrind_x=rowmin(est_rr_ind_x est_rr_ind_x2)
  
 g pe = cond(coord_x>=round(minestind_x,1) & coord_x<=round(maxestind_x,1),1,0)
 g pe_rr = cond(coord_x>=round(minestrrind_x,1) & coord_x<=round(maxestrrind_x,1),1,0)
 
 g double x_pe=est_ind_x
 replace x_pe=est_ind_x2 if abs(est_ind_x-coord_x)>abs(est_ind_x2-coord_x)
 g double y_pe=est_ind_y
 replace y_pe=est_ind_y2 if abs(est_ind_y-coord_y_real)>abs(est_ind_y2-coord_y_real)
 
 /*replace x_pe=est_ind_x2 if id_new==1110214 & round_number==36 //dist(individual optimum, collective) is same so disturbance exists : checked whether px=1 or not
 replace x_pe=est_ind_x if id_new==1110215 & round_number==36
 replace x_pe=est_ind_x if id_new==1220121 & round_number==24 
 replace x_pe=est_ind_x2 if id_new==1220105 & round_number==24
 replace x_pe=est_ind_x if id_new==1610314 & round_number==19 
 replace x_pe=est_ind_x2 if id_new==1610325 & round_number==19*/
 
 merge 1:1 id_new round_number using PE_numerical_Post.dta
 ren _merge _PE
 sort id_max round_number pi_x
 bysort id_max round_number: replace pi_x=pi_x[1]
 bysort id_max round_number: replace pi_y=pi_y[1]
 
 replace x_pe=pi_x if pi_x~=.
 replace y_pe=pi_y if pi_y~=. 
 
 
 g double x_pe_rr=est_rr_ind_x
 replace x_pe_rr=est_rr_ind_x2 if abs(est_rr_ind_x-coord_x)>abs(est_rr_ind_x2-coord_x)
 g double y_pe_rr=est_rr_ind_y
 replace y_pe_rr=est_rr_ind_y2 if abs(est_rr_ind_y-coord_y_real)>abs(est_rr_ind_y2-coord_y_real)
 
 /*replace x_pe_rr=est_rr_ind_x2 if id_new==1110518 & round_number==23 //dist(individual optimum, collective) is same so disturbance exists
 replace x_pe_rr=est_rr_ind_x if id_new==1110513 & round_number==23
 replace x_pe_rr=est_rr_ind_x if id_new==1120426 & round_number==34 
 replace x_pe_rr=est_rr_ind_x2 if id_new==1120407 & round_number==34
 replace x_pe_rr=est_rr_ind_x if id_new==1220115 & round_number==24 
 replace x_pe_rr=est_rr_ind_x2 if id_new==1220109 & round_number==24
 replace x_pe_rr=est_rr_ind_x if id_new==1310128 & round_number==26 
 replace x_pe_rr=est_rr_ind_x2 if id_new==1310123 & round_number==26
 replace x_pe_rr=est_rr_ind_x2 if id_new==2220315 & round_number==23 
 replace x_pe_rr=est_rr_ind_x if id_new==2220304 & round_number==23  
 replace x_pe_rr=est_rr_ind_x if id_new==2410116 & round_number==28 
 replace x_pe_rr=est_rr_ind_x2 if id_new==2410113 & round_number==28 */
 

 sort id_new round_number
 foreach var in alpha alpha_rr rho rho_rr {
 g `var'_ind=`var'
 bysort id_new: replace `var'_ind=`var'[1]
 } 
 
 g double x_w=cond(intercept_x>=intercept_y,0,intercept_x)
 g double y_w=cond(intercept_x>=intercept_y,intercept_y,0)
 g double x_w2= 1/(1/intercept_x + 1/intercept_y)
 
 g mistake=cond(intercept_x<intercept_y&coord_x>coord_y_real | intercept_x>intercept_y&coord_x<coord_y_real,1,0)
 
 g double uic=-alpha_ind*exp(-rho_ind*coord_x)-(1-alpha_ind)*exp(-rho_ind*coord_y_real) if coord_x<coord_y_real
 replace uic=-alpha_ind*exp(-rho_ind*coord_y_real)-(1-alpha_ind)*exp(-rho_ind*coord_x) if coord_x>=coord_y_real
 g double uii=-alpha_ind*exp(-rho_ind*est_ind_x)-(1-alpha_ind)*exp(-rho_ind*est_ind_y) if est_ind_x<est_ind_y
 replace uii=-alpha_ind*exp(-rho_ind*est_ind_y)-(1-alpha_ind)*exp(-rho_ind*est_ind_x) if uii==.
 g double uipec=cond(x_pe<y_pe,-alpha_ind*exp(-rho_ind*x_pe)-(1-alpha_ind)*exp(-rho_ind*y_pe),-alpha_ind*exp(-rho_ind*y_pe)-(1-alpha_ind)*exp(-rho_ind*x_pe)) //individualutilityatparetoefficientcollectivechoice
 g double uiw=cond(x_w<y_w,-alpha_ind*exp(-rho_ind*x_w)-(1-alpha_ind)*exp(-rho_ind*y_w),-alpha_ind*exp(-rho_ind*y_w)-(1-alpha_ind)*exp(-rho_ind*x_w)) //wortstofindividualutilityonthebudgetline
 g double uiw2= -exp(-rho_ind*x_w2)
 replace uiw=uiw2 if uiw>uiw2 
 
 
 g double uic_rr= cond(coord_x<coord_y,alpha_rr_ind*(coord_x^(1-rho_rr_ind))/(1-rho_rr_ind) + (1-alpha_rr_ind)*(coord_y^(1-rho_rr_ind))/(1-rho_rr_ind),alpha_rr_ind*(coord_y^(1-rho_rr_ind))/(1-rho_rr_ind) + (1-alpha_rr_ind)*(coord_x^(1-rho_rr_ind))/(1-rho_rr_ind))
 g double uii_rr= cond(est_rr_ind_x<est_rr_ind_y,alpha_rr_ind*(est_rr_ind_x^(1-rho_rr_ind))/(1-rho_rr_ind) + (1-alpha_rr_ind)*(est_rr_ind_y^(1-rho_rr_ind))/(1-rho_rr_ind),alpha_rr_ind*(est_rr_ind_y^(1-rho_rr_ind))/(1-rho_rr_ind) + (1-alpha_rr_ind)*(est_rr_ind_x^(1-rho_rr_ind))/(1-rho_rr_ind))
 g double uipec_rr= cond(x_pe_rr<y_pe_rr,alpha_rr_ind*(x_pe_rr^(1-rho_rr_ind))/(1-rho_rr_ind) + (1-alpha_rr_ind)*(y_pe_rr^(1-rho_rr_ind))/(1-rho_rr_ind),alpha_rr_ind*(y_pe_rr^(1-rho_rr_ind))/(1-rho_rr_ind) + (1-alpha_rr_ind)*(x_pe_rr^(1-rho_rr_ind))/(1-rho_rr_ind))
 g double uiw_rr=cond(x_w<y_w,alpha_rr_ind*(x_w^(1-rho_rr_ind))/(1-rho_rr_ind) + (1-alpha_rr_ind)*(y_w^(1-rho_rr_ind))/(1-rho_rr_ind),alpha_rr_ind*(y_w^(1-rho_rr_ind))/(1-rho_rr_ind) + (1-alpha_rr_ind)*(x_w^(1-rho_rr_ind))/(1-rho_rr_ind))
 
 g double uloss= cond(pe==0,(uipec-uic)/(uipec-uiw),0)
 replace uloss=. if uipec<uic&pe==0|uipec<uiw&pe==0
 g double uloss_rr= cond(pe_rr==0,(uipec_rr-uic_rr)/(uipec_rr-uiw_rr),0)

 g double uloss_ind=(uii-uic)/(uii-uiw)
 replace uloss_ind=0 if uloss_ind<0 //for all 25 cases, x_c=x*_i, but uloss_ind<0 for minor computation error in Stata
 
 g riskpremium = 2*alpha-1+rho*2*alpha*(1-alpha)*2.25
 g riskpremium_rr= 2*alpha_rr-1+rho_rr*2*alpha_rr*(1-alpha_rr)


 /*replace PlayerRisk_q1=6-PlayerRisk_q1
 recode PlayerRisk_q3 (2=1) (1=2)
 drop Playerq* */
 

 save Risk_merged_post.dta, replace

*-------------------------------------------------------------------------------
*                               Data Reformulation
*------------------------------------------------------------------------------;

use Risk_merged_post.dta, clear 

collapse (mean) ccei_ind ccei_col varian_ind varian_col alpha alpha_std rho rho_std alpha_rr alpha_rr_std rho_rr rho_rr_std ///
riskpref partner mover riskpremium riskpremium_rr mathscore pe pe_rr coed rdu rdu_rr prweighting prweighting_rr uloss uloss_rr uloss_ind ///ccei_com point pointed mutual distance PlayerRisk_q1 PlayerRisk_q2 PlayerRisk_q3
, by(id_new game_type)

sort id_new game_type
bysort id_new: replace uloss=uloss[2]
bysort id_new: replace uloss_rr=uloss_rr[2]

g ccei=ccei_ind if game_type==1
replace ccei=ccei_col if game_type==2
drop ccei_ind ccei_col 
g varian=varian_ind if game_type==1
replace varian=varian_col if game_type==2
drop varian_ind varian_col 

gsort id_new -game_type
foreach var of varlist pe pe_rr uloss uloss_rr uloss_ind {
bysort id_new: replace `var'=`var'[1]
}

reshape wide  alpha alpha_std rho rho_std alpha_rr alpha_rr_std rho_rr rho_rr_std ccei varian riskpref riskpremium ///
riskpremium_rr rdu rdu_rr prweighting prweighting_rr, i(id_new) j(game_type)
ren *2 *_col
ren *1 *_ind
//ren (PlayerRisk_q_ind PlayerRisk_q_col) (PlayerRisk_q1 PlayerRisk_q2)
 
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

egen ccei_min=rowmin(ccei_ind ccei_col)
//g ccei_dif=ccei_min-ccei_com
g class=floor(id_new/100)
g t=1
save Risk_ByIndiv_post.dta, replace
 

use Risk_ByIndiv_post.dta, clear 
foreach var of varlist mathscore  riskpref_ind riskpremium_ind riskpremium_rr_ind ///
riskpref_col riskpremium_col riskpremium_rr_col {
bysort class: egen med`var'=median(`var')
g hi`var'=cond(`var'>=med`var',1,0)
bysort class: egen `var'25=pctile(`var'), p(25)
bysort class: egen `var'75=pctile(`var'), p(75)
g exhi`var'=cond(`var'>=`var'75,1,.)
replace exhi`var'=0 if `var'<=`var'25
drop med`var' `var'25 `var'75
}

egen group= rowmax(id_new partner)
sort group mover
foreach var of varlist alpha_ind rho_ind alpha_rr_ind rho_rr_ind ccei_ind varian_ind  ///
ccei_ind99 ccei_ind95 ccei_ind90 varian_ind99 varian_ind95 varian_ind90 ///
himathscore  exhimathscore riskpref_ind riskpremium_ind riskpremium_rr_ind hiriskpref_ind hiriskpremium_ind hiriskpremium_rr_ind ///
 exhiriskpref_ind exhiriskpremium_ind exhiriskpremium_rr_ind rdu_ind rdu_rr_ind prweighting_ind prweighting_rr_ind ///
  uloss uloss_rr uloss_ind {
g `var'2=`var' if mover==0
bysort group: replace `var'2=`var'2[1]
} 
///ccei_com ccei_dif  sex PlayerRisk_q1 PlayerRisk_q2 PlayerRisk_q3
drop group
drop if mover==0



/*g friendship=cond(point==1&pointed==1,2,0)
replace friendship=1 if pointed==1&point==0
replace friendship=1 if pointed==0&point==1
replace friendship=0 if pointed==0&point==0
la de friendship 2 "Mutual" 1 "One sided" 0 "Nothing"
la val friendship friendship */

g scorepair=cond(himathscore==1&himathscore2==1,2,0)
replace scorepair=1 if himathscore==0&himathscore2==0
la de scorepair 2 "Both HighScore" 1 "Both LowScore" 0 "Others"
la val scorepair scorepair

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


g rpdif=abs(riskpref_ind-riskpref_ind2)
g ftndif= abs(riskpremium_ind-riskpremium_ind2)
g ftndif_rr=abs(riskpremium_rr_ind-riskpremium_rr_ind2)

foreach var in riskpref riskpremium riskpremium_rr {
g `var'pair=cond(hi`var'_ind==1&hi`var'_ind2==1,2,0)
replace `var'pair=1 if hi`var'_ind==0&hi`var'_ind2==0
g `var'pair2=cond(exhi`var'_ind==1&exhi`var'_ind2==1,3,.)
replace `var'pair2=2 if exhi`var'_ind==0&exhi`var'_ind2==0
replace `var'pair2=1 if exhi`var'_ind==.&exhi`var'_ind2==.
recode `var'pair2 (.=0) 
}

la de riskprefpair 0 "Different"  1 "Both Risk Averse" 2 "Both Risk Neutral"
la val riskprefpair riskprefpair
la de riskprefpair2 0 "Different" 1 "Both in gray zone"  2 "Both Risk Averse" 3 "Both Risk Neutral"
la val riskprefpair2 riskprefpair2

la de riskpremiumpair 0 "Different" 1 "Both Risk Neutral" 2 "Both Risk Averse"
la val riskpremiumpair riskpremiumpair
la de riskpremiumpair2 0 "Different" 1 "Both in gray zone" 2 "Both Risk Neutral" 3 "Both Risk Averse"
la val riskpremiumpair2 riskpremiumpair2

la de riskpremium_rrpair 0 "Different" 1 "Both Risk Neutral" 2 "Both Risk Averse"
la val riskpremium_rrpair riskpremium_rrpair
la de riskpremium_rrpair2 0 "Different" 1 "Both in gray zone" 2 "Both Risk Neutral" 3 "Both Risk Averse"
la val riskpremium_rrpair2 riskpremium_rrpair2

la de hiriskpref_col 0 "RiskAverse" 1 "RiskNeutral" 
la val hiriskpref_col hiriskpref_col

la de hiriskpremium_col 0 "RiskNeutral" 1 "RiskAverse" 
la val hiriskpremium_col hiriskpremium_col
la de hiriskpremium_rr_col 0 "RiskNeutral" 1 "RiskAverse"
la val hiriskpremium_rr_col hiriskpremium_rr_col

g rdupair=cond(rdu_ind==0&rdu_ind2==0,0,.)
replace rdupair=1 if rdu_ind==0&rdu_ind2==1 | rdu_ind==1&rdu_ind2==0
replace rdupair=2 if rdu_ind==1&rdu_ind2==1
la de rdupair 0 "Both EUT" 1 "EUT and RDU" 2 "Both RDU"
la val rdupair rdupair

g rdu_rrpair=cond(rdu_rr_ind==0&rdu_rr_ind2==0,0,.)
replace rdu_rrpair=1 if rdu_rr_ind==0&rdu_rr_ind2==1 | rdu_rr_ind==1&rdu_rr_ind2==0
replace rdu_rrpair=2 if rdu_rr_ind==1&rdu_rr_ind2==1
la de rdu_rrpair 0 "Both EUT" 1 "EUT and rdu_rr" 2 "Both rdu_rr"
la val rdu_rrpair rdu_rrpair

g prweightingpair=cond(prweighting_ind==0&prweighting_ind2==0,0,.)
replace prweightingpair=1 if prweighting_ind==0&prweighting_ind2==1 | prweighting_ind==1&prweighting_ind2==0
replace prweightingpair=2 if prweighting_ind==0&prweighting_ind2==2 | prweighting_ind==2&prweighting_ind2==0
replace prweightingpair=3 if prweighting_ind==1&prweighting_ind2==1 
replace prweightingpair=4 if prweighting_ind==1&prweighting_ind2==2 | prweighting_ind==2&prweighting_ind2==1
replace prweightingpair=5 if prweighting_ind==2&prweighting_ind2==2 
la de prweightingpair 0 "Both EUT" 1 "EUT and Optimism" 2 "EUT and Pessimism" 3 "Both Optimism" 4 "Pessimism and Optimism" 5 "Both Pessimism"
la val prweightingpair prweightingpair
la de prweighting_col 0 "EUT" 1 "Optimistic" 2 "Pessimistic"
la val prweighting_col prweighting_col

g prweighting_rr_pair=cond(prweighting_rr_ind==0&prweighting_rr_ind2==0,0,.)
replace prweighting_rr_pair=1 if prweighting_rr_ind==0&prweighting_rr_ind2==1 | prweighting_rr_ind==1&prweighting_rr_ind2==0
replace prweighting_rr_pair=2 if prweighting_rr_ind==0&prweighting_rr_ind2==2 | prweighting_rr_ind==2&prweighting_rr_ind2==0
replace prweighting_rr_pair=3 if prweighting_rr_ind==1&prweighting_rr_ind2==1 
replace prweighting_rr_pair=4 if prweighting_rr_ind==1&prweighting_rr_ind2==2 | prweighting_rr_ind==2&prweighting_rr_ind2==1
replace prweighting_rr_pair=5 if prweighting_rr_ind==2&prweighting_rr_ind2==2 
la de prweighting_rr_pair 0 "Both EUT" 1 "EUT and Optimism" 2 "EUT and Pessimism" 3 "Both Optimism" 4 "Pessimism and Optimism" 5 "Both Pessimism"
la val prweighting_rr_pair prweighting_rr_pair
la de prweighting_rr_col 0 "EUT" 1 "Optimistic" 2 "Pessimistic"
la val prweighting_rr_col prweighting_col

gsort -riskpremium_col
g group=_n
egen riskpremium_lo=rowmin(riskpremium_ind riskpremium_ind2)
egen riskpremium_hi=rowmax(riskpremium_ind riskpremium_ind2)

gsort -riskpremium_rr_col
g group_rr=_n
egen riskpremium_rr_lo=rowmin(riskpremium_rr_ind riskpremium_rr_ind2)
egen riskpremium_rr_hi=rowmax(riskpremium_rr_ind riskpremium_rr_ind2)

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
recode premium_rraggre (.=4) if riskpremium_rr_col>=riskpremium_rr_lo & riskpremium_rr_col<=riskpremium_rr_hi & abs(riskpremium_rr_hi-riskpremium_rr_col)<abs(riskpremium_rr_lo-riskpremium_rr_col)
recode premium_rraggre (.=2) if riskpremium_rr_col>=riskpremium_rr_lo & riskpremium_rr_col<=riskpremium_rr_hi & abs(riskpremium_rr_hi-riskpremium_rr_col)>abs(riskpremium_rr_lo-riskpremium_rr_col)
recode premium_rraggre (.=5) if riskpremium_rr_col>riskpremium_rr_hi
recode premium_rraggre (.=1) if riskpremium_rr_col<riskpremium_rr_lo 
replace premium_rraggre = 3 if riskpremium_rr_lo ==riskpremium_rr_hi == riskpremium_rr_col

la de premium_rraggre 4 "Risk Averse Leader" 2 "Risk Neutral Leader" 5 "Heading Aversion" 1 "Heading Loving" 3 "Homogeneous"
la val premium_rraggre premium_rraggre 
tab premium_rraggre
tab premium_rraggre if ccei_ind>=0.95&ccei_ind2>=0.95

/*egen riskq1=rowmean(PlayerRisk_q1 PlayerRisk_q12)
egen riskq2=rowmean(PlayerRisk_q2 PlayerRisk_q22)

g riskq3=.
replace riskq3=1 if PlayerRisk_q3==1& PlayerRisk_q32==1
replace riskq3=2 if PlayerRisk_q3==2& PlayerRisk_q32==3 | PlayerRisk_q3==3& PlayerRisk_q32==2
replace riskq3=3 if PlayerRisk_q3==1& PlayerRisk_q32==2 | PlayerRisk_q3==2& PlayerRisk_q32==1
replace riskq3=3 if PlayerRisk_q3==1& PlayerRisk_q32==3 | PlayerRisk_q3==3& PlayerRisk_q32==1
replace riskq3=3 if PlayerRisk_q3==1& PlayerRisk_q32==4 | PlayerRisk_q3==4& PlayerRisk_q32==1
replace riskq3=4 if PlayerRisk_q3==2& PlayerRisk_q32==2 
replace riskq3=4 if PlayerRisk_q3==2& PlayerRisk_q32==4 | PlayerRisk_q3==4& PlayerRisk_q32==2
replace riskq3=4 if PlayerRisk_q3==3& PlayerRisk_q32==3 
replace riskq3=4 if PlayerRisk_q3==3& PlayerRisk_q32==4 | PlayerRisk_q3==4& PlayerRisk_q32==3
replace riskq3=4 if PlayerRisk_q3==4& PlayerRisk_q32==4 

la define riskq3 1 "Both reflected" 2 "One takes initiative" 3 "Either thinks both reflected" 4 "The rest"
la val riskq3 riskq3*/


egen uloss_col=rowmean(uloss uloss2)
g imbalance_p=abs(uloss_ind-uloss_ind2)
/*g imbalance_np=abs(ccei_dif-ccei_dif2) */
g t=1
save Risk_ByPair_post.dta, replace

*-------------------------------------------------------------------------------
*                  For numerical search of Pareto improvement
*------------------------------------------------------------------------------;

keep if uiw>uipec | (uipec<uic & pe==0)
keep if pe==0 //310 cases
cap drop mover partner game_type ccei_ind varian_ind ccei_col varian_col ccei_com point pointed mutual distance Participantlabel PlayerRisk_q1 PlayerRisk_q2 PlayerRisk_q3 sex id_max riskpref lnrelprice relconsmp rdu rdu_rr prweighting prweighting_rr mathscore outgoing openness agreeable conscientious stable school coed
keep id_new round_number alpha_ind rho_ind coord* intercept* x_w2
save ParetoImprove_Numerical_Post.dta, replace
export excel using "C:\Users\master\Dropbox\RP\Estimation\1.RP_CARA\data_PE_numerical_Post.xls", firstrow(variables) replace


*-------------------------------------------------------------------------------
*                    Relative Price and Consumption Graph
*------------------------------------------------------------------------------;

use Risk_merged_post.dta, clear 
egen temp22=tag(id_max)
sort temp22 id_max
egen temp3=rank(id_max) if temp22==1
gsort id_max -temp22
bysort id_max : replace temp3=temp3[1]

form ccei %9.2f
form riskpref %9.2f

quietly: su temp3
local M= r(max)
forvalues i=1/`M' {
quietly: su id_new if temp3==`i' & mover==1
local id_mover = r(mean)
quietly: su id_new if temp3==`i' & mover==0
local id_stayer = r(mean)

quietly: su ccei if temp3==`i' & mover==0 & game_type==1
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskpref if temp3==`i' & mover==0 & game_type==1
local RP = trim("`: display %9.2f r(mean)'") 

twoway (scatter relconsmp lnrelprice if temp3==`i'& mover==0 & game_type==1, msize(large) msymbol(circle_hollow))  ///
       (scatter relconsmp lnrelprice if temp3==`i'& mover==0 & game_type==2, msize(large) msymbol(+)), ///
	   legend(order(1 "Individual" 2 "Collective")) xlabel(-2(1)2) xtitle("log(p_y/p_x)") ylabel(0(0.2)1) ytitle("y/(x+y)") saving(temp1, replace) /// 
	   note(Individual CCEI(Risk Pref.)=`C'(`RP')) caption(Id=`id_stayer')

quietly: su ccei if temp3==`i' & mover==1 & game_type==1
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskpref if temp3==`i' & mover==1 & game_type==1
local RP = trim("`: display %9.2f r(mean)'") 

twoway (scatter relconsmp lnrelprice if temp3==`i'& mover==1 & game_type==1, msize(large) msymbol(circle_hollow))  ///
       (scatter relconsmp lnrelprice if temp3==`i'& mover==1 & game_type==2, msize(large) msymbol(+)), ///
	    legend(order(1 "Individual" 2 "Collective")) xlabel(-2(1)2) xtitle("log(p_y/p_x)") ylabel(0(0.2)1) ytitle("y/(x+y)") saving(temp2, replace) ///   
	   note(Individual CCEI(Risk Pref.)=`C'(`RP')) caption(Id=`id_mover')
	   
quietly: su ccei if temp3==`i' & id_new== `id_mover' & game_type==2 
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskpref if temp3==`i' & id_new== `id_mover' & game_type==2
local RP = trim("`: display %9.2f r(mean)'") 

gr combine temp1.gph temp2.gph, title(Group ID=`i') ///
subtitle(Collective CCEI(Risk Pref.) = `C'(`RP')) note("Right Individual moved the seat." "Risk Pref=average(Unit of cheaper goods/sum of unit)")
gr export relconsumption`i'.png, replace
}
