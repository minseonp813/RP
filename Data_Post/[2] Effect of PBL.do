********************************************************************************
*                              Individual Rationality 
********************************************************************************
use Risk_ByIndiv_long.dta, clear
drop if temp==1

*Dist. of Individual Rationality
twoway (histogram ccei_ind if t==0&treatment==0, bin(15) percent ylabel(0(20)80)) (histogram ccei_ind if t==1&treatment==0, fcolor(none) percent ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) bin(15) ),  ///
subtitle(Control) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp1, replace)

twoway (histogram ccei_ind if t==0&treatment==1, bin(15) percent ylabel(0(20)80)) (histogram ccei_ind if t==1&treatment==1, fcolor(none) percent ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) bin(15) ),  ///
subtitle(treatment) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp2, replace)

gr combine temp1.gph temp2.gph
gr export ccei_indiv.png, replace

twoway (histogram varian_ind if t==0&treatment==0, bin(15) percent ylabel(0(20)80)) (histogram varian_ind if t==1&treatment==0, fcolor(none) percent ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) bin(15) ), ///
subtitle(Control) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp1, replace)

twoway (histogram varian_ind if t==0&treatment==1, bin(15) percent ylabel(0(20)80)) (histogram varian_ind if t==1&treatment==1, fcolor(none) percent ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) bin(15)),  ///
subtitle(treatment) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp2, replace)

gr combine temp1.gph temp2.gph
gr export varian_indiv.png, replace

*Summary Stats.
log using Rationality, replace
bysort t treatment: su ccei_ind, de
bysort t treatment: su ccei_ind90
bysort t treatment: su ccei_ind95
bysort t treatment: su ccei_ind99
log c

*Regressions 
reg ccei_ind t treatment txtreatment, r cl(class)

qui ivregress 2sls ccei_ind t i.id_new (pbl_cont=txtreatment)
esttab, keep(pbl_cont t)	

g ccei_ind0=ccei_ind if t==0
g pbl_cont0=pbl_cont if t==0
sort id_new t
bysort id_new: replace ccei_ind0=ccei_ind[1]
bysort id_new: replace pbl_cont0=pbl_cont0[1]
ivregress 2sls ccei_ind ccei_ind0 pbl_cont0 (pbl_cont=treatment) if t==1


********************************************************************************
*                         Individual Risk Attitude
********************************************************************************

*Dist. of Individual Risk Attitude
twoway (histogram ccei_ind if t==0&treatment==0, bin(15) percent ylabel(0(20)80)) (histogram ccei_ind if t==1&treatment==0, fcolor(none) percent ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) bin(15) ),  ///
subtitle(Control) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp1, replace)

twoway (histogram ccei_ind if t==0&treatment==1, bin(15) percent ylabel(0(20)80)) (histogram ccei_ind if t==1&treatment==1, fcolor(none) percent ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) bin(15) ),  ///
subtitle(treatment) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp2, replace)

gr combine temp1.gph temp2.gph
gr export riskpref_indiv.png, replace

*Summary Stats.
bysort t treatment: su riskpref_ind, de

*Regressions 
reg riskpref_ind t treatment txtreatment, r cl(class)
reg riskpref_ind t treatment txtreatment ccei_ind, r cl(class)

qui ivregress 2sls riskpref_ind t i.id_new (pbl_cont=txtreatment)
esttab, keep(pbl_cont t)	
qui ivregress 2sls riskpref_ind t ccei_ind i.id_new (pbl_cont=txtreatment)
esttab, keep(pbl_cont t)	

g riskpref_ind0=riskpref_ind if t==0
sort id_new t
bysort id_new: replace riskpref_ind0=riskpref_ind[1]
ivregress 2sls riskpref_ind riskpref_ind0 pbl_cont0  (pbl_cont=treatment) if t==1
ivregress 2sls riskpref_ind riskpref_ind0 pbl_cont0 ccei_ind (pbl_cont=treatment) if t==1















use Risk_ByPair_long.dta, clear
drop if alpha_ind==.
replace school=floor(id_new/10000) if school==.
drop if school==112|school==122|school==212|school==222
drop if temp==1
sort id_new t
foreach var of varlist sexpair_all {
bysort id_new: replace `var'=`var'[1]
}
egen pbl_cont_col=rowmean(pbl_cont pbl_cont2)

********************************************************************************
*                             Collective Rationality 
********************************************************************************

*Dist. of Collective Rationality

twoway (histogram ccei_col if t==0&treatment==0, bin(15) percent  ylabel(0(20)80)) (histogram ccei_col if t==1&treatment==0&temp==2, fcolor(none) ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) bin(15) percent),  xlabel(0(0.2)1) ///
subtitle(Control) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp1, replace)

twoway (histogram ccei_col if t==0&treatment==1, bin(15) percent ylabel(0(20)80)) (histogram ccei_col if t==1&treatment==1&temp==2, fcolor(none) ///
lcolor(forest_green) lwidth(medthick) lpattern(solid)  bin(15) percent),  xlabel(0(0.2)1) ///
subtitle(Treatment) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp2, replace)

gr combine temp1.gph temp2.gph 
gr export ccei_col.png, replace

twoway (histogram varian_col if t==0&treatment==0, bin(15) percent ylabel(0(20)80)) (histogram varian_col if t==1&treatment==0&temp==2, fcolor(none) ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) bin(15) percent),  xlabel(0(0.2)1) ///
subtitle(Control) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp1, replace)

twoway (histogram varian_col if t==0&treatment==1, bin(15) percent ylabel(0(20)80)) (histogram varian_col if t==1&treatment==1&temp==2, fcolor(none) ///
lcolor(forest_green) lwidth(medthick) lpattern(solid)  bin(15) percent),  xlabel(0(0.2)1) ///
subtitle(Treatment) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp2, replace)

gr combine temp1.gph temp2.gph 
gr export varian_col.png, replace

*Summary Stats.
log using Rationality, replace
bysort t treatment: su ccei_col, de
bysort t treatment: su ccei_col90
bysort t treatment: su ccei_col95
bysort t treatment: su ccei_col99
log c

*Regressions (Mean Effect)
reg ccei_col t treatment txtreatment, r cl(class)
reg ccei_col t treatment txtreatment ccei_ind ccei_ind2 i.riskprefpair i.sexpair_all i.scorepair , r cl(class)

qui ivregress 2sls ccei_col t i.id_new (pbl_cont_col=txtreatment)
esttab, keep(pbl_cont_col t)	
qui ivregress 2sls ccei_col t ccei_ind ccei_ind2 i.id_new (pbl_cont_col=txtreatment)
esttab, keep(pbl_cont_col t)

g ccei_col0=ccei_col if t==0
g pbl_cont_col0=pbl_cont_col if t==0
sort id_new t
bysort id_new: replace ccei_col0=ccei_col[1]
bysort id_new: replace pbl_cont_col0=pbl_cont_col0[1]
ivregress 2sls ccei_col ccei_col0 pbl_cont_col0 (pbl_cont_col=treatment) if t==1
ivregress 2sls ccei_col ccei_col0 pbl_cont_col0 ccei_ind ccei_ind2 (pbl_cont_col=treatment) if t==1


********************************************************************************
*                     Collective Rationality - Composition
********************************************************************************

. bysort id_new: egen max=max(ccei_both90)

. bysort id_new: egen min=min(ccei_both90)

. tab max min

           |               minccei_both90
		max|         0          1          2 |     Total
-----------+---------------------------------+----------
         0 |        30          0          0 |        30 
         1 |       100        172          0 |       272 
         2 |        78        356        264 |       698 
-----------+---------------------------------+----------
     Total |       208        528        264 |     1,000 


g eff=cond(min==2&max==2,1,0)

********************************************************************************
*                   Utility Loss from Pareto Inefficient Choices
********************************************************************************

*Dist. of Utility Loss

twoway (histogram uloss_col if t==0&treatment==0&eff==1, percent width(0.05) ylabel(0(10)60)) (histogram uloss_col if t==1&treatment==0&eff==1, fcolor(none) ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) width(0.05) percent),  xlabel(0(0.2)1) ///
subtitle(Control) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp1, replace)

twoway (histogram uloss_col if t==0&treatment==1&eff==1,  percent width(0.05) ylabel(0(10)60)) (histogram uloss_col if t==1&treatment==1&eff==1, fcolor(none) ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) width(0.05)  percent),  xlabel(0(0.2)1) ///
subtitle(Treatment) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp2, replace)

gr combine temp1.gph temp2.gph 
gr export uloss_col.png, replace

*Summary Stats.
log using temp, replace
bysort t treatment: su uloss_col if eff==1, de
bysort t treatment: su imbalance_p if eff==1, de
log c

*Regressions (Mean Effect)
reg uloss_col t treatment txtreatment if eff==1, r cl(class)
reg uloss_col t treatment txtreatment ccei_ind ccei_ind2 i.riskprefpair i.sexpair_all i.scorepair if eff==1, r cl(class)

qui ivregress 2sls uloss_col t i.id_new (pbl_cont_col=txtreatment)
esttab, keep(pbl_cont_col t)	
qui ivregress 2sls uloss_col t uloss_ind uloss_ind2 i.id_new (pbl_cont_col=txtreatment)
esttab, keep(pbl_cont_col t)

g uloss_col0=uloss_col if t==0
sort id_new t
bysort id_new: replace uloss_col0=uloss_col0[1]
ivregress 2sls uloss_col uloss_col0 pbl_cont_col0 riskprefpair (pbl_cont_col=treatment) if t==1&eff==1
ivregress 2sls uloss_col uloss_col0 pbl_cont_col0 ccei_ind ccei_ind2 ccei_col riskprefpair (pbl_cont_col=treatment) if t==1&eff==1


********************************************************************************
*                           Imbalance within Pair
********************************************************************************

twoway (histogram imbalance_p if t==0&treatment==0&eff==1, percent width(0.05) ylabel(0(10)60)) (histogram imbalance_p if t==1&treatment==0&eff==1, fcolor(none) ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) width(0.05) percent),  xlabel(0(0.2)1) ///
subtitle(Control) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp1, replace)

twoway (histogram imbalance_p if t==0&treatment==1&eff==1,  percent width(0.05) ylabel(0(10)60)) (histogram imbalance_p if t==1&treatment==1&eff==1, fcolor(none) ///
lcolor(forest_green) lwidth(medthick) lpattern(solid) width(0.05)  percent),  xlabel(0(0.2)1) ///
subtitle(Treatment) legend(order(1 "Pre-treatment" 2 "Post-treatment")) saving(temp2, replace)

gr combine temp1.gph temp2.gph 
gr export imbalance.png, replace
