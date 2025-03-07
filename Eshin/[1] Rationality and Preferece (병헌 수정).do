
*-------------------------------------------------------------------------------
*                      Rationality - 1. Collective Consistency
*------------------------------------------------------------------------------;

use Risk_ByIndiv, clear
*CCEI histogram and scatter plot_aggregate data
twoway (histogram ccei_ind, percent) (histogram ccei_col, percent fcolor(none) lcolor(forest_green) lwidth(medthick) lpattern(solid)), ///
ytitle(Fraction of subjects(%)) ylabel(0(5)60) xtitle(CCEI) xlabel(#20) legend(order(1 "Individual" 2 "Collective"))
gr export Figure3.png, replace
twoway (scatter ccei_col ccei_ind) (scatteri 1 1 0 0, recast(line) lcolor(eltblue) lwidth(medthick)), ytitle(ccei_collective) xtitle(ccei_Individual) title(Collective and Individual ccei) subtitle((Corr.=0.246)) note(The solid line is 45 degree line.) legend(off)
gr export ccei_scatter.png, replace

twoway (histogram varian_ind, percent) (histogram varian_col, percent fcolor(none) lcolor(forest_green) lwidth(medthick) lpattern(solid)), ///
ytitle(Fraction of subjects(%)) ylabel(0(5)60) xtitle(varian) xlabel(#20) legend(order(1 "Individual" 2 "Collective"))
gr export Figure3_Varian.png, replace
twoway (scatter varian_col varian_ind) (scatteri 1 1 0 0, recast(line) lcolor(eltblue) lwidth(medthick)), ytitle(varian_collective) xtitle(varian_Individual) title(Collective and Individual varian) subtitle((Corr.=0.246)) note(The solid line is 45 degree line.) legend(off)
gr export varian_scatter.png, replace


*************************************************
use Risk_ByPair, clear
*Correlation between two individual ccei
twoway (scatter ccei_ind ccei_ind2) (scatteri 1 1 0 0, recast(line) lcolor(eltblue) lwidth(medthick)), ytitle(ccei_Partner) xtitle(ccei_Individual) title(Correlation between Partner's CCEI) subtitle((Corr.=0.0757)) note(The solid line is 45 degree line.) legend(off)
gr export ccei_scatter2.png, replace */

*Difference of ccei_histogram 
g dif_ccei=abs(ccei_ind-ccei_ind2)
su dif_ccei
local M=round(r(mean),0.001)
local SD=round(r(sd),0.001)
histogram dif_ccei, xtitle(|Member 1's CCEI - Member 2's CCEI|)  ///
          note(Mean(SD) of Difference=`M'(`SD'))
gr export cceidif.png, replace

su dif_ccei if ccei_ind~=1 | ccei_ind2~=1
local M=round(r(mean),0.001)
local SD=round(r(sd),0.001)
histogram dif_ccei if ccei_ind~=1 | ccei_ind2~=1 , xtitle(|Member 1's CCEI - Member 2's CCEI|) ylabel(0(2)10) title(Distribution of Difference of CCEI between Partners) ///
          subtitle(Excluding Pairs Both Members have CCEI=1) note(Mean(SD) of Difference=`M'(`SD'))
gr export cceidif2.png, replace 
*/

*Difference of ccei and collective ccei
egen ccei_lo= rowmin(ccei_ind ccei_ind2)
egen ccei_hi= rowmax(ccei_ind ccei_ind2)

corr ccei_col dif_ccei 
local r=round(r(rho),0.001)
twoway (scatter  ccei_col dif_ccei), ytitle(ccei_Collective) xtitle(|Member 1's CCEI - Member 2's CCEI|) title(Difference in CCEI and Collective Rationality) ///
        subtitle(Corr=`r')
gr export cceidif_scatter.png, replace

reg ccei_col dif_ccei
outreg2 using reg.xls, cdec(3) bdec(3)
reg ccei_col dif_ccei ccei_lo
outreg2 using reg.xls, append cdec(3) bdec(3)

*-------------------------------------------------------------------------------
*                      Rationality - 2. Extension
*------------------------------------------------------------------------------;

*Rationality yields rationality?
ksmirnov ccei_col if ccei_both99~=2, by(ccei_both99)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov ccei_col if ccei_both99~=0, by(ccei_both99)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot ccei_col, by(ccei_both99) opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both's CCEIs<0.99 (N=319)" 2 "Only One's CCEI>=0.99 (N=351)" 3 "Both's CCEIs>=0.99 (N=116)") col(1)) ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Both's CCEIs<0.99)=(Only One's CCEI>=0.99) `d1'(`p1')"' ///
`" 2. (Only One's CCEI>=0.99)=(Both's CCEIs>=0.99) `d2'(`p2')"') ///
xlabel(0(0.05)1) xtitle(Collective CCEI)
graph export "Figure1_CCEI_99.png", as(png) replace

ksmirnov ccei_col if ccei_both95~=2, by(ccei_both95)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov ccei_col if ccei_both95~=0, by(ccei_both95)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot ccei_col, by(ccei_both95) opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both's CCEIs<0.95 (N=199)" 2 "Only One's CCEI>=0.95 (N=376)" 3 "Both's CCEIs>=0.95 (N=211)") col(1)) ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Both's CCEIs<0.95)=(Only One's CCEI>=0.95) `d1'(`p1')"' ///
`" 2. (Only One's CCEI>=0.95)=(Both's CCEIs>=0.95) `d2'(`p2')"') ///
xlabel(0(0.05)1) xtitle(Collective CCEI)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure1_CCEI_95.png", as(png) replace

ksmirnov ccei_col if ccei_both90~=2, by(ccei_both90)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov ccei_col if ccei_both90~=0, by(ccei_both90)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot ccei_col, by(ccei_both90) opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both's CCEIs<0.9 (N=121)" 2 "Only One's CCEI>=0.9 (N=345)" 3 "Both's CCEIs>=0.9 (N=320)") col(1)) ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Both's CCEIs<0.9)=(Only One's CCEI>=0.9) `d1'(`p1')"' ///
`" 2. (Only One's CCEI>=0.9)=(Both's CCEIs>=0.9) `d2'(`p2')"') ///
xlabel(0(0.05)1) xtitle(Collective CCEI)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure1_CCEI_90.png", as(png) replace


ksmirnov varian_col if varian_both99~=2, by(varian_both99)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov varian_col if varian_both99~=0, by(varian_both99)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot varian_col, by(varian_both99) opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both's Varians<0.99 (N=370)" 2 "Only One's Varian>=0.99 (N=328)" 3 "Both's Varians>=0.99 (N=88)") col(1)) ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Both's Varians<0.99)=(Only One's Varian>=0.99) `d1'(`p1')"' ///
`" 2. (Only One's Varian>=0.99)=(Both's varians>=0.99) `d2'(`p2')"') ///
xlabel(0(0.05)1) xtitle(Collective Varian)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure1_Varian_99.png", as(png) replace

ksmirnov varian_col if varian_both95~=2, by(varian_both95)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov varian_col if varian_both95~=0, by(varian_both95)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot varian_col, by(varian_both95) opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both's Varians<0.95 (N=370)" 2 "Only One's Varian>=0.95 (N=328)" 3 "Both's Varians>=0.95 (N=88)") col(1)) ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Both's Varians<0.95)=(Only One's Varian>=0.95) `d1'(`p1')"' ///
`" 2. (Only One's Varian>=0.95)=(Both's varians>=0.95) `d2'(`p2')"') ///
xlabel(0(0.05)1) xtitle(Collective Varian)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure1_Varian_95.png", as(png) replace


ksmirnov varian_col if varian_both90~=2, by(varian_both90)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov varian_col if varian_both90~=0, by(varian_both90)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot varian_col, by(varian_both90) opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both's Varians<0.90 (N=370)" 2 "Only One's Varian>=0.90 (N=328)" 3 "Both's Varians>=0.90 (N=88)") col(1)) ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Both's Varians<0.90)=(Only One's Varian>=0.90) `d1'(`p1')"' ///
`" 2. (Only One's Varian>=0.90)=(Both's varians>=0.90) `d2'(`p2')"') ///
xlabel(0(0.05)1) xtitle(Collective Varian)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure1_Varian_90.png", as(png) replace

*Rationality makes one a leader?
sort ccei_col
g group= _n
 

g temp=runiform()
twoway (rspike ccei_hi ccei_lo group if temp<0.25, lwidth(vvvthin)) ///
 (scatter ccei_col group if temp<0.25, mcolor(navy) msize(medsmall) msymbol(smx)) ///
 (scatter ccei_hi group if temp<0.25, mcolor(dknavy) msize(small)) ///
 (scatter ccei_lo group if temp<0.25, mcolor(dkgreen) msize(small) msymbol(smtriangle)), ///
 ytitle(CCEI_Individual and Collective) xtitle(Rank of each pair by collective CCEI) ///
 title(Range of Each Member's CCEI within pair and Pair's CCEI) ///
 note(Graph is drawn only with randomly selected 25% of total observaion) ///
 legend(order(2 "Collective CCEI" 3 "Higher CCEI" 4 "Lower CCEI") cols(1))
gr export ccei_range.png, replace

gen dif_lo= ccei_lo-ccei_col
gen dif_hi= ccei_hi-ccei_col
twoway (rspike dif_hi dif_lo group if temp<0.25, lwidth(vvvthin)) ///
(scatter dif_hi group if temp<0.25, mcolor(dknavy) msize(small)) ///
(scatter dif_lo group if temp<0.25, mcolor(dkgreen) msize(small) msymbol(smtriangle)), ///
ytitle(Individual CCEI-Collective CCEI) yline(0) xtitle(Rank of each pair by collective CCEI) ///
 subtitle(Collective CCEI is normalized to 0) ///
 note(Graph is drawn only with randomly selected 25% of total observaion) ///
 legend(order( 2 "Higher CCEI - Collective CCEI" 3 "Lower CCEI - Collective CCEI") cols(1))
gr export ccei_range1.png, replace


g cceiaggre=.
recode cceiaggre (.=4) if ccei_col>=ccei_lo & ccei_col<=ccei_hi & abs(ccei_hi-ccei_col)<abs(ccei_lo-ccei_col)
recode cceiaggre (.=2) if ccei_col>=ccei_lo & ccei_col<=ccei_hi & abs(ccei_hi-ccei_col)>abs(ccei_lo-ccei_col)
recode cceiaggre (.=5) if ccei_col>ccei_hi
recode cceiaggre (.=1) if ccei_col<ccei_lo 
replace cceiaggre = 3 if ccei_lo ==ccei_hi == ccei_col

la de cceiaggre 4 "Rational Leader" 2 "Irrationality Contaigion" 5 "Synerge" 1 "Heading Inefficiency" 3 "Homogeneous"
la val cceiaggre cceiaggre 
tab cceiaggre

/*
 
                      |     rat_col
            cceiaggre |         0          1 |     Total
----------------------+----------------------+----------
 Heading Inefficiency |       195          0 |       195 
                      |     40.04       0.00 |     24.81 
----------------------+----------------------+----------
Irrationality Contaig |        64          0 |        64 
                      |     13.14       0.00 |      8.14 
----------------------+----------------------+----------
          Homogeneous |         0         45 |        45 
                      |      0.00      15.05 |      5.73 
----------------------+----------------------+----------
      Rational Leader |       125        145 |       270 
                      |     25.67      48.49 |     34.35 
----------------------+----------------------+----------
              Synerge |       103        109 |       212 
                      |     21.15      36.45 |     26.97 
----------------------+----------------------+----------
                Total |       487        299 |       786 
                      |    100.00     100.00 |    100.00 */


*-------------------------------------------------------------------------------
*             Regression Tables for Collective Rationality 
*------------------------------------------------------------------------------;

reg varian_col varian_ind_max varian_ind_dist i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColVarian.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace

reg varian_col varian_ind_max varian_ind_dist riskaversion_ind_max riskaversion_ind_dist i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColVarian.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg varian_col varian_ind_max varian_ind_dist ///
ib(1).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColVarian.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg varian_col varian_ind_max varian_ind_dist riskaversion_ind_max riskaversion_ind_dist ///
ib(1).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColVarian.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg varian_col varian_ind_max varian_max_nonmover varian_ind_dist riskaversion_ind_max riskaversion_ind_dist ///
ib(1).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColVarian.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

**# Bookmark #1
use Risk_ByPair, clear

*** 중요! Table 2 Model 1****
reg ccei_col ccei_ind_max ccei_ind_dist i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColCCEI.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace

reg ccei_col ccei_ind_max ccei_ind_dist riskaversion_ind_max riskaversion_ind_dist i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColCCEI.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg ccei_col ccei_ind_max ccei_ind_dist ///
ib(1).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColCCEI.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

**# Bookmark #2

*** 중요! Table2. Model 2 ***
reg ccei_col ccei_ind_max ccei_ind_dist riskaversion_ind_max riskaversion_ind_dist ///
ib(1).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "Table1_ColCCEI.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES, Male Pair, Yes, Height, Yes, Friendship, Yes) replace

* 결측치 확인. 왜 OBS 가 적게 나오지?
sum ccei_col
sum mathscore_max
sum mathscore_dist
sum height_gr_max
sum height_gr_dist

reg ccei_col ccei_ind_max ccei_max_nonmover ccei_ind_dist riskaversion_ind_max riskaversion_ind_dist ///
ib(1).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColCCEI.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

					  
*-------------------------------------------------------------------------------
*                    Risk Preference- 1. Structure: Non parametric
*------------------------------------------------------------------------------;

use Risk_ByIndiv, clear 
putexcel set "Summary.xlsx", sheet("NPRP") replace
putexcel A1=("Variable") B1=("N") C1=("Mean") D1=("SD") E1=("p50") F1=("p75") G1=("p95")
local row=2
   qui su riskaversion_ind, de
putexcel A`row' = ("RP_Ind")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))

local ++row

foreach var of varlist ccei_ind99 ccei_ind95 ccei_ind90 {
   qui su riskaversion_ind if `var'==2, de
putexcel A`row' = ("RP_Ind")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))
local ++row
}

   qui su riskaversion_col if mover==1, de
putexcel A`row' = ("RP_Col")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))

local ++row

foreach var of varlist ccei_col99 ccei_col95 ccei_col90 {
local varlabel : var label `var'
   qui su riskaversion_col if `var'==1 &  mover==1, de
putexcel A`row' = ("RP_Col")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))
local ++row
}


twoway (histogram riskaversion_ind) (histogram riskaversion_col, fcolor(none) lcolor(forest_green) lwidth(medthick) lpattern(solid)), ylabel(#5) legend(order(1 "Risk Pref._Individual" 2 "Risk Pref._Collective")) note(Risk Pref. := Individual average of (Coordination of cheaper goods / Sum of coordination))
gr export riskaversion_hist.png, replace
twoway (scatter riskaversion_col riskaversion_ind) (scatteri 1 1 0 0, recast(line) lcolor(eltblue) lwidth(medthick)), ytitle(riskaversion_collective) xtitle(riskaversion_Individual) title(Collective and Individual riskaversion) subtitle((Corr.=0.419)) note(The solid line is 45 degree line.) legend(off)
gr export riskaversion_scatter.png, replace

use Risk_ByPair, clear
twoway (scatter riskaversion_ind riskaversion_ind2) (scatteri 1 1 0 0, recast(line) lcolor(eltblue) lwidth(medthick)), ytitle(riskaversion_Partner) xtitle(riskaversion_Individual) title(Correlation between Partner's Risk Preference) subtitle((Corr.=0.0685)) note(The solid line is 45 degree line.) legend(off)
gr export riskaversion_scatter2.png, replace

g dif_riskaversion=abs(riskaversion_ind-riskaversion_ind2)
su dif_riskaversion
local M=round(r(mean),0.001)
local SD=round(r(sd),0.001)
histogram dif_riskaversion, xtitle(|Member 1's riskaversion - Member 2's riskaversion|) ///
          ylabel(0(2)10) title(Distribution of Difference of riskaversion between Partners) ///
		  note(Mean(SD) of Difference=`M'(`SD'))
gr export riskaversiondif.png, replace

corr riskaversion_col dif_riskaversion 
local r=round(r(rho),0.001)
twoway (scatter  riskaversion_col dif_riskaversion ), ytitle(riskaversion_Collective) xtitle(|Member 1's riskaversion - Member 2's riskaversion|) title(Difference in riskaversion and Collective Risk Preference) ///
       subtitle(Corr=`r')
gr export riskaversiondif_scatter.png, replace

corr ccei_col dif_riskaversion if ccei_ind>0.95 & ccei_ind2>0.95
local r=round(r(rho),0.001)
twoway (scatter  ccei_col dif_riskaversion if ccei_ind>0.95 & ccei_ind2>0.95 ), ytitle(ccei_Collective) xtitle(|Member 1's riskaversion - Member 2's riskaversion|) title(Difference in riskaversion and Collective Rationality) note(Only pairs both member's CCEI is over 0.95 are included.) ///
       subtitle(Corr=`r')
gr export riskaversiondif_scatter2.png, replace

ksmirnov riskaversion_col if riskaversionpair~=2, by(riskaversionpair)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov riskaversion_col if riskaversionpair~=0, by(riskaversionpair)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot riskaversion_col, by(riskaversionpair) xlabel(0(0.05)1) ///
opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both Less Risk Averse Pairs (N=218)" 2 "Heterogeneous Pairs (N=350)" ///
3 "Both Risk Averse Pairs (N=218)" ) col(1))  ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Heterogeneous=Both Less Risk Averse) `d1'(`p1')"' ///
`"2. (Heterogeneous=Both Risk Averse) `d2'(`p2')"' `"Risk Averse=(Individual Risk Aversion >= the Median) "') ///
xtitle(Collective Non-parametric Risk Aversion)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure2_riskaversion.png", as(png) replace

foreach t in 99 95 90 {
ksmirnov riskaversion_col if riskaversionpair~=2&varian_both`t'==2, by(riskaversionpair)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov riskaversion_col if riskaversionpair~=0&varian_both`t'==2, by(riskaversionpair)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot riskaversion_col if varian_both`t'==2, by(riskaversionpair) xlabel(0(0.05)1) ///
opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both Less Risk Averse Pairs" 2 "Heterogeneous Pairs" ///
3 "Both Risk Averse Pairs" ) col(1))  ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Heterogeneous=Both Less Risk Averse) `d1'(`p1')"' ///
`"2. (Heterogeneous=Both Risk Averse) `d2'(`p2')"' `"Risk Averse=(Individual Risk Aversion >= the Median) "') ///
xtitle(Collective Non-parametric Risk Aversion)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure2_riskaversion_varian`t'.png", as(png) replace
}

sort riskaversion_col
g grouprp=_n
egen riskaversion_lo=rowmin(riskaversion_ind riskaversion_ind2)
egen riskaversion_hi=rowmax(riskaversion_ind riskaversion_ind2)

twoway (rspike riskaversion_hi riskaversion_lo grouprp if ccei_both90==2, lwidth(vvvthin)) ///
 (scatter riskaversion_col grouprp if ccei_both90==2, mcolor(navy) msize(medsmall) msymbol(smx)) ///
 (scatter riskaversion_hi grouprp if ccei_both90==2, mcolor(dknavy) msize(small)) ///
 (scatter riskaversion_lo grouprp if ccei_both90==2, mcolor(dkgreen) msize(small) msymbol(smtriangle)), ///
 ytitle(riskaversion_Individual and Collective) xtitle(Rank of each pair by collective riskaversion) ///
 title(Range of Each Member's riskaversion within pair and Pair's riskaversion) ///
 note(Graph is drawn only with 2 rational-individuals(CCEI>=0.90) pairs) ///
 legend(order(2 "Collective riskaversion" 3 "Higher riskaversion" 4 "Lower riskaversion") cols(1))
gr export riskaversion_range_rational.png, replace

g temp=runiform()
twoway (rspike riskaversion_hi riskaversion_lo group if temp<0.25, lwidth(vvvthin)) ///
 (scatter riskaversion_col group if temp<0.25, mcolor(navy) msize(medsmall) msymbol(smx)) ///
 (scatter riskaversion_hi group if temp<0.25, mcolor(dknavy) msize(small)) ///
 (scatter riskaversion_lo group if temp<0.25, mcolor(dkgreen) msize(small) msymbol(smtriangle)), ///
 ytitle(riskaversion_Individual and Collective) xtitle(Rank of each pair by collective riskaversion) ///
 title(Range of Each Member's riskaversion within pair and Pair's riskaversion) ///
 note(Graph is drawn only with randomly selected 25% of total observaion) ///
 legend(order(2 "Collective riskaversion" 3 "Higher riskaversion" 4 "Lower riskaversion") cols(1))
gr export riskaversion_range.png, replace

gen dif_lo= riskaversion_lo-riskaversion_col
gen dif_hi= riskaversion_hi-riskaversion_col
twoway (rspike dif_hi dif_lo group if temp<0.25, lwidth(vvvthin)) ///
(scatter dif_hi group if temp<0.25, mcolor(dknavy) msize(small)) ///
(scatter dif_lo group if temp<0.25, mcolor(dkgreen) msize(small) msymbol(smtriangle)), ///
ytitle(Individual riskaversion-Collective riskaversion) yline(0) xtitle(Rank of each pair by collective riskaversion) ///
 subtitle(Collective riskaversion is normalized to 0) ///
 note(Graph is drawn only with randomly selected 25% of total observaion) ///
 legend(order( 2 "Higher riskaversion - Collective riskaversion" 3 "Lower riskaversion - Collective riskaversion") cols(1))
gr export riskaversion_range1.png, replace

g riskaversionaggre=.
recode riskaversionaggre (.=4) if riskaversion_col>=riskaversion_lo & riskaversion_col<=riskaversion_hi & abs(riskaversion_hi-riskaversion_col)<abs(riskaversion_lo-riskaversion_col)
recode riskaversionaggre (.=2) if riskaversion_col>=riskaversion_lo & riskaversion_col<=riskaversion_hi & abs(riskaversion_hi-riskaversion_col)>abs(riskaversion_lo-riskaversion_col)
recode riskaversionaggre (.=5) if riskaversion_col>riskaversion_hi
recode riskaversionaggre (.=1) if riskaversion_col<riskaversion_lo 
replace riskaversionaggre = 3 if riskaversion_lo ==riskaversion_hi == riskaversion_col

la de riskaversionaggre 4 "Risk-neutral Leader" 2 "Risk-averse Leader" 5 "Heading Risk-neutrality" 1 "Heading Risk-aversion" 3 "Homogeneous"
la val riskaversionaggre riskaversionaggre 
tab riskaversionaggre
tab riskaversionaggre if ccei_ind>=0.95&ccei_ind2>=0.95
restore 


*-------------------------------------------------------------------------------
*                    Risk Preference- 1. Structure: Parametric
*------------------------------------------------------------------------------;
use Risk_ByIndiv.dta, clear

putexcel set "Summary.xlsx", sheet("Alpha") modify
putexcel A1=("Variable") B1=("N") C1=("Mean") D1=("SD") E1=("p50") F1=("p75") G1=("p95")
local row=2
   qui su alpha_ind, de
putexcel A`row' = ("Alpha_Ind")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))

local ++row

foreach var of varlist ccei_ind99 ccei_ind95 ccei_ind90 {
   qui su alpha_ind if `var'==1, de
putexcel A`row' = ("Alpha_Ind")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))
local ++row
}

   qui su alpha_col if mover==1, de
putexcel A`row' = ("Alpha_Col")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))

local ++row

foreach var of varlist ccei_col99 ccei_col95 ccei_col90 {
local varlabel : var label `var'
   qui su alpha_col if `var'==1 &  mover==1, de
putexcel A`row' = ("Alpha_Col")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))
local ++row
}

putexcel set "Summary.xlsx", sheet("Alpha") modify
local row=13
   qui su alpha_rr_ind, de
putexcel A`row' = ("Alpha_rr_Ind")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))

local ++row

foreach var of varlist ccei_ind99 ccei_ind95 ccei_ind90 {
   qui su alpha_rr_ind if `var'==1, de
putexcel A`row' = ("Alpha_rr_Ind")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))
local ++row
}

   qui su alpha_rr_col if mover==1, de
putexcel A`row' = ("Alpha_rr_Col")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))

local ++row

foreach var of varlist ccei_col99 ccei_col95 ccei_col90 {
local varlabel : var label `var'
   qui su alpha_rr_col if `var'==1 &  mover==1, de
putexcel A`row' = ("Alpha_rr_Col")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))
local ++row
}



putexcel set "Summary.xlsx", sheet("RiskPremium") modify
putexcel A1=("Variable") B1=("N") C1=("Mean") D1=("SD") E1=("p50") F1=("p75") G1=("p95")
local row=2
   qui su riskpremium_ind, de
putexcel A`row' = ("RiskPremium_Ind")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))

local ++row

foreach var of varlist ccei_ind99 ccei_ind95 ccei_ind90 {
   qui su riskpremium_ind if `var'==1, de
putexcel A`row' = ("RiskPremium_Ind")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))
local ++row
}

   qui su riskpremium_col if mover==1, de
putexcel A`row' = ("RiskPremium_Col")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))

local ++row

foreach var of varlist ccei_col99 ccei_col95 ccei_col90 {
local varlabel : var label `var'
   qui su riskpremium_col if `var'==1 &  mover==1, de
putexcel A`row' = ("RiskPremium_Col")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))
local ++row
}

local row=13
   qui su riskpremium_rr_ind, de
putexcel A`row' = ("RiskPremium_rr_Ind")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))

local ++row

foreach var of varlist ccei_ind99 ccei_ind95 ccei_ind90 {
   qui su riskpremium_rr_ind if `var'==1, de
putexcel A`row' = ("RiskPremium_rr_Ind")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))
local ++row
}

   qui su riskpremium_rr_col if mover==1, de
putexcel A`row' = ("RiskPremium_rr_Col")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))

local ++row

foreach var of varlist ccei_col99 ccei_col95 ccei_col90 {
local varlabel : var label `var'
   qui su riskpremium_rr_col if `var'==1 &  mover==1, de
putexcel A`row' = ("RiskPremium_rr_Col")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))
local ++row
}


twoway (histogram alpha_ind) (histogram alpha_col, fcolor(none) lcolor(forest_green) lwidth(medthick) lpattern(solid)), subtitle(CARA Utility fucntion) ylabel(#5) legend(order(1 "alpha_Individual" 2 "alpha_Collective")) xlabel(0(0.1)1) ylabel(0(2)8) saving(temp1, replace)
twoway (histogram alpha_rr_ind) (histogram alpha_rr_col, fcolor(none) lcolor(forest_green) lwidth(medthick) lpattern(solid)), subtitle(CRRA Utility fucntion) ylabel(#5) legend(order(1 "alpha_Individual" 2 "alpha_Collective"))  xlabel(0(0.1)1) ylabel(0(2)8) saving(temp2, replace)
gr combine temp1.gph temp2.gph
gr export hist_alpha.png, replace

twoway (histogram alpha_ind if ccei_ind>=0.95) (histogram alpha_col if ccei_col>=0.95, fcolor(none) lcolor(forest_green) lwidth(medthick) lpattern(solid)), subtitle(CARA Utility fucntion) ylabel(#5) legend(order(1 "alpha_Individual" 2 "alpha_Collective")) xlabel(0(0.1)1) ylabel(0(2)8) saving(temp1, replace)
twoway (histogram alpha_rr_ind if ccei_ind>=0.95) (histogram alpha_rr_col if ccei_col>=0.95, fcolor(none) lcolor(forest_green) lwidth(medthick) lpattern(solid)), subtitle(CRRA Utility fucntion) ylabel(#5) legend(order(1 "alpha_Individual" 2 "alpha_Collective"))  xlabel(0(0.1)1) ylabel(0(2)8) saving(temp2, replace)
gr combine temp1.gph temp2.gph, note(`"Only rational(CCEI>=0.95) individuals and paris are included"')
gr export hist_alpha_rat.png, replace

twoway (histogram riskpremium_ind) (histogram riskpremium_col, fcolor(none) lcolor(forest_green) lwidth(medthick) lpattern(solid)), subtitle(CARA Utility fucntion)ylabel(#5) legend(order(1 "riskpremium_Individual" 2 "riskpremium_Collective") cols(1)) xlabel(0(1)6) ylabel(0(0.5)2) saving(temp1, replace)
twoway (histogram riskpremium_rr_ind) (histogram riskpremium_rr_col, fcolor(none) lcolor(forest_green) lwidth(medthick) lpattern(solid)), subtitle(CRRA Utility fucntion) ylabel(#5) legend(order(1 "riskpremium_Individual" 2 "riskpremium_Collective") cols(1)) xlabel(0(1)6) ylabel(0(0.5)2) saving(temp2, replace)
gr combine temp1.gph temp2.gph
gr export riskpremium.png, replace

g alpha_cut = cond(alpha_ind<0.05,1,0)
g alpha_col_cut=cond(alpha_col<0.05,1,0)

tab rat_ind alpha_cut,row
tab almstrat_ind alpha_cut, row
tab rat_col alpha_col_cut if mover==1, row
tab almstrat_col alpha_col_cut if mover==1, row
tab ccei_col_cut alpha_col_cut if mover==1, row
tab ccei_col_cut2 alpha_col_cut if mover==1, row

use Risk_ByPair.dta, clear

ksmirnov riskpremium_col if riskpremiumpair_90~=2&ccei_both90==2&ccei_col90==1, by(riskpremiumpair_90)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov riskpremium_col if riskpremiumpair_90~=1&ccei_both90==2&ccei_col90==1, by(riskpremiumpair_90)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot riskpremium_col if ccei_both90==2&ccei_col90==1, by(riskpremiumpair_90) xlabel(-1(0.25)2.5) opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Heterogeneous Pairs (N=127)" 3 "Both Risk Averse Pairs (N=71)"  2 "Both Less Risk Averse Pairs (N=61)") col(1))  ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Heterogeneous=Both Risk Averse) `d1'(`p1')"' ///
`"2. (Heterogeneous=Both Less Risk Averse) `d2'(`p2')"' `"Risk Averse%XA1%XD5(Individual Risk Premium>=Average)"') ///
xtitle(Collective Risk Premium)
gr export Figure6.png, replace

ksmirnov riskpremium_rr_col if riskpremium_rrpair_90~=2&ccei_both90==2, by(riskpremium_rrpair_90)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov riskpremium_rr_col if riskpremium_rrpair_90~=1&ccei_both90==2, by(riskpremium_rrpair_90)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot riskpremium_rr_col if ccei_both90==2, by(riskpremium_rrpair_90) xlabel(0(0.2)1) opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Hetero Pairs(N=390)" 2 "Both Risk Averse Pairs(N=198)" 3 "Both Less Risk Averse Pairs(N=198)") col(1))  ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Hetero=Both Risk Averse) `d1'(`p1') 2. (Hetero=Both Less Risk Averse) `d2'(`p2')"' `"Being risk averse is defined as individual non-parametric risk attitude is below the median"') ///
xtitle(Collective Non-parametric Risk Attitude)

foreach t in 90 95 99 {
tab rdupair rdu_col if ccei_both`t'==2&ccei_col`t'==1, row chi2
tab rdu_rrpair rdu_rr_col if ccei_both`t'==2&ccei_col`t'==1, row chi2
}

putexcel set "Summary.xlsx", sheet("RPAggregation") modify
putexcel A1=("Variable") B1=("N") C1=("Mean") D1=("SD") E1=("p50") F1=("p75") G1=("p95")
local row=2
foreach var in riskaversion riskpremium riskpremium_rr{
foreach t in 90 95 99 {
foreach b in 0 1 2 {
su `var'_col if ccei_both`t'==2 & ccei_col`t'==1 & `var'pair==`b', de
putexcel A`row' = ("`var'")
putexcel B`row' = (r(N))
putexcel C`row' = (r(mean))
putexcel D`row' = (r(sd))
putexcel E`row' = (r(p50))
putexcel F`row' = (r(p75))
putexcel G`row' = (r(p95))
local ++row
}
}
}

twoway (rspike riskpremium_hi riskpremium_lo group if ccei_both90==2, lwidth(vvvthin)) ///
 (scatter riskpremium_col group if ccei_both90==2, mcolor(navy) msize(medsmall) msymbol(smx)) ///
 (scatter riskpremium_hi group if ccei_both90==2, mcolor(dknavy) msize(small)) ///
 (scatter riskpremium_lo group if ccei_both90==2, mcolor(dkgreen) msize(small) msymbol(smtriangle)), ///
 ytitle(Riskpremium) xtitle(Rank of each pair by collective riskpremium) ///
 title(Each Member's and Pair's riskpremium(CARA)) ///
 note(Graph is drawn only with 2 rational-individuals(CCEI>=0.90) pairs) ///
 legend(order(2 "Collective riskpremium" 3 "Higher riskpremium" 4 "Lower riskpremium") cols(1))
gr export riskpremium_range_rational.png, replace

gsort -riskpremium_rr_col
g grouprp=_n
twoway (rspike riskpremium_rr_hi riskpremium_rr_lo grouprp if ccei_both90==2, lwidth(vvvthin)) ///
 (scatter riskpremium_rr_col grouprp if ccei_both90==2, mcolor(navy) msize(medsmall) msymbol(smx)) ///
 (scatter riskpremium_rr_hi grouprp if ccei_both90==2, mcolor(dknavy) msize(small)) ///
 (scatter riskpremium_rr_lo grouprp if ccei_both90==2, mcolor(dkgreen) msize(small) msymbol(smtriangle)), ///
 ytitle(riskpremium_rr_Individual and Collective) xtitle(Rank of each pair by collective riskpremium_rr) ///
 title(Each Member's and Pair's riskpremium(CRRA)) ///
 note(Graph is drawn only with 2 rational-individuals(CCEI>=0.90) pairs) ///
 legend(order(2 "Collective riskpremium" 3 "Higher riskpremium" 4 "Lower riskpremium") cols(1))
gr export riskpremium_rr_range_rational.png, replace



