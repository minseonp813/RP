********************************************************************************
*                           Main Figures and Tables
********************************************************************************
use "C:\Users\Minseon Park\Dropbox\RP\Data\Risk_ByPair.dta", clear
set more off


** Figure 1
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

** Table 1
g ccei_ind_max_gr0=cond(ccei_both90==0,ccei_ind_max,0)
g ccei_ind_max_gr1=cond(ccei_both90==1,ccei_ind_max,0)
g ccei_ind_max_gr2=cond(ccei_both90==2,ccei_ind_max,0)

reg ccei_col i.ccei_both90 i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColCCEI0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace

reg ccei_col i.ccei_both90 ccei_ind_max i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColCCEI0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg ccei_col i.ccei_both90 ccei_ind_max i.riskaversionpair i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColCCEI0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg ccei_col i.ccei_both90 ccei_ind_max ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColCCEI0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg ccei_col i.ccei_both90 i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColCCEI0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

** Figure 2A
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
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure2A_riskaversion.png", as(png) replace

** Figure 2B
ksmirnov riskpremium_col if riskpremiumpair~=2 & ccei_both90==2, by(riskpremiumpair)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov riskpremium_col if riskpremiumpair~=0 & ccei_both90==2, by(riskpremiumpair)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot riskpremium_col if ccei_both90==2, by(riskpremiumpair) xlabel(-1(0.2)1.8) ///
opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both Less Risk Averse Pairs (N=99)" 2 "Heterogeneous Pairs (N=136)" ///
3 "Both Risk Averse Pairs (N=85)" ) col(1))  ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Heterogeneous=Both Less Risk Averse) `d1'(`p1')"' ///
`"2. (Heterogeneous=Both Risk Averse) `d2'(`p2')"' `"Risk Averse=(Individual Risk Premium >= the Median) "') ///
xtitle(Collective Risk Premium)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure2B_riskpremium.png", as(png) replace

** Table 2
reg riskaversion_col i.riskaversionpair i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table2_Colriskaversion0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace

reg riskaversion_col i.riskaversionpair i.ccei_both90 i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table2_Colriskaversion0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg riskaversion_col i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table2_Colriskaversion0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg riskaversion_col i.riskaversionpair i.ccei_both90 ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table2_Colriskaversion0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append


** Table 3
putexcel set "C:\Users\Minseon Park\Dropbox\RP\Writing\Table3_Colrisktype0125.xls", sheet("Table3") replace
putexcel  A2=("Both EUT") A3=("One EUT/The other RDU") A4=("Both RDU") A5=("Total")
putexcel  B1=("Collective EUT") C1=("Collective RDU")  D1=("Total")

tab rdupair rdu_col if ccei_both90==2 & ccei_col90==1 , matcell(Tab) 
tab rdupair if ccei_both90==2 & ccei_col90==1, matcell(Tab1)
tab rdu_col if ccei_both90==2 & ccei_col90==1, matcell(Tab2)
matrix TTab2=Tab2'
putexcel B2=matrix(Tab) D2=matrix(Tab1) B5=matrix(TTab2) D5=(r(N))

** Table 4
reg rdu_col i.rdupair i.class if ccei_both90==2 & ccei_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace

reg rdu_col i.rdupair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
if ccei_both90==2 & ccei_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_col i.rdupair rdu_nonmover ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
if ccei_both90==2 & ccei_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

** Figure 3A
twoway (histogram uloss_col, percent bin(25) ) , xlabel(0(0.05)1) ylabel(0(10)40) ///
xtitle(Group Choices' Inefficiency) ytitle(Fraction of subjects(%))
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure3A_ULoss.png", as(png) replace

** Figure 3B
twoway (histogram uloss_col if ccei_both90==2, percent bin(25) ) , xlabel(0(0.05)1) ylabel(0(10)40) ///
note(`"Including only both individuals cceis>=0.9"') xtitle(Group Choices' Inefficiency) ///
ytitle(Fraction of subjects(%))
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure3B_ULoss.png", as(png) replace

** Figure 4A
ksmirnov uloss_col, by(ccei_col90)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")

cdfplot uloss_col, by(ccei_col90) xlabel(0(0.05)1) ///
opt1( lc(teal olive) lp(solid shortdash) ) ///
legend(order(1 "Collective ccei<=0.9" 2 "Collective ccei>0.9") col(1))  ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):`d1'(`p1')"') ///
xtitle(Group Choices' Inefficiency)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure4A_ULoss_ccei.png", as(png) replace

ksmirnov uloss_col if ccei_both90==2, by(ccei_col90)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")

cdfplot uloss_col if ccei_both90==2, by(ccei_col90) xlabel(0(0.05)1) ///
opt1( lc(teal olive) lp(solid shortdash) ) ///
legend(order(1 "Collective ccei<=0.9" 2 "Collective ccei>0.9") col(1))  ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):`d1'(`p1')"') ///
xtitle(Group Choices' Inefficiency)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure4B_ULoss_ccei.png", as(png) replace

** Table 5
reg uloss_col i.ccei_both90 i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace

reg uloss_col i.ccei_both90  ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.riskaversionpair i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ///
i.riskaversionpair i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ///
i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col ccei_col90 i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col ccei_col90  ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col ccei_col90 ///
i.riskaversionpair i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col ccei_col90 ///
i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ccei_col90 i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ccei_col90  ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ccei_col90 ///
i.riskaversionpair i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ccei_col90 ///
i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append


********************************************************************************
*                           Appendix Figures and Tables
********************************************************************************

** Figure 1_Variations
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
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure1_CCEI_99.png", as(png) replace

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

** Table 1_Variations

reg varian_col varian_ind_max varian_ind_dist i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColVarian0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace

reg varian_col varian_ind_max varian_ind_dist i.riskaversionpair i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColVarian0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg varian_col varian_ind_max varian_ind_dist ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColVarian0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg varian_col varian_ind_max varian_ind_dist i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColVarian0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg varian_col varian_ind_max varian_max_nonmover varian_ind_dist i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table1_ColVarian0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

** Figure 2A_Variations
foreach t in 99 95 90 {
ksmirnov riskaversion_col if riskaversionpair~=2&ccei_both`t'==2, by(riskaversionpair)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov riskaversion_col if riskaversionpair~=0&ccei_both`t'==2, by(riskaversionpair)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot riskaversion_col if ccei_both`t'==2, by(riskaversionpair) xlabel(0(0.05)1) ///
opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both Less Risk Averse Pairs" 2 "Heterogeneous Pairs" ///
3 "Both Risk Averse Pairs" ) col(1))  ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Heterogeneous=Both Less Risk Averse) `d1'(`p1')"' ///
`"2. (Heterogeneous=Both Risk Averse) `d2'(`p2')"' `"Risk Averse=(Individual Risk Aversion >= the Median) "') ///
xtitle(Collective Non-parametric Risk Aversion)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure2A_riskaversion_ccei`t'.png", as(png) replace
}

** Figure 2B_Variations
foreach t in 99 95 90 {
ksmirnov riskpremium_col if riskpremiumpair~=2 & ccei_both`t'==2, by(riskpremiumpair)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov riskpremium_col if riskpremiumpair~=0 & ccei_both`t'==2, by(riskpremiumpair)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot riskpremium_col if ccei_both`t'==2, by(riskpremiumpair) xlabel(-1(0.2)1.8) ///
opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both Less Risk Averse Pairs" 2 "Heterogeneous Pairs" ///
3 "Both Risk Averse Pairs" ) col(1))  ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Heterogeneous=Both Less Risk Averse) `d1'(`p1')"' ///
`"2. (Heterogeneous=Both Risk Averse) `d2'(`p2')"' `"Risk Averse=(Individual Risk Premium >= the Median) "') ///
xtitle(Collective Risk Premium)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure2B_riskpremium_ccei`t'.png", as(png) replace
}
foreach t in 99 95 90 {
ksmirnov riskpremium_crra_col if riskpremium_crrapair~=2 & ccei_both`t'==2, by(riskpremium_crrapair)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")
ksmirnov riskpremium_crra_col if riskpremium_crrapair~=0 & ccei_both`t'==2, by(riskpremium_crrapair)
local d2=trim("`: display %9.2f r(D)'")
local p2=trim("`: display %9.2f r(p_cor)'")

cdfplot riskpremium_crra_col if ccei_both`t'==2, by(riskpremium_crrapair) xlabel(-1(0.5)5) ///
opt1( lc(teal olive navy) lp(solid shortdash longdash) ) ///
legend(order(1 "Both Less Risk Averse Pairs" 2 "Heterogeneous Pairs" ///
3 "Both Risk Averse Pairs" ) col(1))  ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):"' `" 1. (Heterogeneous=Both Less Risk Averse) `d1'(`p1')"' ///
`"2. (Heterogeneous=Both Risk Averse) `d2'(`p2')"' `"Risk Averse=(Individual Risk Premium >= the Median) "') ///
xtitle(Collective Risk Premium)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure2B_riskpremium_crra_ccei`t'.png", as(png) replace
}


** Table 2_Variations

reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table2_Colriskpremium0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace

reg riskpremium_col riskpremium_ind_max riskpremium_ind_dist ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table2_Colriskpremium0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg riskpremium_col riskpremium_ind_max riskpremium_max_nonmover riskpremium_ind_dist  ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table2_Colriskpremium0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg riskpremium_crra_col riskpremium_crra_ind_max riskpremium_crra_ind_dist i.class if ccei_both90==2 , r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table2_Colriskpremium0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg riskpremium_crra_col riskpremium_crra_ind_max riskpremium_crra_ind_dist ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table2_Colriskpremium0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg riskpremium_crra_col riskpremium_crra_ind_max riskpremium_crra_max_nonmover riskpremium_crra_ind_dist  ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table2_Colriskpremium0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append


** Table 3_Variantions
putexcel set "C:\Users\Minseon Park\Dropbox\RP\Writing\Table3_Colrisktype0125.xls", sheet("Table3_all") modify
putexcel  A2=("Both EUT") A3=("One EUT/The other RDU") A4=("Both RDU") A5=("Total")
putexcel  B1=("Collective EUT") C1=("Collective RDU")  D1=("Total")

tab rdupair rdu_col, matcell(Tab) 
tab rdupair, matcell(Tab1)
tab rdu_col, matcell(Tab2)
matrix TTab2=Tab2'
putexcel B2=matrix(Tab) D2=matrix(Tab1) B5=matrix(TTab2) D5=(r(N))

putexcel set "C:\Users\Minseon Park\Dropbox\RP\Writing\Table3_Colrisktype0125.xls", sheet("Table3_ccei_crra") modify
putexcel  A2=("Both EUT") A3=("One EUT/The other RDU") A4=("Both RDU") A5=("Total")
putexcel  B1=("Collective EUT") C1=("Collective RDU")  D1=("Total")

tab rdu_crrapair rdu_crra_col if ccei_both90==2 & ccei_col90==1 , matcell(Tab) 
tab rdu_crrapair if ccei_both90==2 & ccei_col90==1, matcell(Tab1)
tab rdu_crra_col if ccei_both90==2 & ccei_col90==1, matcell(Tab2)
matrix TTab2=Tab2'
putexcel B2=matrix(Tab) D2=matrix(Tab1) B5=matrix(TTab2) D5=(r(N))

putexcel set "C:\Users\Minseon Park\Dropbox\RP\Writing\Table3_Colrisktype0125.xls", sheet("Table3_varian90") modify
putexcel  A2=("Both EUT") A3=("One EUT/The other RDU") A4=("Both RDU") A5=("Total")
putexcel  B1=("Collective EUT") C1=("Collective RDU")  D1=("Total")

tab rdupair rdu_col if varian_ind>=0.9 & varian_ind2>=0.9 & varian_col>=0.9, matcell(Tab) 
tab rdupair if varian_ind>=0.9 & varian_ind2>=0.9 & varian_col>=0.9, matcell(Tab1)
tab rdu_col if varian_ind>=0.9 & varian_ind2>=0.9 & varian_col>=0.9, matcell(Tab2)
matrix TTab2=Tab2'
putexcel B2=matrix(Tab) D2=matrix(Tab1) B5=matrix(TTab2) D5=(r(N))

putexcel set "C:\Users\Minseon Park\Dropbox\RP\Writing\Table3_Colrisktype0125.xls", sheet("Table3_varian90_crra") modify
putexcel  A2=("Both EUT") A3=("One EUT/The other RDU") A4=("Both RDU") A5=("Total")
putexcel  B1=("Collective EUT") C1=("Collective RDU")  D1=("Total")

tab rdu_crrapair rdu_crra_col if varian_ind>=0.9 & varian_ind2>=0.9 & varian_col>=0.9, matcell(Tab) 
tab rdu_crrapair if varian_ind>=0.9 & varian_ind2>=0.9 & varian_col>=0.9, matcell(Tab1)
tab rdu_crra_col if varian_ind>=0.9 & varian_ind2>=0.9 & varian_col>=0.9, matcell(Tab2)
matrix TTab2=Tab2'
putexcel B2=matrix(Tab) D2=matrix(Tab1) B5=matrix(TTab2) D5=(r(N))


** Table 4_Variations

reg rdu_col i.rdupair i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace

reg rdu_col i.rdupair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_col i.rdupair rdu_nonmover ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_crra_col i.rdu_crrapair i.class, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_crra_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace

reg rdu_crra_col i.rdu_crrapair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_crra_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_crra_col i.rdu_crrapair rdu_crra_nonmover ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_crra_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_col i.rdupair i.class if ccei_both90==2 & ccei_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_col i.rdupair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
if ccei_both90==2 & ccei_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_col i.rdupair rdu_nonmover ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
if ccei_both90==2 & ccei_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_crra_col i.rdu_crrapair i.class if ccei_both90==2 & ccei_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_crra_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_crra_col i.rdu_crrapair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
if ccei_both90==2 & ccei_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_crra_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_crra_col i.rdu_crrapair rdu_nonmover ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
if ccei_both90==2 & ccei_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_crra_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_col i.rdupair i.class if varian_both90==2 & varian_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_col i.rdupair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
if varian_both90==2 & varian_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_col i.rdupair rdu_nonmover ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
if varian_both90==2 & varian_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_crra_col i.rdu_crrapair i.class if varian_both90==2 & varian_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_crra_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_crra_col i.rdu_crrapair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
if varian_both90==2 & varian_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_crra_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg rdu_crra_col i.rdu_crrapair rdu_crra_nonmover ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class ///
if varian_both90==2 & varian_col90==1, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table4_Colrdu_crra_Appendix0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

** Figure 3B_Variantions
twoway (histogram uloss_col if varian_both90==2, percent bin(25) ) , xlabel(0(0.05)1) ylabel(0(10)40) ///
note(`"Including only both individuals varians>=0.9"') xtitle(Group Choices' Inefficiency) ///
ytitle(Fraction of subjects(%))
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure3B_ULoss_varian90.png", as(png) replace


** Figure 4B_Variations
ksmirnov uloss_col if varian_both90==2, by(varian_col90)
local d1=trim("`: display %9.2f r(D)'")
local p1=trim("`: display %9.2f r(p_cor)'")

cdfplot uloss_col if varian_both90==2, by(varian_col90) xlabel(0(0.05)1) ///
opt1( lc(teal olive) lp(solid shortdash) ) ///
legend(order(1 "Collective varian<=0.9" 2 "Collective varian>0.9") col(1))  ///
note(`"Kolmogorov-Smirnov Distance(corrected p-value):`d1'(`p1')"') ///
xtitle(Group Choices' Inefficiency)
graph export "C:\Users\Minseon Park\Dropbox\RP\Writing\Figures\Figure4B_ULoss_varian90.png", as(png) replace


** Table 5_Variations
reg uloss_col i.ccei_both90 i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) replace

reg uloss_col i.ccei_both90  ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.riskaversionpair i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ///
i.riskaversionpair i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ///
i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col ccei_col i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col ccei_col  ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col ccei_col ///
i.riskaversionpair i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col ccei_col ///
i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ccei_col i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ccei_col  ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ccei_col ///
i.riskaversionpair i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

reg uloss_col i.ccei_both90 ccei_col ///
i.riskaversionpair ///
ib(2).malepair_co mathscore_max mathscore_dist height_gr_max height_gr_dist i.friendship i.class if ccei_both90==2, r cl(class)
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\Table5_Coluloss_Variations0125.xls", ///
bdec(3) cdec(3) drop(i.class) label addtext(Class FE, YES) append

**Summary Stats
set matsize 8000
use Risk_ByIndiv.dta, clear
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\SummaryStat0125.xls", replace ///
sum(detail) keep(varian_ind ccei_ind  ///
riskaversion_ind riskpremium_ind riskpremium_crra_ind rdu_ind ///
male mathscore height_gr)

use Risk_ByPair.dta, clear
outreg2 using "C:\Users\Minseon Park\Dropbox\RP\Writing\SummaryStat0125.xls", append ///
sum(detail) keep( varian_col ccei_col ///
riskaversion_col riskpremium_col riskpremium_crra_col rdu_col ///
uloss_col malepair friendship)
