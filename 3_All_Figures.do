************************************************************************
* WRITTEN BY Byunghun Hahn, 041025
* MAIN ANALYSIS: Figures
************************************************************************

/*********************************************
Table of Contents

  Section 1. Presets & Import Data
  
  Section 2. Replication
  
	*** Figure 1: Not based on data
	(2-1) Figure 2: Replication, same results [Line 86]
	(2-2) Figure 3: Replication, same results [Line 151]
	*** Figure 4: Same with Figure 2
	(2-3) Figure 5: Replication, same results [Line 193]
	*** Figure 6: Not based on data
	(2-4) Figure 7: Replication, 7-a is different [Line 207]
	
*********************************************/


**********************************************
* Section 1. Presets & Import Data
**********************************************


set more off
set matsize 8000

cd "C:\Users\hahn0\Dropbox\RP\Data"
global results_int = "C:\Users\hahn0\Dropbox\RP\Data\Results_Internal"
global results_main = "C:\Users\hahn0\Dropbox\RP\Data\Results_Main"
global results_app = "C:\Users\hahn0\Dropbox\RP\Data\Results_Appendix"

global Ivcut = 0.1 
global Imcut = 0.2
global Iscut = 0.1

global Ivcutpair = "Iv_afriat_ind<=$Ivcut & Iv_afriat_ind2<=$Ivcut"
global Imcutpair = "Im_ind<=$Imcut & Im_ind2<=$Imcut"
global Iscutpair = "Is_ind<=$Iscut & Is_ind2<=$Iscut"

global cov1="mathscore_max mathscore_dist"
global cov2="mathscore_max mathscore_dist i.malepair i.friendship"


use Risk_ByPair.dta, clear

sort id

gen ccei_ind = 1 - Iv_afriat_ind
gen ccei_col = 1 - Iv_afriat_col
gen ccei_ind2 = 1 - Iv_afriat_ind2


g ccei_ind99=cond(ccei_ind>=0.99,1,0)
g ccei_ind95=cond(ccei_ind>=0.95,1,0)
g ccei_ind90=cond(ccei_ind>=0.90,1,0)
g ccei_col99=cond(ccei_col>=0.99,1,0)
g ccei_col95=cond(ccei_col>=0.95,1,0)
g ccei_col90=cond(ccei_col>=0.90,1,0)

g ccei_ind992=cond(ccei_ind2>=0.99,1,0)
g ccei_ind952=cond(ccei_ind2>=0.95,1,0)
g ccei_ind902=cond(ccei_ind2>=0.90,1,0)
g ccei_col992=cond(ccei_col>=0.99,1,0)
g ccei_col952=cond(ccei_col>=0.95,1,0)
g ccei_col902=cond(ccei_col>=0.90,1,0)

g ccei_ind_median = cond(ccei_ind >=0.9534135,1,0) 
g ccei_ind_median2 = cond(ccei_ind2 >=0.9534135,1,0) 

foreach t in 99 95 90 _median {
g ccei_both`t'=cond(ccei_ind`t'==1|ccei_ind`t'2==1,1,0)
replace ccei_both`t' =2 if ccei_ind`t'==1&ccei_ind`t'2==1
la define ccei_both`t' 0 "Both Irrational" 1 "Only One is Rational" 2 "Both Rational"
la val ccei_both`t' ccei_both`t'
}

**********************************************
* Section 2-1. Figure 2
**********************************************

global grph = "C:\Users\hahn0\Dropbox\RP\Data\graphs"

use "C:\Users\hahn0\Dropbox\RP\Data\Risk_merged.dta", clear

keep if inlist(groupid, 2220707, 2310810, 1410813, 1410721)
gen temp1 = groupid
levelsof temp1, local(glist)

foreach gid in `glist' {

    quietly: su id if temp1==`gid' & mover=="t"
    local id_mover = r(mean)
    
    quietly: su id if temp1==`gid' & mover=="f"
    local id_stayer = r(mean)

    twoway (scatter relconsmp lnrelprice if temp1==`gid' & mover=="f" & game_type==1, ///
        msize(large) msymbol(circle_hollow) mcolor(red)) ///
        (scatter relconsmp lnrelprice if temp1==`gid' & mover=="f" & game_type==2, ///
        msize(large) msymbol(x) mcolor(gs0)), ///
        legend(order(1 "Individual" 2 "Collective") col(1) position(8) ring(0) size(4)) ///
        xlabel(-2(0.5)2, labsize(4)) ///
        xtitle("Log price ratio (log(p{subscript:2}/p{subscript:1}))", size(4.5) height(4)) ///
        ylabel(0(0.2)1, labsize(4)) ///
        ytitle("Share for the second security (x{subscript:2}/(x{subscript:2}+x{subscript:1}))", size(4.5) height(4)) ///
        xline(2, lcolor(black)) ///
        yline(1, lcolor(black)) ///
        xline(0, lcolor(blue) lstyle(grid)) ///
        yline(0.5, lcolor(blue)) ///
        graphregion(color(white) lcolor(black)) ///
        plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick)) ///
        saving(temp1, replace) ///
        caption(Id: `id_stayer', size(4))

    twoway (scatter relconsmp lnrelprice if temp1==`gid' & mover=="t" & game_type==1, ///
        msize(large) msymbol(circle_hollow) mcolor(red)) ///
        (scatter relconsmp lnrelprice if temp1==`gid' & mover=="t" & game_type==2, ///
        msize(large) msymbol(x) mcolor(gs0)), ///
        legend(order(1 "Individual" 2 "Collective") col(1) position(8) ring(0) size(4)) ///
        xlabel(-2(0.5)2, labsize(4)) ///
        xtitle("Log price ratio (log(p{subscript:2}/p{subscript:1}))", size(4.5) height(4)) ///
        ylabel(0(0.2)1, labsize(4)) ///
        ytitle("Share for the second security (x{subscript:2}/(x{subscript:2}+x{subscript:1}))", size(4.5) height(4)) ///
        xline(2, lcolor(black)) ///
        yline(1, lcolor(black)) ///
        xline(0, lcolor(blue)) ///
        yline(0.5, lcolor(blue)) ///
        graphregion(color(white)) ///
        plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick)) ///
        saving(temp2, replace) ///
        caption(Id: `id_mover', size(4))

    gr combine temp1.gph temp2.gph, title(Group ID: `gid', size(4)) graphregion(color(white))
    gr export "$grph\relconsumption`gid'.png", replace

    erase temp1.gph
    erase temp2.gph
}



**********************************************
* Section 2-2. Figure 3
**********************************************

* Figure 3. (완료)
cdfplot ccei_col, by(ccei_both_median) ///
opt1( ///
lc(black blue red) ///
lp(solid shortdash solid) ///
lwidth(0.8 0.8 0.8) ///
) ///
legend( ///
order(1 "(Low, Low)" 2 "(Low, High)" 3 "(High, High)") col(1) ///
position(10) ring(0) ///
size(5) ///
) ///
xtitle("Collective Rationality", size(5) height(6)) ///
xscale(range(0.1 1.0)) xlabel(0.2 (0.2) 1.0, labgap(0.5) labsize(4)) ///
ytitle("Cumulative Frequency", size(5) height(6)) ///
yscale(range(0.0 1.0)) ylabel(0.0 (0.2) 1.0, labgap(0.5) labsize(4)) ///
graphregion(color(white))
//graph export "Figure1_CCEI_90.png", as(png) replace

* Figure 3. (중간값을 대신 사용한 버젼)
cdfplot ccei_col, by(ccei_both90) ///
opt1( ///
lc(black blue red) ///
lp(solid shortdash solid) ///
lwidth(0.8 0.8 0.8) ///
) ///
legend( ///
order(1 "(Low, Low)" 2 "(Low, High)" 3 "(High, High)") col(1) ///
position(10) ring(0) ///
size(5) ///
) ///
xtitle("Collective Rationality", size(5) height(6)) ///
xscale(range(0.1 1.0)) xlabel(0.2 (0.2) 1.0, labgap(0.5) labsize(4)) ///
ytitle("Cumulative Frequency", size(5) height(6)) ///
yscale(range(0.0 1.0)) ylabel(0.0 (0.2) 1.0, labgap(0.5) labsize(4)) ///
graphregion(color(white))
//graph export "Figure1_CCEI_90.png", as(png) replace

**********************************************
* Section 2-3. Figure 5
**********************************************

use Risk_ByPair.dta, clear

cdfplot riskaversion_col , by(riskaversionpair) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "(Low, Low)" 2 "(Low, High)" 3 "(High, High)") col(1)) ///
xlabel(0(0.2)1) xtitle(Collective Risk Aversion) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
// gr export "$results_main\CDF_RiskAversionAggregation.png", replace


**********************************************
* Section 2-4. Figure 7
**********************************************

use Risk_ByPair.dta, clear

* Figure 7-a Ver.A.
twoway (histogram uloss_col, percent), xtitle(Welfare Loss) graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) )
// gr export "$results_main/Hist_WelfareLoss.png", replace

* Figure 7-a Ver.B.
twoway ( histogram uloss_col, percent bcolor(gs10) barw(0.03) lcolor(gs1) lwidth(1) ) , ///
xtitle("Welfare Loss", size(5) height(6)) ///
xscale(range(0.0 1.0)) xlabel(0.0 (0.2) 1.0, labgap(0.5) labsize(4)) ///
ytitle("Frequency (%)", size(5) height(6)) ///
yscale(range(0 25)) ylabel(0 (5) 25, labgap(0.5) labsize(4)) ///
graphregion(color(white))


* Figure 7-b

use Risk_ByPair.dta, clear

g Im_col_lo = cond(Im_col<=$Imcut ,1,0)
gen ccei_col90 = cond( Iv_afriat_col <= 0.1 ,1,0) // added 2025.03.06

cdfplot uloss_col if $Iscutpair, by(Im_col_lo) opt1( lc(black blue red) lp(solid shortdash longdash) ) ///
legend(pos(5) ring(0) order(1 "High" 2 "Low") col(1)) ///
xlabel(0(0.1)1) xtitle(Welfare Loss) ///
graphregion(color(white) lcolor(black)  ) ///
plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) 
// gr export "$results_main\CDF_WelfareLoss_IsCut.png", replace