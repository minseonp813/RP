************************************************************************
* WRITTEN BY Byunghun Hahn, 042825
* MAIN ANALYSIS: Tables
************************************************************************

/*********************************************
Table of Contents

  Section 1. Presets & Import Data [Line 16-36]
  Section 2. Tables
	(2-1) Table 1: Nothing Changed [Line 37-136]
	(2-2) Table 2: Same Specification, Slightly Different [Line 137-196]
	(2-3) Table 6: Same Specification, Slightly Different [Line 197-238]
*********************************************/

*texdoc do "C:\Users\hahn0\RP\3_All_Tables_replication(1,2,6).do"
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


*********************************************
* Table 1
*********************************************

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

egen ccei_ind_max = rowmax(ccei_ind ccei_ind2)
gen ccei_ind_dist = abs(ccei_ind - ccei_ind2)
egen height_gr_max = rowmax(height_gr height_gr2)
gen height_gr_dist = abs(height_gr - height_gr2)


g malepair_shin = cond(male == 1 & male2 == 1, 2, 0)
replace malepair_shin = 1 if (male == 1 & male2 == 2) | (male == 2 & male2 == 1)
replace malepair_shin = 3 if male == 2 & male2 == 2
replace malepair_shin = . if malepair_shin == 0
la de malepair_shin 1 "Hetero" 2 "Boys" 3 "Girls"
la val malepair_shin malepair_shin
tab malepair_shin

g malepair_co = cond(coed == 0, 0, .)
replace malepair_co = 1 if coed == 0 & malepair_shin == 3
replace malepair_co = 2 if coed == 1 & malepair_shin == 1
replace malepair_co = 3 if coed == 1 & malepair_shin == 2
replace malepair_co = 4 if coed == 1 & malepair_shin == 3
tab malepair_co
la de malepair_co 0 "Non Coed_Boys" 1 "Non Coed_Girls" 2 "Coed_Hetero" 3 "Coed_Boys" 4 "Coed_Girls"
la val malepair_co malepair_co

gen ccei1 = ccei_ind
gen ccei2 = ccei_ind2
gen ccei3 = ccei_col

reshape long ccei, i(groupid) j(source)

gen type = .
replace type = 2 if inlist(source, 1, 2)
replace type = 1 if source == 3

**************************************
ttest ccei, by(type)

local N_ind : di %9.0gc r(N_2)
local mean_ind : di %5.3f r(mu_2)
local sd_ind : di %5.3f r(sd_2)

local N_col : di %9.0gc r(N_1)
local mean_col : di %5.3f r(mu_1)
local sd_col : di %5.3f r(sd_1)

local pval : di %5.3f r(p)

capture file close myfile
file open myfile using "BH_TEX/Table1.tex", write replace
file write myfile ///
"\begin{table}[ht]" _n ///
"\centering" _n ///
"\begin{tabular}{lcccc}" _n ///
"\toprule" _n ///
"& Observations & Mean & Standard deviation & \textit{p}-value \\" _n ///
"\midrule" _n ///
"individual decision & `N_ind' & `mean_ind' & `sd_ind' & \\" _n ///
"collective decision & `N_col' & `mean_col' & `sd_col' & `pval' \\" _n ///
"\bottomrule" _n ///
"\end{tabular}" _n ///
"\caption{Summary statistics of the CCEI scores}" _n ///
"\end{table}" _n
file close myfile

******************************************
* Table 2
******************************************

use Risk_ByPair.dta, clear

gen ccei_ind = 1 - Iv_afriat_ind
gen ccei_col = 1 - Iv_afriat_col
gen ccei_ind2 = 1 - Iv_afriat_ind2
egen ccei_ind_max = rowmax(ccei_ind ccei_ind2)
gen ccei_ind_dist = abs(ccei_ind - ccei_ind2)

egen height_gr_max = rowmax(height_gr height_gr2)
gen height_gr_dist = abs(height_gr - height_gr2)

foreach var in mathscore_max mathscore_dist height_gr_max height_gr_dist {
    gen `var'_missing = missing(`var')
    replace `var' = 0 if missing(`var')
}

gen malepair_shin = cond(male == 1 & male2 == 1, 2, 0)
replace malepair_shin = 1 if (male == 1 & male2 == 2) | (male == 2 & male2 == 1)
replace malepair_shin = 3 if male == 2 & male2 == 2
replace malepair_shin = . if malepair_shin == 0
label define malepair_shin 1 "Hetero" 2 "Boys" 3 "Girls"
label values malepair_shin malepair_shin

gen malepair_co = cond(coed == 0, 0, .)
replace malepair_co = 1 if coed == 0 & malepair_shin == 3
replace malepair_co = 2 if coed == 1 & malepair_shin == 1
replace malepair_co = 3 if coed == 1 & malepair_shin == 2
replace malepair_co = 4 if coed == 1 & malepair_shin == 3
label define malepair_co 0 "Non Coed_Boys" 1 "Non Coed_Girls" 2 "Coed_Hetero" 3 "Coed_Boys" 4 "Coed_Girls"
label values malepair_co malepair_co

eststo clear

* Model 1
eststo m1: reg ccei_col ccei_ind_max ccei_ind_dist i.class, r cl(class)
estadd local classfe "Yes"
estadd local malepair "No"
estadd local friendship "No"
estadd local height "No"

* Model 2
eststo m2: reg ccei_col ccei_ind_max ccei_ind_dist ///
    mathscore_max mathscore_dist ///
    height_gr_max height_gr_dist ///
    height_gr_dist_missing height_gr_max_missing ///
    mathscore_dist_missing mathscore_max_missing ///
    ib(1).malepair_co i.friendship i.class, r cl(class)
estadd local classfe "Yes"
estadd local malepair "Yes"
estadd local friendship "Yes"
estadd local height "Yes"

* Model 3
eststo m3: reg ccei_col ///
    mathscore_max mathscore_dist ///
    height_gr_max height_gr_dist ///
    height_gr_dist_missing height_gr_max_missing ///
    mathscore_dist_missing mathscore_max_missing ///
    ib(1).malepair_co i.friendship i.class, r cl(class)
estadd local classfe "Yes"
estadd local malepair "Yes"
estadd local friendship "Yes"
estadd local height "Yes"

esttab m1 m2 m3 using "BH_TEX/Table2.tex", ///
    keep(ccei_ind_max ccei_ind_dist mathscore_max mathscore_dist _cons) ///
    se star(* 0.1 ** 0.05 *** 0.01) ///
    label booktabs replace ///
    title(Table 2: Econometric analysis on rationality extension) ///
    s(N classfe malepair friendship height, ///
      label("N" "Class FE" "Gender Pair FE" "Friendship FE" "Height FE"))



************************************
* Table 6
************************************

use Risk_ByPair.dta, clear

foreach var in mathscore_max mathscore_dist {
    gen `var'_missing = missing(`var')
    replace `var' = 0 if missing(`var')
}

eststo clear

eststo m1: reg riskaversion_col riskaversion_ind_max riskaversion_ind_dist i.class, vce(robust)
estadd local classfe "Yes"
estadd local genderpair "No"
estadd local friendship "No"

eststo m2: reg riskaversion_col riskaversion_ind_max riskaversion_ind_dist ///
    mathscore_max mathscore_dist mathscore_max_missing mathscore_dist_missing ///
    i.class i.malepair i.friendship, vce(robust)
estadd local classfe "Yes"
estadd local genderpair "Yes"
estadd local friendship "Yes"

eststo m3: reg riskaversion_col mathscore_max mathscore_dist ///
    mathscore_max_missing mathscore_dist_missing ///
    i.class i.malepair i.friendship, vce(robust)
estadd local classfe "Yes"
estadd local genderpair "Yes"
estadd local friendship "Yes"

esttab m1 m2 m3 using "BH_TEX/Table6.tex", ///
    keep(riskaversion_ind_max riskaversion_ind_dist mathscore_max mathscore_dist _cons) ///
    se star(* 0.1 ** 0.05 *** 0.01) ///
    label booktabs replace ///
    title(Table 6, using math_missing dummy) ///
    s(N classfe genderpair friendship, label("N" "Class FE" "Gender Pair FE" "Friendship FE"))

use Risk_ByPair.dta, clear
drop if mathscore_dist == .

eststo clear

eststo d1: reg riskaversion_col riskaversion_ind_max riskaversion_ind_dist i.class, vce(robust)
estadd local classfe "Yes"
estadd local genderpair "No"
estadd local friendship "No"

eststo d2: reg riskaversion_col riskaversion_ind_max riskaversion_ind_dist ///
    mathscore_max mathscore_dist i.class i.malepair i.friendship, vce(robust)
estadd local classfe "Yes"
estadd local genderpair "Yes"
estadd local friendship "Yes"

eststo d3: reg riskaversion_col mathscore_max mathscore_dist ///
    i.class i.malepair i.friendship, vce(robust)
estadd local classfe "Yes"
estadd local genderpair "Yes"
estadd local friendship "Yes"

esttab d1 d2 d3 using "BH_TEX/Table6_drop.tex", ///
    keep(riskaversion_ind_max riskaversion_ind_dist mathscore_max mathscore_dist _cons) ///
    se star(* 0.1 ** 0.05 *** 0.01) ///
    label booktabs replace ///
    title(Table 6, dropped math missings) ///
    s(N classfe genderpair friendship, label("N" "Class FE" "Gender Pair FE" "Friendship FE"))