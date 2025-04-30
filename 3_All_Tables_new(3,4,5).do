************************************************************************
* WRITTEN BY Byunghun Hahn, 042825
* MAIN ANALYSIS: Tables
************************************************************************

/*********************************************
Table of Contents

  Section 1. Presets & Import Data [Line 16-34]
  Section 2. Tables
	(2-1) Table 3: [Line 35-200]
	(2-2) Table 4: [Line 201-277]
	(2-3) Table 5: [Line 278-418]
*********************************************/

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

******************************************
* Table 3.A.: alpha
******************************************

use Risk_ByPair.dta, clear
rename a_ind a_ind1
drop a_crra_ind a_crra_col a_crra_ind2
reshape long a_, i(groupid) j(source) string
rename a_ a

gen group = .
replace group = 0 if inlist(source, "ind1", "ind2")
replace group = 1 if source == "col"

gen condA = 1
gen condB = (Im_ind <= 0.15 & Im_ind2 <= 0.15 & Im_col <= 0.15)
gen condC = (Iv_afriat_ind <= 0.1 & Iv_afriat_ind2 <= 0.1 & Iv_afriat_col <= 0.1)
gen condD = (Is_ind <= 0.1 & Is_ind2 <= 0.1 & Is_col <= 0.1)

local panel_list condA condB condC condD

matrix Table3 = J(12, 4, .)

local panelnum = 0

foreach cond of local panel_list {
    local row_panel = `panelnum' * 3 + 1

    forvalues g = 0/1 {
        local i = `row_panel' + 1 + `g'

        quietly count if group == `g' & `cond'
        local N = r(N)

        quietly sum a if group == `g' & `cond'
        local mean = r(mean)
        local sd = r(sd)

        quietly ttest a if `cond', by(group)
        if `g' == 1 {
            local pval = r(p)
        }
        else {
            local pval = .
        }

        matrix Table3[`i',1] = `N'
        matrix Table3[`i',2] = `mean'
        matrix Table3[`i',3] = `sd'
        matrix Table3[`i',4] = `pval'
    }

    local ++panelnum
}

matrix colnames Table3 = "N" "mean" "sd" "pval"

matrix rownames Table3 = "Panel A" "Individual" "Collective" ///
                         "Panel B" "Individual" "Collective" ///
                         "Panel C" "Individual" "Collective" ///
                         "Panel D" "Individual" "Collective"

esttab matrix(Table3, fmt(%9.0g %9.3f %9.3f %9.3f)) using "BH_TEX/Table3_A.tex", ///
    nomtitles nonumbers replace


******************************************
* Table 3.B.: rdu
******************************************

use Risk_ByPair.dta, clear

rename rdu_ind rdu_ind1
drop rdu_crra_ind rdu_crra_col rdu_crra_ind2 rdu_crrapair

reshape long rdu_, i(groupid) j(source) string
rename rdu_ rdu

gen group = .
replace group = 0 if inlist(source, "ind1", "ind2")
replace group = 1 if source == "col"

gen condA = 1  // 전체 샘플
gen condB = (Im_ind <= 0.15 & Im_ind2 <= 0.15 & Im_col <= 0.15)
gen condC = (Iv_afriat_ind <= 0.1 & Iv_afriat_ind2 <= 0.1 & Iv_afriat_col <= 0.1)
gen condD = (Is_ind <= 0.1 & Is_ind2 <= 0.1 & Is_col <= 0.1)

local panel_list condA condB condC condD

matrix Table3_B = J(12, 4, .)

local panelnum = 0

foreach cond of local panel_list {
    local row_panel = `panelnum' * 3 + 1

    forvalues g = 0/1 {
        local i = `row_panel' + 1 + `g'

        quietly count if group == `g' & `cond'
        local N = r(N)

        quietly sum rdu if group == `g' & `cond'
        local mean = r(mean)
        local sd = r(sd)

        quietly ttest rdu if `cond', by(group)
        if `g' == 1 {
            local pval = r(p)
        }
        else {
            local pval = .
        }

        matrix Table3_B[`i',1] = `N'
        matrix Table3_B[`i',2] = `mean'
        matrix Table3_B[`i',3] = `sd'
        matrix Table3_B[`i',4] = `pval'
    }

    local ++panelnum
}

matrix colnames Table3_B = "N" "mean" "sd" "pval"

matrix rownames Table3_B = "Panel A" "Individual" "Collective" ///
                           "Panel B" "Individual" "Collective" ///
                           "Panel C" "Individual" "Collective" ///
                           "Panel D" "Individual" "Collective"

esttab matrix(Table3_B, fmt(%9.0g %9.3f %9.3f %9.3f)) using "BH_TEX/Table3_B.tex", ///
    nomtitles nonumbers replace

use Risk_ByPair.dta, clear
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

la de rdu_col 0 "EU" 1 "DAU"
la val rdu_col rdu_col

// texdoc do "C:\Users\hahn0\RP\3_All_Tables_new(3,4,5).do"

************************************
* Table 4
************************************

use Risk_ByPair.dta, clear
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

la de rdu_col 0 "EU" 1 "DAU"
la val rdu_col rdu_col


texdoc init "BH_TEX\Table4.tex", replace

texdoc stlog


* (1) Full sample
di "Panel A: Full Sample"
tab rdupair rdu_col

* (2) ccei >= 0.9
di "Panel B: CCEI ≥ 0.9"
tab rdupair rdu_col if (ccei_col >= 0.9 & ccei_ind >= 0.9 & ccei_ind2 >= 0.9)

* (3) ccei ≥ 0.8
di "Panel C: CCEI ≥ 0.8"
tab rdupair rdu_col if (ccei_col >= 0.8 & ccei_ind >= 0.8 & ccei_ind2 >= 0.8)

* (4) Im ≤ 0.2
di "Panel D: Im ≤ 0.2"
tab rdupair rdu_col if (Im_ind <= 0.2 & Im_ind2 <= 0.2 & Im_col <= 0.2)

* (5) Im ≤ 0.19
di "Panel E: Im ≤ 0.19"
tab rdupair rdu_col if (Im_ind <= 0.19 & Im_ind2 <= 0.19 & Im_col <= 0.19)

* (6) Is ≤ 0.1
di "Panel F: Is ≤ 0.1"
tab rdupair rdu_col if (Is_ind <= 0.1 & Is_ind2 <= 0.1 & Is_col <= 0.1)

* (7) Is ≤ 0.08
di "Panel G: Is ≤ 0.08"
tab rdupair rdu_col if (Is_ind <= 0.08 & Is_ind2 <= 0.08 & Is_col <= 0.08)

texdoc stlog close

texdoc close

******************************************
* Table 5.A. (riskaversion)
******************************************

use Risk_ByPair.dta, clear

rename riskaversion_ind riskaversion_ind1
drop riskaversion_ind_hi riskaversion_ind_hi2 riskaversion_ind_max riskaversion_ind_min riskaversion_ind_ave riskaversion_ind_dist riskaversionpair

reshape long riskaversion_, i(groupid) j(source) string
rename riskaversion_ riskaversion

gen group = .
replace group = 0 if inlist(source, "ind1", "ind2")
replace group = 1 if source == "col"

gen condA = 1 
gen condB = (Im_ind <= 0.15 & Im_ind2 <= 0.15 & Im_col <= 0.15)
gen condC = (Iv_afriat_ind <= 0.1 & Iv_afriat_ind2 <= 0.1 & Iv_afriat_col <= 0.1)
gen condD = (Is_ind <= 0.1 & Is_ind2 <= 0.1 & Is_col <= 0.1)

local panel_list condA condB condC condD

matrix Table5_A = J(12, 4, .)

local panelnum = 0

foreach cond of local panel_list {
    local row_panel = `panelnum' * 3 + 1

    forvalues g = 0/1 {
        local i = `row_panel' + 1 + `g'

        quietly count if group == `g' & `cond'
        local N = r(N)

        quietly sum riskaversion if group == `g' & `cond'
        local mean = r(mean)
        local sd = r(sd)

        quietly ttest riskaversion if `cond', by(group)
        if `g' == 1 {
            local pval = r(p)
        }
        else {
            local pval = .
        }

        matrix Table5_A[`i',1] = `N'
        matrix Table5_A[`i',2] = `mean'
        matrix Table5_A[`i',3] = `sd'
        matrix Table5_A[`i',4] = `pval'
    }

    local ++panelnum
}

matrix colnames Table5_A = "N" "mean" "sd" "pval"

matrix rownames Table5_A = "Panel A" "Individual" "Collective" ///
                           "Panel B" "Individual" "Collective" ///
                           "Panel C" "Individual" "Collective" ///
                           "Panel D" "Individual" "Collective"

esttab matrix(Table5_A, fmt(%9.0g %9.3f %9.3f %9.3f)) using "BH_TEX/Table5_A.tex", ///
    nomtitles nonumbers replace





******************************************
* Table 5.B. (riskpremium)
******************************************

use Risk_ByPair.dta, clear

rename riskpremium_ind riskpremium_ind1

drop riskpremium_crra_ind riskpremium_crra_col riskpremium_ind_hi riskpremium_crra_ind_hi ///
     riskpremium_crra_ind2 riskpremium_ind_hi2 riskpremium_crra_ind_hi2 riskpremium_ind_max ///
     riskpremium_ind_min riskpremium_ind_ave riskpremium_ind_dist riskpremium_crra_ind_max ///
     riskpremium_crra_ind_min riskpremium_crra_ind_dist riskpremiumpair riskpremium_crra_ind_ave

reshape long riskpremium_, i(groupid) j(source) string
rename riskpremium_ riskpremium

gen group = .
replace group = 0 if inlist(source, "ind1", "ind2")
replace group = 1 if source == "col"

gen condA = 1 
gen condB = (Im_ind <= 0.15 & Im_ind2 <= 0.15 & Im_col <= 0.15)
gen condC = (Iv_afriat_ind <= 0.1 & Iv_afriat_ind2 <= 0.1 & Iv_afriat_col <= 0.1)
gen condD = (Is_ind <= 0.1 & Is_ind2 <= 0.1 & Is_col <= 0.1)

local panel_list condA condB condC condD

matrix Table5_B = J(12, 4, .)

local panelnum = 0

foreach cond of local panel_list {
    local row_panel = `panelnum' * 3 + 1

    forvalues g = 0/1 {
        local i = `row_panel' + 1 + `g'

        quietly count if group == `g' & `cond'
        local N = r(N)

        quietly sum riskpremium if group == `g' & `cond'
        local mean = r(mean)
        local sd = r(sd)

        quietly ttest riskpremium if `cond', by(group)
        if `g' == 1 {
            local pval = r(p)
        }
        else {
            local pval = .
        }

        matrix Table5_B[`i',1] = `N'
        matrix Table5_B[`i',2] = `mean'
        matrix Table5_B[`i',3] = `sd'
        matrix Table5_B[`i',4] = `pval'
    }

    local ++panelnum
}

matrix colnames Table5_B = "N" "mean" "sd" "pval"

matrix rownames Table5_B = "Panel A" "Individual" "Collective" ///
                           "Panel B" "Individual" "Collective" ///
                           "Panel C" "Individual" "Collective" ///
                           "Panel D" "Individual" "Collective"

esttab matrix(Table5_B, fmt(%9.0g %9.3f %9.3f %9.3f)) using "BH_TEX/Table5_B.tex", ///
    nomtitles nonumbers replace