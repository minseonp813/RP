********************************************************************************
* Diagnostic: Sources of within-individual CCEI changes
* One observation per individual; covariates are baseline characteristics.
********************************************************************************

set more off
set matsize 8000

adopath + "`c(pwd)'"
cd "~/Dropbox/RP/RA_Byunghun"


********************************************************************************
* Construct outcomes: endline minus baseline
********************************************************************************

use "data/finalized_panel_individual_251206.dta", clear

keep id post ccei_i HighCCEI mathscore_i inclass_n_friends inclass_popularity
reshape wide ccei_i HighCCEI mathscore_i inclass_n_friends inclass_popularity, i(id) j(post)

gen delta_ccei_i = ccei_i1 - ccei_i0
gen delta_high_ccei = HighCCEI1 - HighCCEI0
gen delta_mathscore_i = mathscore_i1 - mathscore_i0
gen delta_inclass_n_friends = inclass_n_friends1 - inclass_n_friends0
gen delta_inclass_popularity = inclass_popularity1 - inclass_popularity0

label var ccei_i0 "Baseline CCEI"
label var ccei_i1 "Endline CCEI"
label var HighCCEI0 "Higher CCEI at baseline"
label var HighCCEI1 "Higher CCEI at endline"
label var delta_ccei_i "Change in CCEI"
label var delta_high_ccei "Change in Higher CCEI"
label var delta_mathscore_i "Change in Math Score"
label var delta_inclass_n_friends "Change in Out-degree"
label var delta_inclass_popularity "Change in In-degree"

assert inlist(delta_high_ccei, -1, 0, 1) if !missing(delta_high_ccei)

tempfile ccei_changes
save `ccei_changes'


********************************************************************************
* Keep baseline individual-level covariates
********************************************************************************

use "data/finalized_panel_individual_251206.dta", clear
keep if post == 0

foreach v in mathscore_i outgoing_i opened_i agreeable_i conscientious_i stable_i {
    gen `v'_missing = missing(`v')
    replace `v' = 0 if missing(`v')
}

local individual_char ///
    male_i height_i mathscore_i ///
    outgoing_i opened_i agreeable_i conscientious_i stable_i ///
    inclass_n_friends inclass_popularity

local missing_char ///
    mathscore_i_missing outgoing_i_missing opened_i_missing ///
    agreeable_i_missing conscientious_i_missing stable_i_missing

local controls `individual_char' `missing_char'
local change_controls ///
    delta_mathscore_i delta_inclass_n_friends delta_inclass_popularity

merge 1:1 id using `ccei_changes', keep(match) nogen
keep if !missing(delta_ccei_i, delta_high_ccei)

label var male_i "Male"
label var height_i "Height"
label var mathscore_i "Math Score"
label var inclass_n_friends "Out-degree"
label var inclass_popularity "In-degree"


********************************************************************************
* Descriptives and regressions
********************************************************************************

summ delta_ccei_i delta_high_ccei ccei_i0 ccei_i1 HighCCEI0 HighCCEI1
tab delta_high_ccei

eststo clear
eststo ccei_change: reg delta_ccei_i `controls', vce(cluster class)
eststo high_change: reg delta_high_ccei `controls', vce(cluster class)
eststo ccei_change_with_deltas: reg delta_ccei_i `controls' `change_controls', vce(cluster class)
eststo high_change_with_deltas: reg delta_high_ccei `controls' `change_controls', vce(cluster class)

esttab ccei_change high_change ccei_change_with_deltas high_change_with_deltas, ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    drop(*missing*)

esttab ccei_change high_change ccei_change_with_deltas high_change_with_deltas ///
    using "Tables/table_ccei_change_sources.tex", replace ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    drop(*missing*) fragment nonumbers nolines
