* Baseline characteristics and their changes predicting endline-minus-baseline CCEI.
* Compare baseline controls with baseline controls plus changes, without fixed effects.
* Run from Code. No partner characteristics or within-pair differences enter.
set more off

local data_dir "data"
local out_dir "results/tables"
capture mkdir "results"
capture mkdir "`out_dir'"
tempfile coefficients
capture log close ccei_change
log using "`out_dir'/individual_ccei_change.log", name(ccei_change) text replace

use "`data_dir'/panel_individual.dta", clear
isid id post
bysort id (post): assert _N == 2 & post[1] == 0 & post[2] == 1
assert !missing(ccei_i)
bysort id (post): gen double ccei_change = ccei_i[2] - ccei_i[1]
bysort id: egen n_distance = total(!missing(Ihat_ig))
gen byte table3_sample = (n_distance == 2)


local student "male_i height_i mathscore_i outgoing_i opened_i agreeable_i conscientious_i stable_i inclass_n_friends_i inclass_popularity_i"
local missing_controls "mathscore_i_missing outgoing_i_missing opened_i_missing agreeable_i_missing conscientious_i_missing stable_i_missing"

foreach var in mathscore_i inclass_n_friends_i inclass_popularity_i {
    bysort id (post): gen double `var'_change = `var'[2] - `var'[1]
}

local student_chng "mathscore_i_change inclass_n_friends_i_change inclass_popularity_i_change"

keep if post == 0
isid id
assert _N == 1304
count if table3_sample
assert r(N) == 1256

label var male_i "Male"
label var height_i "Height"
label var mathscore_i "Math score"
label var outgoing_i "Outgoingness"
label var opened_i "Openness"
label var agreeable_i "Agreeableness"
label var conscientious_i "Conscientiousness"
label var stable_i "Emotional stability"
label var inclass_n_friends_i "Out-degree"
label var inclass_popularity_i "In-degree"
label var mathscore_i_change "Change in math score"
label var inclass_n_friends_i_change "Change in out-degree"
label var inclass_popularity_i_change "Change in in-degree"

eststo clear
eststo change1: regress ccei_change `student' `missing_controls' ///
    if table3_sample == 1, vce(cluster class)
eststo change2: regress ccei_change `student' `student_chng' `missing_controls' ///
    if table3_sample == 1, vce(cluster class)

postfile coefficient_file byte column str32 variable double beta se p N ///
    df clusters using `coefficients'
forvalues column = 1/2 {
    estimates restore change`column'
    assert e(N) == 1256 & e(N_clust) == 64
    local reported "`student'"
    if `column' == 2 local reported "`student' `student_chng'"
    foreach variable in `reported' {
        post coefficient_file (`column') ("`variable'") (_b[`variable']) ///
            (_se[`variable']) (2 * ttail(e(df_r), abs(_b[`variable'] / _se[`variable']))) ///
            (e(N)) (e(df_r)) (e(N_clust))
    }
    quietly summarize ccei_change if e(sample)
    estadd scalar mean_change = r(mean)
    estadd local class_fe "No"
    estadd local missing_indicators "Yes"
    estadd scalar class_clusters = e(N_clust)
}
postclose coefficient_file

esttab change1 change2, b(3) se(3) label ///
    keep(`student' `student_chng') order(`student' `student_chng') ///
    mtitles("Baseline" "Baseline + changes") star(+ 0.1 * 0.05 ** 0.01) ///
    stats(N r2 class_fe, labels("Individuals" "R-squared" "Class fixed effects") fmt(0 3 %9s))

esttab change1 change2 using "`out_dir'/table_individual_ccei_change.tex", replace ///
    b(3) se(3) keep(`student' `student_chng') order(`student' `student_chng') ///
    stats(N r2 mean_change class_clusters class_fe missing_indicators, ///
    labels("Individuals" "R-squared" "Mean change in CCEI" "Class clusters" "Class fixed effects" ///
    "Missing-value indicators") fmt(0 3 3 0 %9s %9s)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    nomtitles fragment nonumbers nolines prefoot("\midrule") postfoot("\bottomrule")

preserve
    use `coefficients', clear
    export delimited using "`out_dir'/individual_ccei_change_coefficients.csv", replace
restore
log close ccei_change
