* Run from Code. Risk-survey distributions across Figure 6's four CCEI/CEIV categories.
clear all
set more off
local out "results/new_indices/risk_survey_categories"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace
do "programs/load_risk_survey_panel.do"

gen byte ccei_endpoint = ccei_g >= 1-1e-9
gen byte ceiv_endpoint = ceiv_g >= 1-1e-9
assert ceiv_endpoint == (CEIV_lower >= 1-1e-9)
assert ceiv_endpoint == (CEIV_upper >= 1-1e-9)
gen byte joint_category = 1 + ccei_endpoint + 2*ceiv_endpoint
label define joint 1 "CCEI<1, CEIV<1" 2 "CCEI=1, CEIV<1" ///
    3 "CCEI<1, CEIV=1" 4 "CCEI=1, CEIV=1"
label values joint_category joint
assert !missing(ccei_i, ccei_j)
bysort group_id post: assert ccei_i[1] == ccei_j[2] & ccei_i[2] == ccei_j[1]
gen byte ccei_tied = abs(ccei_i-ccei_j) <= 1e-9
gen byte lower_member = ccei_i < ccei_j-1e-9
bysort group_id post: egen byte lower_count = total(lower_member)
assert lower_count == 1-ccei_tied
gen byte untied = !ccei_tied
quietly count if lower_member
assert r(N) == 1105
egen byte pairwave_tag = tag(group_id post)
tabulate joint_category if pairwave_tag
preserve
    keep if pairwave_tag
    collapse (count) pairwaves=post (sum) tied_pairwaves=ccei_tied untied_pairwaves=untied, by(joint_category)
    gen double percent = 100*pairwaves/1304
    export delimited using "`out'/category_counts.csv", replace nolabel
restore

tempname means shares tests
tempfile mean_data share_data test_data
postfile `means' str4 sample str11 question byte joint_category double mean se low high long n clusters using `mean_data'
postfile `shares' str4 sample str11 question byte joint_category response double share se low high long n total clusters using `share_data'
postfile `tests' str4 sample str11 question str16 measure str17 comparison double difference se low high p long n clusters using `test_data'

foreach sample in all less {
local restriction "1"
if "`sample'" == "less" local restriction "lower_member"
foreach q in cooperation similar whose {
    local survey risk_`q'_i
    local responses = cond("`q'" == "cooperation",5,4)
    quietly count if !missing(`survey') & `restriction'
    local survey_n = r(N)
    assert `survey_n' == cond("`sample'" == "all",2607,1105)
    if "`q'" != "whose" {
        quietly regress `survey' ibn.joint_category if `restriction', noconstant vce(cluster class_fe)
        local clusters = e(N_clust)
        local critical = invttail(e(df_r),0.025)
        forvalues category = 1/4 {
            quietly count if e(sample) & joint_category == `category'
            post `means' ("`sample'") ("`q'") (`category') (_b[`category'.joint_category]) ///
                (_se[`category'.joint_category]) ///
                (_b[`category'.joint_category]-`critical'*_se[`category'.joint_category]) ///
                (_b[`category'.joint_category]+`critical'*_se[`category'.joint_category]) ///
                (r(N)) (`clusters')
        }
        quietly test (1.joint_category=4.joint_category) ///
            (2.joint_category=4.joint_category) (3.joint_category=4.joint_category)
        post `tests' ("`sample'") ("`q'") ("mean_score") ("all_categories") ///
            (.) (.) (.) (.) (r(p)) (`survey_n') (`clusters')
        quietly lincom 4.joint_category - 3.joint_category
        post `tests' ("`sample'") ("`q'") ("mean_score") ("both_minus_ceiv") ///
            (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p)) (`survey_n') (`clusters')
    }

    forvalues response = 1/`responses' {
        tempvar chosen
        gen byte `chosen' = `survey' == `response' if !missing(`survey') & `restriction'
        quietly regress `chosen' ibn.joint_category, noconstant vce(cluster class_fe)
        local clusters = e(N_clust)
        local critical = invttail(e(df_r),0.025)
        forvalues category = 1/4 {
            quietly count if e(sample) & joint_category == `category'
            local total = r(N)
            quietly count if e(sample) & joint_category == `category' & `chosen'
            post `shares' ("`sample'") ("`q'") (`category') (`response') ///
                (_b[`category'.joint_category]) (_se[`category'.joint_category]) ///
                (_b[`category'.joint_category]-`critical'*_se[`category'.joint_category]) ///
                (_b[`category'.joint_category]+`critical'*_se[`category'.joint_category]) ///
                (r(N)) (`total') (`clusters')
        }
        * The both-versus-CEIV-only contrast holds full CEIV status fixed.
        quietly lincom 4.joint_category - 3.joint_category
        post `tests' ("`sample'") ("`q'") ("response_`response'") ("both_minus_ceiv") ///
            (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p)) (`survey_n') (`clusters')
        drop `chosen'
    }

    * Stack K-1 response indicators for a joint test of the entire distribution.
    * Class clustering includes dependence across indicators, members, and waves.
    preserve
        keep if !missing(`survey') & `restriction'
        gen long respondent = _n
        expand `=`responses'-1'
        bysort respondent: gen byte answer = _n
        gen byte chosen = `survey' == answer
        quietly regress chosen i.answer##ib4.joint_category, vce(cluster class_fe)
        quietly testparm i.joint_category i.answer#i.joint_category
        post `tests' ("`sample'") ("`q'") ("distribution") ("all_categories") ///
            (.) (.) (.) (.) (r(p)) (`survey_n') (e(N_clust))
        local constraints "(3.joint_category=0)"
        forvalues response = 2/`=`responses'-1' {
            local constraints "`constraints' (`response'.answer#3.joint_category=0)"
        }
        quietly test `constraints'
        post `tests' ("`sample'") ("`q'") ("distribution") ("both_vs_ceiv") ///
            (.) (.) (.) (.) (r(p)) (`survey_n') (e(N_clust))
    restore
}
}
postclose `means'
postclose `shares'
postclose `tests'
use `mean_data', clear
assert _N == 16 & !missing(mean,se,low,high)
sort sample question joint_category
export delimited using "`out'/score_means.csv", replace
use `share_data', clear
assert _N == 104 & !missing(share,se,low,high)
bysort sample question joint_category: egen double sum_check = total(share)
assert abs(sum_check-1) < 1e-8
drop sum_check
sort sample question joint_category response
export delimited using "`out'/response_shares.csv", replace
use `test_data', clear
assert !missing(p,n,clusters)
sort sample question measure comparison
export delimited using "`out'/category_tests.csv", replace
di as result "SUCCESS: all-member and lower-CCEI samples, four categories, shares, means and class-clustered tests validated."
log close
