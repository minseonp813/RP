* Column (3): equality of max/min coefficients and of slopes across outcomes.
clear all
set more off
local out "results/new_indices/table5_coefficient_tests"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace
tempname results
tempfile tests
postfile `results' str24 comparison str8 term double difference se p low high ///
    long n clusters using `tests'
foreach outcome in ccei ceiv {
    local model "results/new_indices/risk_split/table5_ccei_pooled.ster"
    if "`outcome'"=="ceiv" local model "results/new_indices/table5_ceiv_3.ster"
    estimates use "`model'"
    assert e(N)==1304 & e(N_clust)==64
    local `outcome'_max = _b[ccei_max]
    local `outcome'_min = _b[ccei_min]
    if "`outcome'"=="ccei" {
        assert abs(_b[ccei_max]-.131)<.0005
        assert abs(_b[ccei_min]-.235)<.0005
    }
    else {
        assert abs(_b[ccei_max]-.176)<.0005
        assert abs(_b[ccei_min]-.086)<.0005
    }
    lincom ccei_min-ccei_max
    post `results' ("within_`outcome'") ("min-max") ///
        (r(estimate)) (r(se)) (r(p)) (r(lb)) (r(ub)) (e(N)) (e(N_clust))
}

* Regressing the outcome difference accounts for the cross-outcome covariance
* when the estimation sample, regressors, weights, and fixed effects coincide.
do "programs/prepare_collective_sample.do" "`c(pwd)'/data"
use "results/new_indices/analysis_sample.dta", clear
gen double outcome_gap = ccei_g-ceiv_g
reghdfe outcome_gap ccei_max ccei_min $t5_group $t5_friend $t5_share, ///
    absorb(class_fe) vce(cluster class_fe)
assert e(N)==1304 & e(N_clust)==64
assert abs(_b[ccei_max]-(`ccei_max'-`ceiv_max'))<1e-8
assert abs(_b[ccei_min]-(`ccei_min'-`ceiv_min'))<1e-8
foreach member in max min {
    lincom ccei_`member'
    post `results' ("ccei_minus_ceiv") ("`member'") ///
        (r(estimate)) (r(se)) (r(p)) (r(lb)) (r(ub)) (e(N)) (e(N_clust))
}
postclose `results'
use `tests', clear
export delimited using "`out'/tests.csv", replace
list, noobs
log close
