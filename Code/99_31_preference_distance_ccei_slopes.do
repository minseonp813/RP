* Run from Code. Continuous max/min CCEI slopes and common-CCEI comparisons.
clear all
set more off
local out "results/new_indices/preference_distance_slopes"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace
do "programs/prepare_preference_distance_sample.do"
keep if included & role==1
rename Ihat_ig distance_more
keep group_id post joint_category distance_more
tempfile distances
save `distances'
do "programs/prepare_communication_sample.do"
merge 1:1 group_id post using `distances', keep(match) assert(match master) nogen
assert _N==1101
gen double ccei_max_01 = 10*ccei_max
gen double ccei_min_01 = 10*ccei_min
quietly summarize ccei_max_01, meanonly
local reference_max = r(mean)
quietly summarize ccei_min_01, meanonly
local reference_min = r(mean)
preserve
    collapse (count) pairwaves=distance_more (mean) distance_more mean_max=ccei_max mean_min=ccei_min ///
        (sd) sd_max=ccei_max sd_min=ccei_min (min) low_max=ccei_max low_min=ccei_min ///
        (max) high_max=ccei_max high_min=ccei_min, by(joint_category)
    gen double reference_max = `reference_max'/10
    gen double reference_min = `reference_min'/10
    export delimited using "`out'/ccei_diagnostics.csv", replace
restore
tempname slopes means contrasts
tempfile slope_data mean_data contrast_data
postfile `slopes' str10 specification byte joint_category str3 member ///
    double estimate se low high p long n category_n clusters using `slope_data'
postfile `means' str10 specification byte joint_category double estimate se low high ///
    long n category_n clusters using `mean_data'
postfile `contrasts' str10 specification str10 measure double difference se low high p ///
    long n clusters using `contrast_data'

quietly regress distance_more ibn.joint_category, noconstant vce(cluster class_fe)
local critical = invttail(e(df_r),0.025)
forvalues category=1/4 {
    quietly count if joint_category==`category'
    local category_n = r(N)
    post `means' ("raw") (`category') (_b[`category'.joint_category]) ///
        (_se[`category'.joint_category]) ///
        (_b[`category'.joint_category]-`critical'*_se[`category'.joint_category]) ///
        (_b[`category'.joint_category]+`critical'*_se[`category'.joint_category]) ///
        (e(N)) (`category_n') (e(N_clust))
}
quietly lincom 4.joint_category-3.joint_category
post `contrasts' ("raw") ("mean") (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p)) (e(N)) (e(N_clust))

foreach specification in unadjusted controlled {
    local controls ""
    if "`specification'"=="controlled" local controls "$t5_group $t5_friend i.class_fe"
    regress distance_more ib1.joint_category##c.(ccei_max_01 ccei_min_01) `controls', vce(cluster class_fe)
    assert e(N)==1101
    local n = e(N)
    local clusters = e(N_clust)
    estimates save "`out'/`specification'.ster", replace
    forvalues category=1/4 {
        quietly count if joint_category==`category'
        local category_n = r(N)
        foreach member in max min {
            local expression "ccei_`member'_01"
            if `category'>1 local expression "`expression'+`category'.joint_category#c.ccei_`member'_01"
            quietly lincom `expression'
            post `slopes' ("`specification'") (`category') ("`member'") ///
                (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p)) (`n') (`category_n') (`clusters')
        }
    }
    foreach member in max min {
        quietly lincom 4.joint_category#c.ccei_`member'_01-3.joint_category#c.ccei_`member'_01
        post `contrasts' ("`specification'") ("`member'_slope") ///
            (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p)) (`n') (`clusters')
    }
    margins joint_category, at(ccei_max_01=(`reference_max') ccei_min_01=(`reference_min')) post
    matrix table = r(table)
    forvalues category=1/4 {
        quietly count if joint_category==`category'
        post `means' ("`specification'") (`category') (table[1,`category']) (table[2,`category']) ///
            (table[5,`category']) (table[6,`category']) (`n') (r(N)) (`clusters')
    }
    quietly lincom 4.joint_category-3.joint_category
    post `contrasts' ("`specification'") ("mean") (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p)) (`n') (`clusters')
}
postclose `slopes'
postclose `means'
postclose `contrasts'
use `slope_data', clear
assert _N==16 & !missing(estimate,se,low,high,p)
export delimited using "`out'/ccei_coefficients.csv", replace
use `mean_data', clear
assert _N==12 & !missing(estimate,se,low,high)
export delimited using "`out'/common_ccei_means.csv", replace
use `contrast_data', clear
assert _N==7 & !missing(difference,se,low,high,p)
export delimited using "`out'/both_minus_ceiv_only.csv", replace
di as result "SUCCESS: joint max/min slopes; common CCEI levels; identical 1,101 pair-wave samples."
log close
