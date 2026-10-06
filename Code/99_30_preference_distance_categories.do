* Run from Code. Relative member-to-group RP distance by rationality role and group outcomes.
clear all
set more off
local out "results/new_indices/preference_distance_categories"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace
do "programs/prepare_preference_distance_sample.do"
preserve
    keep if pairwave_tag
    collapse (count) eligible_pairwaves=post (sum) tied_pairwaves=ccei_tied ///
        missing_untied_pairwaves=missing_untied included_pairwaves=included, by(joint_category)
    assert eligible_pairwaves==tied_pairwaves+missing_untied_pairwaves+included_pairwaves
    export delimited using "`out'/sample_counts.csv", replace
restore

tempname means gaps contrasts
tempfile mean_data gap_data contrast_data
postfile `means' byte joint_category role double mean se low high long pairwaves pairs clusters using `mean_data'
postfile `gaps' byte joint_category double difference se low high p long pairwaves clusters using `gap_data'
postfile `contrasts' str12 measure byte higher lower double difference se low high p long pairwaves clusters using `contrast_data'
quietly regress Ihat_ig ibn.joint_category#ibn.role if included, noconstant vce(cluster class_fe)
local clusters = e(N_clust)
local critical = invttail(e(df_r),0.025)
assert e(N)==2202
forvalues category=1/4 {
    quietly count if e(sample) & joint_category==`category' & role==1
    local n = r(N)
    tempvar pairtag
    egen byte `pairtag' = tag(group_id) if included & joint_category==`category'
    quietly count if `pairtag'==1
    local pairs = r(N)
    forvalues role=1/2 {
        local b = _b[`category'.joint_category#`role'.role]
        local se = _se[`category'.joint_category#`role'.role]
        post `means' (`category') (`role') (`b') (`se') ///
            (`b'-`critical'*`se') (`b'+`critical'*`se') (`n') (`pairs') (`clusters')
    }
    quietly lincom `category'.joint_category#2.role-`category'.joint_category#1.role
    post `gaps' (`category') (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p)) (`n') (`clusters')
}
forvalues higher=2/4 {
    forvalues lower=1/`=`higher'-1' {
        forvalues role=1/2 {
            local name = cond(`role'==1,"more","less")
            quietly lincom `higher'.joint_category#`role'.role-`lower'.joint_category#`role'.role
            post `contrasts' ("`name'") (`higher') (`lower') ///
                (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p)) (e(N)/2) (`clusters')
        }
        quietly lincom (`higher'.joint_category#2.role-`higher'.joint_category#1.role) - ///
            (`lower'.joint_category#2.role-`lower'.joint_category#1.role)
        post `contrasts' ("gap") (`higher') (`lower') ///
            (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p)) (e(N)/2) (`clusters')
    }
}
postclose `means'
postclose `gaps'
postclose `contrasts'
use `mean_data', clear
assert _N==8 & !missing(mean,se,low,high)
bysort joint_category: assert abs(mean[1]+mean[2]-1)<1e-8
export delimited using "`out'/distance_means.csv", replace
use `gap_data', clear
assert _N==4 & !missing(difference,se,low,high,p)
export delimited using "`out'/within_pair_gaps.csv", replace
use `contrast_data', clear
assert _N==18 & !missing(difference,se,low,high,p)
export delimited using "`out'/category_contrasts.csv", replace
di as result "SUCCESS: 1,101 untied pair-waves; complementary role means and class-clustered paired contrasts."
log close
