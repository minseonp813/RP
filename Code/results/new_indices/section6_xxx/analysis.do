* Run from Code; evidence for the XXX passages in Section 6 only.
clear all
set more off
adopath ++ "programs"
local out "results/new_indices/section6_xxx"
log using "`out'/analysis.log", text replace
use "results/new_indices/collective_rationality_summary/analysis_sample.dta", clear
isid group_id post
assert _N==1304
local student "mathscore_max mathscore_dist height_max height_dist male_diff outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_max conscientious_dist stable_max stable_dist mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local friendship "inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist friend"
local shares "corner_share_max corner_share_dist mid_share_max mid_share_dist"
local controls "`student' `friendship' `shares'"

* Other covariates: continuous derivatives, with class-clustered inference.
estimates use "results/new_indices/collective_rationality_summary/figure6.ster"
assert e(N)==1304 & e(N_clust)==64 & e(converged)==1
local full_ll = e(ll)
* Saved estimates omit e(sample); refit to restore the exact margins sample.
collective_multinomial_ame joint_category, controls(`controls') base(1) iterations(300)
assert abs(e(ll)-`full_ll')<1e-5
tempname ames
tempfile covariate_results
postfile `ames' byte outcome str32 term double estimate se p low high scale using `covariate_results'
forvalues category=1/4 {
    quietly margins, dydx(`controls') predict(outcome(`category'))
    matrix effects = r(table)
    local column = 0
    foreach term in `controls' {
        local ++column
        quietly summarize `term'
        local scale = r(sd)
        if inlist("`term'","male_diff","friend") local scale = 1
        post `ames' (`category') ("`term'") (effects[1,`column']) (effects[2,`column']) ///
            (effects[4,`column']) (effects[5,`column']) (effects[6,`column']) (`scale')
    }
}
postclose `ames'
preserve
    use `covariate_results', clear
    export delimited using "`out'/covariate_ame.csv", replace
restore

* Exact Shapley allocation of log-likelihood gains beyond class fixed effects.
* Four blocks imply 16 subset fits, all on the identical sample.
matrix likelihoods = J(16,1,.)
forvalues subset=0/15 {
    local regressors ""
    if mod(`subset',2)==1 local regressors "ccei_max_01 ccei_min_01"
    if mod(floor(`subset'/2),2)==1 local regressors "`regressors' `student'"
    if mod(floor(`subset'/4),2)==1 local regressors "`regressors' `friendship'"
    if mod(floor(`subset'/8),2)==1 local regressors "`regressors' `shares'"
    quietly mlogit joint_category `regressors' i.class_fe, baseoutcome(1) difficult iterate(300)
    assert e(N)==1304 & e(converged)==1
    matrix likelihoods[`subset'+1,1] = e(ll)
    di as result "Completed subset `subset': " e(ll)
}
assert abs(likelihoods[16,1]-`full_ll')<1e-5
mata:
    ll=st_matrix("likelihoods")
    shapley=J(4,1,0)
    for (j=0;j<4;j++) {
        for (s=0;s<16;s++) {
            if (mod(floor(s/2^j),2)==1) continue
            k=0
            for (b=0;b<4;b++) k=k+mod(floor(s/2^b),2)
            weight=exp(lngamma(k+1)+lngamma(4-k)-lngamma(5))
            shapley[j+1]=shapley[j+1]+weight*(ll[s+2^j+1]-ll[s+1])
        }
    }
    assert(abs(sum(shapley)-(ll[16]-ll[1]))<1e-6)
    st_matrix("shapley",(shapley,100*shapley/(ll[16]-ll[1])))
end
preserve
    clear
    svmat double shapley
    gen str16 block = ""
    replace block = "Rationality" in 1
    replace block = "Student" in 2
    replace block = "Friendship" in 3
    replace block = "Choice patterns" in 4
    rename (shapley1 shapley2) (likelihood_gain percent)
    export delimited using "`out'/shapley.csv", replace
restore

* Alternative individual and group rationality; hold CEIV calibration fixed.
* Rescale controls to stabilize the Hessian; fitted probabilities are unchanged.
foreach term in `controls' {
    quietly summarize `term'
    if r(sd)>0 replace `term' = (`term'-r(mean))/r(sd)
}
tempname alternatives
tempfile alternative_results
postfile `alternatives' str8 measure byte outcome str7 member double estimate se low high using `alternative_results'
foreach measure in HM MaxMPI {
    preserve
        if "`measure'"=="HM" {
            replace ccei_max_01 = 10*(1-min(hm_i,hm_j)/18)
            replace ccei_min_01 = 10*(1-max(hm_i,hm_j)/18)
            gen byte alternative_category = 1+(hm_g==0)+2*ceiv_at1
        }
        else {
            replace ccei_max_01 = 10*(1-min(maxmpi_i,maxmpi_j))
            replace ccei_min_01 = 10*(1-max(maxmpi_i,maxmpi_j))
            gen byte alternative_category = 1+(maxmpi_g<=1e-9)+2*ceiv_at1
        }
        assert alternative_category==joint_category
        collective_multinomial_ame alternative_category, controls(`controls') base(1) iterations(300)
        assert r(n)==1304 & r(pairs)==652 & r(clusters)==64
        matrix effects = r(effects)
        estimates save "`out'/`measure'.ster", replace
        forvalues category=1/4 {
            forvalues member=1/2 {
                local name = cond(`member'==1,"maximum","minimum")
                local start = 4*(`member'-1)
                post `alternatives' ("`measure'") (`category') ("`name'") ///
                    (effects[`category',`start'+1]) (effects[`category',`start'+2]) ///
                    (effects[`category',`start'+3]) (effects[`category',`start'+4])
            }
        }
    restore
}
postclose `alternatives'
use `alternative_results', clear
export delimited using "`out'/alternative_ame.csv", replace
di as result "SUCCESS: covariate AMEs, exact conditional Shapley decomposition, and alternative-index AMEs validated."
log close
