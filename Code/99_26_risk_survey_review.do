* Run from Code. Explore risk-survey responses, raw distance, group CCEI, and CEIV.
clear all
set more off
local out "results/new_indices/risk_survey"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace

do "programs/load_risk_survey_panel.do"
gen double distance = Ihat_ig
gen byte figure5_sample = !missing(distance)
gen double RA_gap = abs(RA_i - RA_j)
quietly summarize RA_gap if figure5_sample, detail
local median = r(p50)
gen byte figure5_high_gap = figure5_sample & RA_gap >= `median'
quietly count if figure5_sample
assert r(N) == 2560
quietly count if figure5_high_gap
assert r(N) == 1280
di as result "Figure 5 pooled risk-gap median: " %15.12f `median'
foreach q in cooperation similar whose {
    tabulate risk_`q'_i post, missing
}

tempname bars assoc
tempfile means associations
postfile `bars' str11 question str10 sample str8 outcome byte response ///
    double mean se low high percent median_gap long n total clusters using `means'
postfile `assoc' str11 question str10 sample str8 outcome ///
    double pearson pearson_p spearman spearman_p slope slope_se slope_p ///
    adjusted_slope adjusted_se adjusted_p categorical_r2 categorical_p ///
    long n clusters using `associations'

foreach q in cooperation similar whose {
    local survey risk_`q'_i
    local categories = cond("`q'" == "cooperation",5,4)
    foreach outcome in distance ccei_g ceiv_g {
        * Common sample preserves the exact Figure 5 response counts across outcomes.
        foreach sample in common high_gap {
            if "`sample'" == "high_gap" & "`q'" != "similar" continue
            local restriction figure5_sample
            if "`sample'" == "high_gap" local restriction figure5_high_gap
            quietly regress `outcome' ibn.`survey' if `restriction', ///
                noconstant vce(cluster class_fe)
            local total = e(N)
            local clusters = e(N_clust)
            local critical = invttail(e(df_r),0.025)
            forvalues response = 1/`categories' {
                quietly count if e(sample) & `survey' == `response'
                local n = r(N)
                post `bars' ("`q'") ("`sample'") ("`outcome'") (`response') ///
                    (_b[`response'.`survey']) (_se[`response'.`survey']) ///
                    (_b[`response'.`survey']-`critical'*_se[`response'.`survey']) ///
                    (_b[`response'.`survey']+`critical'*_se[`response'.`survey']) ///
                    (100*`n'/`total') (`median') (`n') (`total') (`clusters')
            }
        }

        * All-available correlations; common sample and Figure 5 high-gap checks.
        foreach sample in available common high_gap {
            if "`sample'" == "high_gap" & "`q'" != "similar" continue
            local restriction "!missing(`survey',`outcome')"
            if "`sample'" == "common" local restriction "`restriction' & figure5_sample"
            if "`sample'" == "high_gap" local restriction "`restriction' & figure5_high_gap"
            quietly regress `outcome' i.`survey' if `restriction', vce(cluster class_fe)
            local n = e(N)
            local clusters = e(N_clust)
            local categorical_r2 = e(r2)
            quietly testparm i.`survey'
            local categorical_p = r(p)
            foreach result in pearson pearson_p spearman spearman_p slope slope_se slope_p adjusted_slope adjusted_se adjusted_p {
                local `result' .
            }
            if "`q'" != "whose" {
                quietly correlate `outcome' `survey' if `restriction'
                local pearson = r(rho)
                quietly regress `outcome' c.`survey' if `restriction', vce(cluster class_fe)
                local slope = _b[`survey']
                local slope_se = _se[`survey']
                local slope_p = 2*ttail(e(df_r),abs(`slope'/`slope_se'))
                local pearson_p = `slope_p'
                tempvar outcome_rank survey_rank
                egen double `outcome_rank' = rank(`outcome') if `restriction'
                egen double `survey_rank' = rank(`survey') if `restriction'
                quietly correlate `outcome_rank' `survey_rank'
                local spearman = r(rho)
                quietly regress `outcome_rank' `survey_rank', vce(cluster class_fe)
                local spearman_p = 2*ttail(e(df_r),abs(_b[`survey_rank']/_se[`survey_rank']))
                drop `outcome_rank' `survey_rank'
                quietly regress `outcome' c.`survey' i.post i.class_fe if `restriction', ///
                    vce(cluster class_fe)
                local adjusted_slope = _b[`survey']
                local adjusted_se = _se[`survey']
                local adjusted_p = 2*ttail(e(df_r),abs(`adjusted_slope'/`adjusted_se'))
            }
            post `assoc' ("`q'") ("`sample'") ("`outcome'") ///
                (`pearson') (`pearson_p') (`spearman') (`spearman_p') ///
                (`slope') (`slope_se') (`slope_p') ///
                (`adjusted_slope') (`adjusted_se') (`adjusted_p') ///
                (`categorical_r2') (`categorical_p') (`n') (`clusters')
        }
    }
}
postclose `bars'
postclose `assoc'
preserve
    use `means', clear
    assert !missing(mean,se,low,high)
    assert total == cond(sample=="common",2560,1280)
    bysort question sample outcome: egen long count_check = total(n)
    assert count_check == total
    drop count_check
    export delimited using "`out'/response_means.csv", replace
    use `associations', clear
    assert !missing(categorical_r2,categorical_p,n,clusters)
    assert !missing(pearson,pearson_p,spearman,spearman_p,adjusted_slope) if question!="whose"
    export delimited using "`out'/associations.csv", replace
restore

di as result "SUCCESS: survey ranges and Figure 5 samples validated; all means, intervals, correlations and categorical tests computed with class clustering."
log close
