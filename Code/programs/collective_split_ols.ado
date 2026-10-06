* Subgroup Table 5(3) slopes and fully interacted, class-clustered difference tests.
program define collective_split_ols
    syntax varname(numeric), NAME(name) OUTDIR(string) CONTROLS(varlist)
    local split `varlist'
    quietly levelsof `split' if !missing(`split'), local(groups)
    local base : word 1 of `groups'
    quietly count if !missing(`split')
    local eligible = r(N)
    tempname coeff tests
    tempfile coefficient_data test_data
    postfile `coeff' str13 outcome str8 sample int group str3 member ///
        double estimate se low high p long n clusters eligible using `coefficient_data'
    postfile `tests' str13 outcome str8 test str3 member int higher lower ///
        double estimate se low high p long n clusters using `test_data'

    foreach outcome in ccei_g ceiv_g ccei_ceiv_gap {
        quietly reghdfe `outcome' ccei_max ccei_min `controls' if !missing(`split'), ///
            absorb(class_fe) vce(cluster class_fe)
        foreach member in max min {
            local b = _b[ccei_`member']
            local se = _se[ccei_`member']
            local critical = invttail(e(df_r),0.025)
            post `coeff' ("`outcome'") ("pooled") (.) ("`member'") ///
                (`b') (`se') (`b'-`critical'*`se') (`b'+`critical'*`se') ///
                (2*ttail(e(df_r),abs(`b'/`se'))) (e(N)) (e(N_clust)) (`eligible')
        }
        foreach g of local groups {
            quietly reghdfe `outcome' ccei_max ccei_min `controls' if `split'==`g', ///
                absorb(class_fe) vce(cluster class_fe)
            local critical = invttail(e(df_r),0.025)
            quietly count if `split'==`g'
            local group_eligible = r(N)
            foreach member in max min {
                local b = _b[ccei_`member']
                local se = _se[ccei_`member']
                local slope_`member'_`g' = `b'
                post `coeff' ("`outcome'") ("subgroup") (`g') ("`member'") ///
                    (`b') (`se') (`b'-`critical'*`se') (`b'+`critical'*`se') ///
                    (2*ttail(e(df_r),abs(`b'/`se'))) (e(N)) (e(N_clust)) (`group_eligible')
            }
        }
        * Fully interact controls and class effects, reproducing each subgroup's slopes.
        quietly regress `outcome' ib`base'.`split'##c.(ccei_max ccei_min `controls') ///
            ib`base'.`split'##i.class_fe if !missing(`split'), vce(cluster class_fe)
        local model_n = e(N)
        local clusters = e(N_clust)
        foreach member in max min {
            quietly testparm i.`split'#c.ccei_`member'
            post `tests' ("`outcome'") ("omnibus") ("`member'") (.) (.) ///
                (.) (.) (.) (.) (r(p)) (`model_n') (`clusters')
            foreach hi of local groups {
                foreach lo of local groups {
                    if `hi' <= `lo' continue
                    local contrast "`hi'.`split'#c.ccei_`member'"
                    if `lo' != `base' local contrast "`contrast'-`lo'.`split'#c.ccei_`member'"
                    quietly lincom `contrast'
                    assert abs(r(estimate)-(`slope_`member'_`hi''-`slope_`member'_`lo'')) < 1e-7
                    post `tests' ("`outcome'") ("pairwise") ("`member'") (`hi') (`lo') ///
                        (r(estimate)) (r(se)) (r(lb)) (r(ub)) (r(p)) (`model_n') (`clusters')
                }
            }
        }
    }
    postclose `coeff'
    postclose `tests'
    preserve
        keep if !missing(`split')
        tempvar full_ccei full_ceiv
        gen byte `full_ccei' = ccei_g >= 1-1e-9
        gen byte `full_ceiv' = ceiv_g >= 1-1e-9
        collapse (count) n=ccei_min (mean) mean_min=ccei_min mean_max=ccei_max ///
            mean_ccei=ccei_g mean_ceiv=ceiv_g full_ccei_share=`full_ccei' ///
            full_ceiv_share=`full_ceiv' (sd) sd_min=ccei_min, by(`split')
        rename `split' group
        export delimited using "`outdir'/`name'_diagnostics.csv", replace
    restore
    preserve
        use `coefficient_data', clear
        assert !missing(estimate,se,low,high,p,n,clusters,eligible)
        sort outcome sample group member
        export delimited using "`outdir'/`name'_coefficients.csv", replace
        use `test_data', clear
        assert !missing(p,n,clusters)
        assert !missing(estimate,se,low,high) if test=="pairwise"
        sort outcome member test higher lower
        export delimited using "`outdir'/`name'_tests.csv", replace
    restore
end
