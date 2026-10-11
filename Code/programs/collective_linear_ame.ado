* Four separate LPMs; return effects in the same layout as collective_multinomial_ame.
program define collective_linear_ame, rclass
    syntax varname(numeric) [if], [CONTROLS(varlist) FE(varname)]
    marksample touse
    if "`fe'"=="" local fe class_fe
    tempvar outcome_indicator pairtag
    tempname effects
    matrix `effects' = J(4,9,.)
    gen byte `outcome_indicator' = .
    forvalues category=1/4 {
        quietly replace `outcome_indicator' = `varlist'==`category' if `touse'
        reghdfe `outcome_indicator' ccei_max_01 ccei_min_01 `controls' if `touse', ///
            absorb(`fe') vce(cluster class_fe)
        if `category'==1 {
            local n = e(N)
            local clusters = e(N_clust)
            egen byte `pairtag' = tag(group_id) if e(sample)
            quietly count if `pairtag'==1
            local pairs = r(N)
        }
        assert e(N)==`n' & e(N_clust)==`clusters' & e(df_r)==`clusters'-1
        quietly count if e(sample) & `varlist'==`category'
        matrix `effects'[`category',9] = r(N)/`n'
        forvalues member=1/2 {
            local x = cond(`member'==1,"ccei_max_01","ccei_min_01")
            local start = 4*(`member'-1)
            quietly lincom `x'
            matrix `effects'[`category',`start'+1] = r(estimate)
            matrix `effects'[`category',`start'+2] = r(se)
            matrix `effects'[`category',`start'+3] = r(lb)
            matrix `effects'[`category',`start'+4] = r(ub)
        }
    }
    forvalues member=1/2 {
        local column = 4*(`member'-1)+1
        assert abs(`effects'[1,`column']+`effects'[2,`column']+`effects'[3,`column']+`effects'[4,`column'])<1e-8
    }
    return matrix effects = `effects'
    return scalar n = `n'
    return scalar pairs = `pairs'
    return scalar clusters = `clusters'
end
