* Fit Figure 6's four-category model and return validated AMEs for both individual CCEIs.
program define collective_multinomial_ame, rclass
    syntax varname(numeric) [if], CONTROLS(varlist) BASE(integer) ///
        [TECHnique(string) ITERations(integer 150)]
    marksample touse
    local optimizer "difficult iterate(`iterations')"
    if "`technique'"!="" local optimizer "`optimizer' technique(`technique')"
    mlogit `varlist' c.ccei_max_01 c.ccei_min_01 `controls' i.class_fe ///
        if `touse', baseoutcome(`base') vce(cluster class_fe) `optimizer'
    assert e(converged)==1
    local n = e(N)
    local clusters = e(N_clust)
    local ll = e(ll)
    tempvar pairtag
    egen byte `pairtag' = tag(group_id) if e(sample)
    quietly count if `pairtag'==1
    local pairs = r(N)
    tempname effects table
    matrix `effects' = J(4,9,.)
    forvalues category=1/4 {
        quietly count if e(sample) & `varlist'==`category'
        matrix `effects'[`category',9] = r(N)/`n'
        quietly margins, dydx(ccei_max_01 ccei_min_01) predict(outcome(`category'))
        matrix `table' = r(table)
        forvalues member=1/2 {
            local start = 4*(`member'-1)
            foreach element in 1 2 5 6 {
                local column = cond(`element'<=2,`element',`element'-2)+`start'
                matrix `effects'[`category',`column'] = `table'[`element',`member']
                assert !missing(`effects'[`category',`column'])
            }
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
    return scalar ll = `ll'
end
