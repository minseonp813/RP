* Run immediately after 99_1_Tables_Main.do; reuse its pair-wave sample/controls.
* Argument: output CSV path. The split pools both waves and assigns ties low.
args outfile
isid group_id post
assert !missing(RA_dist, ccei_max_01, ccei_min_01, cei_full, ccei_full)
assert abs(RA_dist - abs(RA_i - RA_j)) < 1e-7

quietly summarize RA_dist, detail
local median = r(p50)
quietly count if RA_dist == `median'
di as result "Pooled RA-difference median: " %12.9f `median' "; ties: " r(N)
tempvar ra_high outcome pair_tag class_tag
gen byte `ra_high' = RA_dist > `median'
gen byte `outcome' = 1 + ccei_full + 2 * cei_full
tabulate `ra_high' `outcome'
bysort group_id (post): gen byte `pair_tag' = _n == 1
quietly count if `pair_tag' & `ra_high' != `ra_high'[_n+1]
di as result "Pairs changing RA-difference group across waves: " r(N)

tempname handle
tempfile effects
postfile `handle' byte ra_high outcome str12 member ///
    double estimate se low high median_ra share ///
    long n pairs clusters using `effects'
forvalues split = 0/1 {
    * Equivalent outcome normalizations avoid singular covariance calculations
    * in the sparse subgroup fits. Fitted probabilities and AMEs are invariant.
    local base = cond(`split' == 0, 1, 3)
    mlogit `outcome' c.ccei_max_01 c.ccei_min_01 ///
        $t5_group $t5_friend $t5_share i.class_fe ///
        if `ra_high' == `split', baseoutcome(`base') vce(cluster class_fe) ///
        difficult iterate(100)
    assert e(converged) == 1
    local n = e(N)
    local clusters = e(N_clust)
    quietly count if `ra_high' == `split'
    assert `n' == r(N)
    egen byte `class_tag' = tag(group_id) if e(sample)
    quietly count if `class_tag' == 1
    local pairs = r(N)
    drop `class_tag'
    forvalues category = 1/4 {
        quietly count if e(sample) & `outcome' == `category'
        local share = r(N) / `n'
        margins, dydx(ccei_max_01 ccei_min_01) predict(outcome(`category'))
        matrix ame = r(table)
        forvalues col = 1/2 {
            local member = cond(`col' == 1, "maximum", "minimum")
            post `handle' (`split') (`category') ("`member'") ///
                (ame[1,`col']) (ame[2,`col']) (ame[5,`col']) (ame[6,`col']) ///
                (`median') (`share') (`n') (`pairs') (`clusters')
        }
    }
}
postclose `handle'
preserve
    use `effects', clear
    bysort ra_high member: egen double ame_sum = total(estimate)
    assert abs(ame_sum) < 1e-8
    assert !missing(estimate, se, low, high)
    drop ame_sum
    export delimited using `"`outfile'"', replace
restore
