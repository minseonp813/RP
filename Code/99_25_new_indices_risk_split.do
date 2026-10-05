* Run from Code. Table 5 column (3) and Figure 6, split at each wave's RA-gap median.
clear all
set more off
local out "results/new_indices/risk_split"
cap mkdir "`out'"
capture log close
log using "`out'/analysis.log", text replace

do "programs/prepare_collective_sample.do" "`c(pwd)'/data"
merge 1:1 group_id post using "data/panel_group_new_indices.dta", ///
    keepusing(ceiv_g CEIV_lower CEIV_upper) assert(match) nogen
assert _N == 1304
assert !missing(RA_i, RA_j, ccei_g, ceiv_g)
assert abs(RA_dist - abs(RA_i - RA_j)) < 1e-7
gen double ccei_min = ccei_max - ccei_dist
label var ccei_max "$\text{CCEI}_{\text{max},gt}$"
label var ccei_min "$\text{CCEI}_{\text{min},gt}$"
egen double median_ra_wave = median(RA_dist), by(post)
assert RA_dist != median_ra_wave
gen byte ra_high = RA_dist > median_ra_wave
label define ra_split 0 "Below wave median" 1 "Above wave median"
label values ra_high ra_split
tabulate post ra_high
local controls "$t5_group $t5_friend $t5_share"

preserve
    collapse (count) n=RA_dist (min) median_ra=median_ra_wave, by(post ra_high)
    assert n == 326
    export delimited using "`out'/split_summary.csv", replace
restore

* Include the pooled column (3) as a reference beside the two subgroup estimates.
eststo clear
tempname coeff tests
tempfile coefficients differences
postfile `coeff' str4 outcome str9 sample str8 member double estimate se p ///
    long n clusters using `coefficients'
postfile `tests' str4 outcome str8 member double difference se p using `differences'
foreach outcome in ccei ceiv {
    foreach sample in pooled similar different {
        local restriction ""
        if "`sample'" == "similar" local restriction "if ra_high == 0"
        if "`sample'" == "different" local restriction "if ra_high == 1"
        eststo `outcome'_`sample': reghdfe `outcome'_g ccei_max ccei_min ///
            `controls' `restriction', absorb(class_fe) vce(cluster class_fe)
        assert e(N) == cond("`sample'" == "pooled", 1304, 652)
        estimates save "`out'/table5_`outcome'_`sample'.ster", replace
        foreach member in max min {
            post `coeff' ("`outcome'") ("`sample'") ("`member'") ///
                (_b[ccei_`member']) (_se[ccei_`member']) ///
                (2*ttail(e(df_r),abs(_b[ccei_`member']/_se[ccei_`member']))) ///
                (e(N)) (e(N_clust))
        }
    }
    local mode replace
    if "`outcome'" == "ceiv" local mode append
    local title = upper("`outcome'")
    esttab `outcome'_pooled `outcome'_similar `outcome'_different ///
        using "`out'/table5_split.tex", `mode' b(3) se(3) ///
        stats(N r2, labels("N" "R-squared") fmt(0 3)) nogap compress ///
        star(+ 0.1 * 0.05 ** 0.01) label keep(ccei_max ccei_min) ///
        order(ccei_max ccei_min) fragment nomtitles nonumbers nolines substitute(\_ _) ///
        prehead("\midrule" "\multicolumn{4}{l}{\textit{Group `title' -- OLS}}\\") ///
        prefoot("\midrule") postfoot("")

    * Fully interact controls and class effects to test differences between subgroup slopes.
    quietly regress `outcome'_g i.ra_high##c.(ccei_max ccei_min `controls') ///
        i.ra_high##i.class_fe, vce(cluster class_fe)
    assert e(N) == 1304
    foreach member in max min {
        post `tests' ("`outcome'") ("`member'") ///
            (_b[1.ra_high#c.ccei_`member']) (_se[1.ra_high#c.ccei_`member']) ///
            (2*ttail(e(df_r),abs(_b[1.ra_high#c.ccei_`member']/_se[1.ra_high#c.ccei_`member'])))
    }
}
postclose `coeff'
postclose `tests'
preserve
    use `coefficients', clear
    export delimited using "`out'/table5_coefficients.csv", replace
    use `differences', clear
    export delimited using "`out'/slope_differences.csv", replace
restore

replace ccei_full = ccei_g >= 1-1e-9
gen byte ceiv_full = ceiv_g >= 1-1e-9
assert ceiv_full == (CEIV_lower >= 1-1e-9)
assert ceiv_full == (CEIV_upper >= 1-1e-9)
gen double ccei_max_01 = 10*ccei_max
gen double ccei_min_01 = 10*ccei_min
do "programs/cei_ame_ra_split.do" "`out'/figure6_ame.csv" ceiv_full ra_high "`out'"

di as result "SUCCESS: wave-median split validated; all six OLS and both multinomial models use complete samples; finite AMEs sum to zero."
log close
