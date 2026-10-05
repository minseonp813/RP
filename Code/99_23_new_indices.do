* Run from Code. Separate review outputs; no draft files are changed.
clear all
set more off
set matsize 8000
capture log close
log using "results/new_indices/analysis.log", text replace
local data_dir "`c(pwd)'/data"
local out "results/new_indices"

* Preserve exact string identifiers: group IDs exceed float precision.
import excel "../CEI_CEIV_CEIC_statistics.xlsx", sheet("Group results") firstrow allstring clear
keep group_id post member_i_id member_j_id CEI CEIV CEIC ccei_i ccei_j ccei_group CEIC_lower CEIC_upper CEIV_lower CEIV_upper CEIC_status CEIV_status CEIC_precision_met CEIV_precision_met
rename (ccei_i ccei_j ccei_group CEI CEIV CEIC) (xlsx_ccei_i xlsx_ccei_j xlsx_ccei_g xlsx_cei ceiv_g ceic_g)
destring post xlsx_* ceiv_g ceic_g CEIC_lower CEIC_upper CEIV_lower CEIV_upper CEIC_precision_met CEIV_precision_met, replace
isid group_id post
assert _N == 1304
assert member_i_id != member_j_id
assert !missing(group_id, member_i_id, member_j_id)
assert !missing(ceiv_g, ceic_g)
assert inrange(ceiv_g,0,1) & inrange(ceic_g,0,1)
* Keep the supplied unresolved midpoint and expose its interval in the audit.
list group_id post ceic_g CEIC_lower CEIC_upper CEIC_status if CEIC_precision_met != 1, noobs
assert CEIV_precision_met == 1
assert CEIC_lower <= ceic_g & ceic_g <= CEIC_upper
assert CEIV_lower <= ceiv_g & ceiv_g <= CEIV_upper
tempfile indices
save `indices'

* Validate all student-wave observations before collapsing to pair-waves.
use "`data_dir'/panel_individual.dta", clear
isid id post
local original_n = _N
merge m:1 group_id post using `indices', assert(match) nogen
assert _N == `original_n'
assert (id == member_i_id & partner_id == member_j_id) | (id == member_j_id & partner_id == member_i_id)
assert abs(ccei_i - xlsx_ccei_i) < 1e-7 if id == member_i_id
assert abs(ccei_i - xlsx_ccei_j) < 1e-7 if id == member_j_id
assert abs(ccei_j - xlsx_ccei_j) < 1e-7 if partner_id == member_j_id
assert abs(ccei_j - xlsx_ccei_i) < 1e-7 if partner_id == member_i_id
assert abs(ccei_g - xlsx_ccei_g) < 1e-7
assert abs(cei_g - xlsx_cei) < 1e-7
bysort group_id post: assert _N == 2
save "`data_dir'/panel_individual_new_indices.dta", replace

* The same checks apply to the group-level main panel.
use "`data_dir'/panel_group.dta", clear
isid group_id post
merge 1:1 group_id post using `indices', assert(match) nogen
assert id_mover == member_i_id & id_nonmover == member_j_id
save "`data_dir'/panel_group_new_indices.dta", replace

* Enrich the wide main panel as well, validating mover/nonmover IDs by wave.
use `indices', clear
keep group_id post member_i_id member_j_id ceiv_g ceic_g
reshape wide member_i_id member_j_id ceiv_g ceic_g, i(group_id) j(post)
rename (ceiv_g0 ceiv_g1 ceic_g0 ceic_g1) (ceiv_g_base ceiv_g_end ceic_g_base ceic_g_end)
tempfile wide_indices
save `wide_indices'
use "`data_dir'/panel_final.dta", clear
isid group_id
merge 1:1 group_id using `wide_indices', assert(match) nogen
assert _N == 652
assert id_mover_base == member_i_id0 & id_nonmover_base == member_j_id0
assert id_mover_end == member_i_id1 & id_nonmover_end == member_j_id1
drop member_i_id0 member_i_id1 member_j_id0 member_j_id1
save "`data_dir'/panel_final_new_indices.dta", replace

do "programs/prepare_collective_sample.do" "`data_dir'"
merge 1:1 group_id post using `indices', assert(match) nogen
assert _N == 1304
bysort group_id: assert _N == 2
assert (id == member_i_id & partner_id == member_j_id) | (id == member_j_id & partner_id == member_i_id)
gen double ccei_min = ccei_max - ccei_dist
label var ccei_max "\$\text{CCEI}_{\text{max},gt}\$"
label var ccei_min "\$\text{CCEI}_{\text{min},gt}\$"
save "`out'/analysis_sample.dta", replace

* Table 5: two additional OLS panels, identical controls and inference.
eststo clear
foreach outcome in ceiv ceic {
    forvalues spec = 1/4 {
        local controls ""
        if `spec' >= 2 local controls "$t5_group $t5_friend"
        if `spec' >= 3 local controls "`controls' $t5_share"
        local fe class_fe
        if `spec' == 4 local fe pair_fe
        eststo `outcome'`spec': reghdfe `outcome'_g ccei_max ccei_min `controls', absorb(`fe') vce(cluster class_fe)
        assert e(N) == 1304
        assert e(N_clust) == 64
        estimates save "`out'/table5_`outcome'_`spec'.ster", replace
    }
    local mode replace
    local panel C
    if "`outcome'" == "ceic" {
        local mode append
        local panel D
    }
    local title = upper("`outcome'")
    esttab `outcome'1 `outcome'2 `outcome'3 `outcome'4 using "`out'/table5_new_panels.tex", `mode' ///
        b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
        nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
        keep(ccei_max ccei_min) order(ccei_max ccei_min) ///
        fragment nomtitles nonumbers nolines substitute(\_ _) ///
        prehead("\midrule" "\multicolumn{5}{l}{\textit{Panel `panel': Group `title' -- OLS}}\\") ///
        prefoot("\midrule") postfoot("")
}

* Figure 6: preserve the 1e-9 threshold and scaled average derivatives.
gen byte ccei_one = ccei_g >= 1-1e-9
gen byte ceiv_one = ceiv_g >= 1-1e-9
gen byte ceic_one = ceic_g >= 1-1e-9
* Ensure the omitted fourth category in classification B is truly empty.
assert !(ceiv_one == 0 & ceic_one == 1)
* Audit interval-sensitive classifications without changing supplied indices.
assert ceiv_one == (CEIV_lower >= 1-1e-9)
assert ceiv_one == (CEIV_upper >= 1-1e-9)
assert ceic_one == (CEIC_lower >= 1-1e-9)
count if ceic_one != (CEIC_upper >= 1-1e-9)
preserve
    keep if CEIC_precision_met != 1 | ceic_one != (CEIC_upper >= 1-1e-9)
    keep group_id post member_i_id member_j_id ceic_g CEIC_lower CEIC_upper CEIC_status CEIC_precision_met
    export delimited using "`out'/precision_audit.csv", replace
restore
gen byte class_a = 1 + ccei_one + 2*ceiv_one
gen byte class_b = 1 + ceiv_one + ceic_one
gen double ccei_max_01 = 10*ccei_max
gen double ccei_min_01 = 10*ccei_min
foreach kind in a b {
    tabulate class_`kind' post
    mlogit class_`kind' c.ccei_max_01 c.ccei_min_01 $t5_group $t5_friend $t5_share i.class_fe, baseoutcome(1) vce(cluster class_fe)
    assert e(converged) == 1
    assert e(N) == 1304
    assert e(N_clust) == 64
    estimates save "`out'/figure6_`kind'.ster", replace
    local n = e(N)
    local clusters = e(N_clust)
    local categories = cond("`kind'" == "a", 4, 3)
    tempname handle
    tempfile effects
    postfile `handle' byte outcome str7 member double estimate se low high share long n pairs clusters using `effects'
    forvalues category = 1/`categories' {
        quietly count if e(sample) & class_`kind' == `category'
        local share = r(N)/`n'
        margins, dydx(ccei_max_01 ccei_min_01) predict(outcome(`category'))
        matrix ame = r(table)
        forvalues col = 1/2 {
            local member = cond(`col' == 1, "maximum", "minimum")
            post `handle' (`category') ("`member'") (ame[1,`col']) (ame[2,`col']) (ame[5,`col']) (ame[6,`col']) (`share') (`n') (652) (`clusters')
        }
    }
    postclose `handle'
    preserve
        use `effects', clear
        bysort member: egen double ame_sum = total(estimate)
        assert abs(ame_sum) < 1e-8
        assert !missing(estimate,se,low,high)
        drop ame_sum
        export delimited using "`out'/figure6_`kind'_ame.csv", replace
    restore
}
* Fractional-response companion table.
do "99_24_new_indices_fractional.do"

di as result "SUCCESS: all merges, member checks, estimation samples, and AME checks passed."
log close
