* Table 4: actual, placebo, and normalized distance in Table 3's six specifications.
* Run from Code. Reuse the existing all-other-pairs placebo benchmarks.
set more off
capture log close table4
log using "results/placebo_normalized/table4_regressions.log", name(table4) text replace

tempfile placebo
use "results/placebo_normalized/placebo_normalized_member_wave.dta", clear
tostring group_id, replace format(%14.0f)
tostring id, replace format(%7.0f)
keep group_id post id I_actual M_all_imp n_all
rename I_actual I_cached
isid group_id post id
save `placebo'

do "programs/prepare_table3_controls.do"
merge 1:1 group_id post id using `placebo', assert(match) nogen
assert missing(I_cached) == missing(Ihat_ig)
assert abs(I_cached - Ihat_ig) < 1e-4 if !missing(Ihat_ig)
assert n_all == 651 & !missing(M_all_imp)
* Use Table 3's actual distance exactly; cached diagonals differ by solver tolerance.
gen double I_actual = Ihat_ig
gen double I_normalized = I_actual - M_all_imp
keep if balanced_t3
assert _N == 2512
bysort group_id post: assert _N == 2
bysort group_id post: assert abs(M_all_imp[1] + M_all_imp[2] - 1) < 1e-7
bysort group_id post: assert abs(I_normalized[1] + I_normalized[2]) < 1e-7

local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local gender "female_i_male_j male_i_female_j"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"
label var HighCCEI_both_high "\$Higher CCEI_i\$"
label var ccei_gap_ij "\$CCEI_i-CCEI_j\$"

local output "results/tables/placebo_normalized/placebo_normalized_both_high.tex"
local write_mode "replace"
matrix coefficients = J(4,6,.)
forvalues panel = 1/4 {
    if `panel' == 1 {
        local outcome "I_actual"
        local title "Panel A: Actual revealed-preference distance, \ensuremath{I_{ig}}"
    }
    if `panel' == 2 {
        local outcome "M_all_imp"
        local title "Panel B: Mean placebo benchmark, \ensuremath{M_{ig}}"
    }
    if `panel' == 3 {
        local outcome "I_normalized"
        local title "Panel C: Placebo-normalized distance, \ensuremath{I^*_{ig}=I_{ig}-M_{ig}}"
    }
    if `panel' == 4 {
        local outcome "I_actual"
        local title "Panel D: Actual distance controlling for placebo benchmark, \ensuremath{I_{ig}|M_{ig}}"
    }
    eststo clear
    forvalues column = 1/6 {
        local focal "HighCCEI_both_high"
        if `column' > 3 local focal "ccei_gap_ij"
        local specification = mod(`column' - 1, 3) + 1
        local controls ""
        local fixed_effect "class"
        if `specification' == 2 local controls "`individual' `gender' `friendship' `missing_controls' `shares'"
        if `specification' == 3 {
            local controls "`individual' `friendship' `missing_controls' `shares'"
            local fixed_effect "id_fe"
        }
        if `panel' == 4 {
            local controls "`controls' M_all_imp"
        }
        eststo m`column': reghdfe `outcome' `focal' `controls', ///
            absorb(`fixed_effect') vce(cluster class)
        assert e(N) == 2512 & e(N_clust) == 64
        assert e(sample) == 1
        matrix coefficients[`panel',`column'] = _b[`focal']
        if `panel' == 1 {
            local expected "-.250 -.243 -.233 -.802 -.794 -.751"
            local target : word `column' of `expected'
            assert abs(_b[`focal'] - `target') < .0005
        }
        display "RESULT panel `panel' column `column': b=" _b[`focal'] ///
            " se=" _se[`focal'] " p=" 2 * ttail(e(df_r), abs(_b[`focal'] / _se[`focal']))
    }
    esttab m1 m2 m3 m4 m5 m6 using "`output'", `write_mode' ///
        b(3) se(3) stats(r2, labels("R-squared") fmt(3)) ///
        nogap compress star(+ 0.1 * 0.05 ** 0.01) label substitute(\_ _) ///
        keep(HighCCEI_both_high ccei_gap_ij) order(HighCCEI_both_high ccei_gap_ij) ///
        nomtitles fragment nonumbers nolines ///
        prehead("\multicolumn{7}{l}{\emph{`title'}} \\") ///
        prefoot("\addlinespace[2pt]") postfoot("\midrule")
    local write_mode "append"
}
forvalues column = 1/6 {
    assert abs(coefficients[3,`column'] - (coefficients[1,`column'] - coefficients[2,`column'])) < 1e-10
}
file open footer using "`output'", write append
file write footer "Observations & 2512 & 2512 & 2512 & 2512 & 2512 & 2512 \\" _n
file write footer "Fixed effects & Class & Class & Individual & Class & Class & Individual \\" _n
file write footer "Individual characteristics & & \checkmark & \checkmark & & \checkmark & \checkmark \\" _n
file write footer "Friendship characteristics & & \checkmark & \checkmark & & \checkmark & \checkmark \\" _n
file write footer "Corner/midpoint share controls & & \checkmark & \checkmark & & \checkmark & \checkmark \\" _n
file write footer "\bottomrule" _n
file close footer
log close table4
