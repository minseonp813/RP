* Appendix mover-status regressions, matching Table 3's six specifications.
* Run from Code. No RA controls; corner/equal-allocation shares use exact choices.
set more off
capture mkdir "results"
capture mkdir "results/tables"
capture log close appendix_mover
log using "results/tables/appendix_mover.log", name(appendix_mover) text replace

do "programs/prepare_table3_controls.do"
keep if balanced_t3
assert _N == 2512
bysort id (post): assert _N == 2 & mover == mover[1]
assert inlist(mover, 0, 1)
gen double higher_mover = HighCCEI_both_high * mover
gen double gap_mover = ccei_gap_ij * mover
gen double centered_gap = ccei_gap_ij * (mover - .5)

local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local gender "female_i_male_j male_i_female_j"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"

label var HighCCEI_both_high "\$Higher CCEI_i\$"
label var ccei_gap_ij "\$CCEI_i-CCEI_j\$"
label var mover "\$Mover_i\$"
label var higher_mover "\$Higher CCEI_i \times Mover_i\$"
label var gap_mover "\$(CCEI_i-CCEI_j) \times Mover_i\$"
label var mathscore_i "\$Math score_i\$"
label var mathscore_diff "\$Math score_{diff}\$"
label var female_i_male_j "\$(Female_i, Male_j)\$"
label var male_i_female_j "\$(Male_i, Female_j)\$"
label var inclass_popularity_i "\$In-degree_i\$"
label var inclass_pop_diff "\$In-degree_{diff}\$"

* Stata requires an e-class program to repost the equivalent covariance.
capture program drop mover_gap_class_fe
program define mover_gap_class_fe, eclass
    * Pair symmetry makes this interaction and its variance exactly zero.
    * Centering avoids numerical failure in the otherwise identical model.
    quietly reghdfe Ihat_ig ccei_gap_ij mover centered_gap, ///
        absorb(class) vce(cluster class)
    tempname b V T
    matrix `b' = e(b)
    matrix `V' = e(V)
    matrix `T' = I(colsof(`b'))
    matrix `T'[1,3] = -.5
    matrix `b' = `b' * `T''
    matrix `V' = `T' * `V' * `T''
    assert abs(`b'[1,3]) < 1e-12 & abs(`V'[3,3]) < 1e-24
    matrix `b'[1,3] = 0
    forvalues term = 1/4 {
        matrix `V'[3,`term'] = 0
        matrix `V'[`term',3] = 0
    }
    matrix colnames `b' = ccei_gap_ij mover gap_mover _cons
    matrix colnames `V' = ccei_gap_ij mover gap_mover _cons
    matrix rownames `V' = ccei_gap_ij mover gap_mover _cons
    ereturn repost b=`b' V=`V', rename
end

eststo clear
forvalues column = 1/6 {
    local focal "HighCCEI_both_high"
    local interaction "higher_mover"
    if `column' > 3 {
        local focal "ccei_gap_ij"
        local interaction "gap_mover"
    }
    local specification = mod(`column' - 1, 3) + 1
    local controls ""
    local fixed_effect "class"
    local fixed_label "Class"
    if `specification' == 2 local controls "`individual' `gender' `friendship' `missing_controls' `shares'"
    if `specification' == 3 {
        local controls "`individual' `friendship' `missing_controls' `shares'"
        local fixed_effect "id_fe"
        local fixed_label "Individual"
    }

    * Verify the underlying model against the current Table 3 before augmentation.
    quietly reghdfe Ihat_ig `focal' `controls', absorb(`fixed_effect') vce(cluster class)
    local expected "-.250 -.243 -.233 -.802 -.794 -.751"
    local target : word `column' of `expected'
    assert abs(_b[`focal'] - `target') < .0005

    if `column' == 4 {
        mover_gap_class_fe
        estimates replay
    }
    else reghdfe Ihat_ig `focal' mover `interaction' `controls', ///
        absorb(`fixed_effect') vce(cluster class)
    eststo m`column'
    assert e(sample) == 1
    assert e(N) == 2512 & e(N_clust) == 64
    if `specification' == 3 assert _b[mover] == 0 & _se[mover] == 0
    estadd local fixed_effects "`fixed_label'" : m`column'
    if `specification' > 1 {
        estadd local individual_controls "\checkmark" : m`column'
        estadd local friendship_controls "\checkmark" : m`column'
        estadd local share_controls "\checkmark" : m`column'
    }
}

esttab m1 m2 m3 m4 m5 m6 using "results/tables/table_bargainingCCEI_mover.tex", replace ///
    b(3) se(3) stats(N r2 fixed_effects individual_controls friendship_controls share_controls, ///
    labels("N" "R-squared" "Fixed effects" "Other individual characteristics" ///
    "Other friendship characteristics" "Corner/midpoint share controls") ///
    fmt(0 3 %9s %9s %9s %9s)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label substitute(\_ _) noomitted ///
    keep(HighCCEI_both_high ccei_gap_ij mover higher_mover gap_mover mathscore_i mathscore_diff ///
    female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff) ///
    order(HighCCEI_both_high ccei_gap_ij mover higher_mover gap_mover mathscore_i mathscore_diff ///
    female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff) ///
    nomtitles fragment nonumbers nolines prefoot("\hline") postfoot("\hline \bottomrule")

* Mark structural-zero inference as unavailable, rather than a missing-value SE.
tempfile formatted
filefilter "results/tables/table_bargainingCCEI_mover.tex" `formatted', ///
    from("(.)") to("---") replace
copy `formatted' "results/tables/table_bargainingCCEI_mover.tex", replace
log close appendix_mover
