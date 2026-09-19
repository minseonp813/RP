* Table 3 choice-buffer variants and appendix table. Run from Code.
* Outputs stay in results; copy the appendix fragment to Overleaf when reviewed.
clear all
set more off

local data_dir "data"
local out_dir "results/table3_choice_buffers"
capture mkdir "results"
capture mkdir "`out_dir'"
capture mkdir "results/tables"
capture log close
log using "`out_dir'/estimation.log", text replace

* Reuse Table 3's raw-choice definitions, adding inclusive payoff-share buffers.
tempfile baseline_shares choice_shares partner_shares coefficients rates
foreach wave in base end {
    use "`data_dir'/`wave'_raw.dta", clear
    keep if game_type == 1
    gen byte post = ("`wave'" == "end")
    assert !missing(coord_x, coord_y) & coord_x >= 0 & coord_y >= 0
    assert coord_x + coord_y > 0
    bysort id: assert _N == 18
    gen byte corner_0 = (coord_x == 0 | coord_y == 0)
    gen byte mid_0 = (coord_x == coord_y)
    * Integer multipliers include exact boundaries without floating-point drift.
    gen byte corner_025 = 40 * min(coord_x, coord_y) <= coord_x + coord_y
    gen byte mid_025 = 20 * abs(coord_x - coord_y) <= coord_x + coord_y
    gen byte corner_05 = 20 * min(coord_x, coord_y) <= coord_x + coord_y
    gen byte mid_05 = 10 * abs(coord_x - coord_y) <= coord_x + coord_y
    assert corner_0 <= corner_025 & corner_025 <= corner_05
    assert mid_0 <= mid_025 & mid_025 <= mid_05
    collapse (mean) corner_* mid_*, by(id post)
    if "`wave'" == "base" save `baseline_shares'
}
append using `baseline_shares'
isid id post
save `choice_shares'
rename id partner_id
save `partner_shares'

use "`data_dir'/panel_individual.dta", clear
isid id post
bysort id: egen n_distance = total(!missing(Ihat_ig))
keep if n_distance == 2
assert _N == 2512
bysort id: assert _N == 2
drop n_distance
egen long id_fe = group(id)

merge 1:1 id post using `choice_shares', keep(master match) assert(match using) nogen
foreach suffix in 0 025 05 {
    rename corner_`suffix' corner_`suffix'_i
    rename mid_`suffix' mid_`suffix'_i
}
merge m:1 partner_id post using `partner_shares', keep(master match) assert(match using) nogen
foreach suffix in 0 025 05 {
    rename corner_`suffix' corner_`suffix'_j
    rename mid_`suffix' mid_`suffix'_j
}
capture drop female_i_male_j male_i_female_j
gen byte female_i_male_j = (male_i == 0 & male_j == 1)
gen byte male_i_female_j = (male_i == 1 & male_j == 0)

* Same control lists, sample, and class clustering as 99_1_Tables_Main.do.
local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local gender "female_i_male_j male_i_female_j"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"

label var HighCCEI_both_high "Higher CCEI"
label var ccei_gap_ij "CCEI difference"
label var mathscore_i "Math score"
label var mathscore_diff "Math-score difference"
label var female_i_male_j "Female member, male partner"
label var male_i_female_j "Male member, female partner"
label var inclass_popularity_i "In-degree"
label var inclass_pop_diff "In-degree difference"

postfile coefficient_file str16 variant double buffer_pp byte column ///
    str20 focal double beta se p N r2 df clusters using `coefficients'
postfile rate_file str16 variant double buffer_pp corner_share midpoint_share N using `rates'

foreach variant in original no_buffer small_buffer generous_buffer {
    local suffix "0"
    local buffer_pp = 0
    if "`variant'" == "small_buffer" {
        local suffix "025"
        local buffer_pp = 2.5
    }
    if "`variant'" == "generous_buffer" {
        local suffix "05"
        local buffer_pp = 5
    }
    capture drop corner_share_i corner_share_diff mid_share_i mid_share_diff
    gen corner_share_i = corner_`suffix'_i
    gen corner_share_diff = corner_`suffix'_i - corner_`suffix'_j
    gen mid_share_i = mid_`suffix'_i
    gen mid_share_diff = mid_`suffix'_i - mid_`suffix'_j
    label var corner_share_i "Corner-choice share"
    label var corner_share_diff "Corner-share difference"
    label var mid_share_i "Equal-allocation share"
    label var mid_share_diff "Equal-allocation share difference"

    quietly summarize corner_share_i
    local corner_mean = r(mean)
    quietly summarize mid_share_i
    post rate_file ("`variant'") (`buffer_pp') (`corner_mean') (r(mean)) (r(N))

    local ra ""
    if "`variant'" == "original" local ra "RA_i RA_diff"
    eststo clear
    forvalues column = 1/6 {
        local focal "HighCCEI_both_high"
        if `column' > 3 local focal "ccei_gap_ij"
        local specification = mod(`column' - 1, 3) + 1
        local controls ""
        local fixed_effect "class"
        local fixed_label "Class"
        if `specification' == 2 local controls "`individual' `gender' `friendship' `missing_controls' `ra' `shares'"
        if `specification' == 3 {
            local controls "`individual' `friendship' `missing_controls' `ra' `shares'"
            local fixed_effect "id_fe"
            local fixed_label "Individual"
        }
        quietly eststo m`column': reghdfe Ihat_ig `focal' `controls', ///
            absorb(`fixed_effect') vce(cluster class)
        assert e(sample) == 1
        assert e(N) == 2512 & e(N_clust) == 64
        if "`variant'" == "original" {
            local expected "-.250 -.221 -.217 -.802 -.712 -.686"
            local target : word `column' of `expected'
            assert abs(_b[`focal'] - `target') < .0005
        }
        post coefficient_file ("`variant'") (`buffer_pp') (`column') ///
            ("`focal'") (_b[`focal']) (_se[`focal']) ///
            (2 * ttail(e(df_r), abs(_b[`focal'] / _se[`focal']))) ///
            (e(N)) (e(r2)) (e(df_r)) (e(N_clust))
        estadd local fixed_effects "`fixed_label'" : m`column'
        if `specification' > 1 {
            estadd local other_controls "Yes" : m`column'
            estadd local choice_controls "Yes" : m`column'
            if "`variant'" == "original" estadd local ra_controls "Yes" : m`column'
        }
    }
    esttab m1 m2 m3 m4 m5 m6 using "`out_dir'/table3_`variant'.tex", replace ///
        b(3) se(3) stats(N r2 fixed_effects other_controls ra_controls choice_controls, ///
        labels("N" "R-squared" "Fixed effects" "Other individual/friendship controls" ///
        "RA and RA-difference controls" "Corner/equal-allocation share controls") ///
        fmt(0 3 %9s %9s %9s %9s)) ///
        nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
        keep(HighCCEI_both_high ccei_gap_ij mathscore_i mathscore_diff ///
        female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff `shares') ///
        order(HighCCEI_both_high ccei_gap_ij mathscore_i mathscore_diff ///
        female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff `shares') ///
        nomtitles fragment nonumbers nolines prefoot("\midrule") postfoot("\bottomrule")

    * Appendix: the controlled indicator and continuous-difference models only.
    if "`variant'" != "original" {
        local write_mode "append"
        local panel "B: 2.5-percentage-point buffer"
        if "`variant'" == "no_buffer" {
            local write_mode "replace"
            local panel "A: No buffer"
        }
        if "`variant'" == "generous_buffer" local panel "C: 5-percentage-point buffer"
        esttab m2 m3 m5 m6 using "results/tables/table_bargainingCCEI_buffers.tex", `write_mode' ///
            b(3) se(3) noobs nogap compress star(+ 0.1 * 0.05 ** 0.01) substitute(\_ _) ///
            keep(HighCCEI_both_high ccei_gap_ij) order(HighCCEI_both_high ccei_gap_ij) ///
            coeflabels(HighCCEI_both_high "\$Higher CCEI_i\$" ccei_gap_ij "\$CCEI_i-CCEI_j\$") ///
            nomtitles fragment nonumbers nolines collabels(none) ///
            prehead("\multicolumn{5}{l}{\emph{Panel `panel'}} \\") ///
            prefoot("") postfoot("\addlinespace")
    }
}
postclose coefficient_file
postclose rate_file
use `coefficients', clear
export delimited using "`out_dir'/coefficients.csv", replace
use `rates', clear
export delimited using "`out_dir'/choice_rates.csv", replace
log close
