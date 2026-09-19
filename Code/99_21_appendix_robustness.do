* Appendix robustness table: HM, MaxMPI, and risk-aversion distance.
* Run from Code. Six columns follow Table 3, without RA controls.
* Keep the existing outcome-specific samples and exact choice-share controls.
set more off
capture mkdir "results/tables"
capture log close appendix_robustness
log using "results/tables/appendix_robustness.log", name(appendix_robustness) text replace
do "programs/prepare_table3_controls.do"

local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local gender "female_i_male_j male_i_female_j"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"

gen double RA_distance_denom = (RA_i - RA_g)^2 + (RA_j - RA_g)^2
gen double RA_I_ig = (RA_i - RA_g)^2 / RA_distance_denom if RA_distance_denom > 0
assert hm_gap_ij == hm_j - hm_i
label var HighHM_both_high "\$Higher FGARP_i\$"
label var hm_gap_ij "\$HM_j-HM_i\$"
label var HighMaxMPI_both_high "\$Higher RevMaxMPI_i\$"
label var maxmpi_gap_ij "\$RevMaxMPI_i-RevMaxMPI_j\$"
label var HighCCEI_both_high "\$Higher CCEI_i\$"
label var ccei_gap_ij "\$CCEI_i-CCEI_j\$"
label var mathscore_i "\$Math score_i\$"
label var mathscore_diff "\$Math score_{diff}\$"
label var female_i_male_j "\$(Female_i, Male_j)\$"
label var male_i_female_j "\$(Male_i, Female_j)\$"
label var inclass_popularity_i "\$In-degree_i\$"
label var inclass_pop_diff "\$In-degree_{diff}\$"

local write_mode "replace"
foreach measure in HM MaxMPI RA {
    if "`measure'" == "HM" {
        local outcome "Ihat_hm_ig"
        local high "HighHM_both_high"
        local gap "hm_gap_ij"
        local panel "Panel A: HM-based revealed-preference distance"
    }
    else if "`measure'" == "MaxMPI" {
        local outcome "Ihat_maxmpi_ig"
        local high "HighMaxMPI_both_high"
        local gap "maxmpi_gap_ij"
        local panel "Panel B: MaxMPI-based revealed-preference distance"
    }
    else {
        local outcome "RA_I_ig"
        local high "HighCCEI_both_high"
        local gap "ccei_gap_ij"
        local panel "Panel C: Normalized risk-aversion distance"
    }
    capture drop n_outcome sample_alt
    bysort id: egen n_outcome = total(!missing(`outcome'))
    gen byte sample_alt = (n_outcome == 2)
    local expected_n = 2512
    if "`measure'" == "RA" {
        replace sample_alt = !missing(`outcome')
        local expected_n = 2604
    }

    eststo clear
    forvalues column = 1/6 {
        local focal "`high'"
        if `column' > 3 local focal "`gap'"
        local specification = mod(`column' - 1, 3) + 1
        local controls ""
        local fixed_effect "class"
        local fixed_label "Class"
        local singleton_option ""
        if `specification' == 2 local controls "`individual' `gender' `friendship' `missing_controls' `shares'"
        if `specification' == 3 {
            local controls "`individual' `friendship' `missing_controls' `shares'"
            local fixed_effect "id_fe"
            local fixed_label "Individual"
            if "`measure'" == "RA" local singleton_option "keepsingletons"
        }
        reghdfe `outcome' `focal' `controls' if sample_alt, ///
            absorb(`fixed_effect') vce(cluster class) `singleton_option'
        assert e(N) == `expected_n' & e(N_clust) == 64
        assert e(sample) == sample_alt
        eststo m`column'
        estadd local fixed_effects "`fixed_label'" : m`column'
        if `specification' > 1 {
            estadd local individual_controls "\checkmark" : m`column'
            estadd local friendship_controls "\checkmark" : m`column'
            estadd local share_controls "\checkmark" : m`column'
        }
        display "CHECK `measure' column `column': b=" _b[`focal'] " se=" _se[`focal'] ///
            " p=" 2 * ttail(e(df_r), abs(_b[`focal'] / _se[`focal']))
        * Pooled estimation-sample SDs for the magnitudes reported in Section 5.2.
        quietly summarize `outcome' if e(sample)
        local outcome_sd = r(sd)
        quietly summarize `focal' if e(sample)
        display "SCALE `measure' column `column': outcome_sd=" `outcome_sd' ///
            " focal_sd=" r(sd) " pct_outcome_sd=" 100 * abs(_b[`focal']) / `outcome_sd' ///
            " standardized_effect=" abs(_b[`focal']) * r(sd) / `outcome_sd'
    }

    esttab m1 m2 m3 m4 m5 m6 using "results/tables/table_bargaining_alternatives.tex", `write_mode' ///
        b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
        nogap compress star(+ 0.1 * 0.05 ** 0.01) label substitute(\_ _) ///
        keep(`high' `gap' mathscore_i mathscore_diff) ///
        order(`high' `gap' mathscore_i mathscore_diff) ///
        nomtitles fragment nonumbers nolines ///
        prehead("\multicolumn{7}{l}{\textit{`panel'}} \\") prefoot("\addlinespace[2pt]") postfoot("\midrule")
    local write_mode "append"

    * Retain the existing individual fragments for users of the old file names.
    if "`measure'" != "RA" {
        esttab m1 m2 m3 m4 m5 m6 using "results/tables/table_bargaining`measure'.tex", replace ///
            b(3) se(3) stats(N r2 fixed_effects individual_controls friendship_controls share_controls, ///
            labels("N" "R-squared" "Fixed effects" "Other individual characteristics" ///
            "Other friendship characteristics" "Corner/midpoint share controls") fmt(0 3 %9s %9s %9s %9s)) ///
            nogap compress star(+ 0.1 * 0.05 ** 0.01) label substitute(\_ _) ///
            keep(`high' `gap' mathscore_i mathscore_diff female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff) ///
            order(`high' `gap' mathscore_i mathscore_diff female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff) ///
            nomtitles fragment nonumbers nolines prefoot("\hline") postfoot("\hline \bottomrule")
    }
    else {
        foreach definition in high gap {
            local focal "``definition''"
            local models "m1 partial m2 m3"
            local suffix ""
            if "`definition'" == "gap" {
                local models "m4 partial m5 m6"
                local suffix "_cceidiff"
            }
            quietly reghdfe `outcome' `focal' `individual' `gender' `friendship' `missing_controls' ///
                if sample_alt, absorb(class) vce(cluster class)
            estimates store partial
            esttab `models' using "results/tables/table_bargainingRA_distance`suffix'.tex", replace ///
                b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
                nogap compress star(+ 0.1 * 0.05 ** 0.01) label substitute(\_ _) ///
                keep(`focal' mathscore_i mathscore_diff female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff) ///
                nomtitles fragment nonumbers nolines prefoot("\hline") postfoot("\bottomrule")
        }
    }
}
file open footer using "results/tables/table_bargaining_alternatives.tex", write append
file write footer "Fixed effects & Class & Class & Individual & Class & Class & Individual \\" _n
file write footer "Individual and friendship controls & & \checkmark & \checkmark & & \checkmark & \checkmark \\" _n
file write footer "Corner/midpoint share controls & & \checkmark & \checkmark & & \checkmark & \checkmark \\" _n
file write footer "\bottomrule" _n
file close footer
log close appendix_robustness
