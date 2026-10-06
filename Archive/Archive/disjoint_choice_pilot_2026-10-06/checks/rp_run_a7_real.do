cd "/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code"
********************************************************************************
* Table A7: disjoint-choice validation using Table 3's six specifications.
* Run from Code after programs/calculate_indices_disjoint.R (500 partitions by default).
* R calculates CCEI, distance, choice shares and outcome-half M; Stata estimates.
* Only a complete 500-partition, 652-pair run is copied into the manuscript.
* Smaller runs write a pilot table beside the split inputs for checking.
********************************************************************************
clear
set more off
local code_dir `"`c(pwd)'"'
local split_dir : environment DISJOINT_OUTPUT_DIR
if `"`split_dir'"' == "" local split_dir `"`code_dir'/results/tests/disjoint_choice"'
capture confirm file `"`split_dir'/run_config.dta"'
if _rc {
    di as text "Table A7 not regenerated: run programs/calculate_indices_disjoint.R first."
}
else {
    cap mkdir "Logs"
    capture log close disjoint_choice
    log using "Logs/08_disjoint_choice.log", name(disjoint_choice) text replace
    use `"`split_dir'/run_config.dta"', clear
    assert _N == 1 & repetitions >= 1 & repetitions == floor(repetitions)
    assert n_pairs > 1
    local split_reps = repetitions[1]
    local split_pairs = n_pairs[1]
    local production = (`split_reps' == 500 & `split_pairs' == 652)
    forvalues repetition = 1/`split_reps' {
        local suffix : display %04.0f `repetition'
        confirm file `"`split_dir'/indices/split_`suffix'.dta"'
    }

    * Start with Table 3's full-choice balanced sample and its covariates.
    tempfile table3_panel disjoint_results summary
    use "data/panel_individual.dta", clear
    isid id post
    bysort id: egen n_full_distance = total(!missing(Ihat_ig))
    keep if n_full_distance == 2
    drop n_full_distance
    bysort id: assert _N == 2
    if `production' assert _N == 2512
    capture drop id_fe female_i_male_j male_i_female_j
    egen long id_fe = group(id)
    gen byte female_i_male_j = (male_i == 0 & male_j == 1)
    gen byte male_i_female_j = (male_i == 1 & male_j == 0)
    foreach variable in corner_share_i corner_share_diff mid_share_i mid_share_diff {
        capture drop `variable'
    }
    save `table3_panel'

    * Pool A -> B and B -> A estimates, retaining each fit's clustered SE and N.
    * Same covariates as the adopted Table 3; all columns include M.
    * Source-half shares replace the full-choice shares in this hold-out test.
    local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
    local gender "female_i_male_j male_i_female_j"
    local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
    local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
    local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"
    local full_controls "`individual' `friendship' `missing_controls' `shares'"
    local selected "mathscore_i mathscore_diff female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff"
    tempname draws
    postfile `draws' int repetition byte direction column str32 term ///
        double coefficient se n r2 clusters using `disjoint_results', replace
    forvalues repetition = 1/`split_reps' {
        local suffix : display %04.0f `repetition'
        foreach source in A B {
            local outcome = cond("`source'" == "A", "B", "A")
            local direction = cond("`source'" == "A", 1, 2)
            use `table3_panel', clear
            merge 1:1 id post using `"`split_dir'/indices/split_`suffix'.dta"', ///
                keep(master match) assert(match using) nogen
            assert repetition == `repetition'
            assert group_id != "" & !missing(ccei_`source', partner_ccei_`source')
            assert n_donors_`outcome' == `split_pairs' - 1 & !missing(M_`outcome')
            * Keep outcome-half distance defined in both waves; all six fits
            * in this repetition/direction use the same balanced observations.
            bysort id: egen n_half_distance = total(!missing(I_`outcome'))
            keep if n_half_distance == 2
            drop n_half_distance
            assert _N > 0
            local sample_n = _N
            gen byte split_high = (ccei_`source' >= partner_ccei_`source')
            gen double split_gap = ccei_`source' - partner_ccei_`source'
            gen double split_I = I_`outcome'
            gen double split_M = M_`outcome'
            gen double corner_share_i = corner_share_`source'
            gen double corner_share_diff = corner_diff_`source'
            gen double mid_share_i = mid_share_`source'
            gen double mid_share_diff = mid_diff_`source'
            local column = 0
            foreach focal in split_high split_gap {
                forvalues spec = 1/3 {
                    local ++column
                    if `spec' == 1 {
                        quietly reghdfe split_I `focal' split_M, absorb(class) vce(cluster class)
                    }
                    if `spec' == 2 {
                        quietly reghdfe split_I `focal' split_M `full_controls' `gender', ///
                            absorb(class) vce(cluster class)
                    }
                    if `spec' == 3 {
                        quietly reghdfe split_I `focal' split_M `full_controls', ///
                            absorb(id_fe) vce(cluster class)
                    }
                    assert e(N) == `sample_n'
                    assert _se[`focal'] > 0 & _se[split_M] > 0
                    foreach term in `focal' split_M `selected' {
                        if !missing(colnumb(e(b), "`term'")) {
                            if _se[`term'] > 0 {
                                post `draws' (`repetition') (`direction') (`column') ("`term'") ///
                                    (_b[`term']) (_se[`term']) (e(N)) (e(r2)) (e(N_clust))
                            }
                        }
                    }
                }
            }
        }
        di as text "Table A7: estimated partition `repetition'/`split_reps'."
    }
    postclose `draws'
    use `disjoint_results', clear
    isid repetition direction column term
    save `"`split_dir'/disjoint_estimates.dta"', replace

    * Distribution percentiles describe the split estimates, not confidence
    * intervals based on treating partitions as independent observations.
    preserve
    keep if inlist(term, "split_high", "split_gap")
    bysort column: assert _N == 2 * `split_reps'
    collapse (p50) n r2 clusters, by(column)
    forvalues column = 1/6 {
        foreach stat in n r2 clusters {
            quietly summarize `stat' if column == `column', meanonly
            local fmt "%9.0fc"
            if "`stat'" == "r2" local fmt "%9.3f"
            local `stat'`column' = strtrim(string(r(mean), "`fmt'"))
        }
    }
    restore
    bysort term column: egen double median = median(coefficient)
    bysort term column: egen double lower = pctile(coefficient), p(2.5)
    bysort term column: egen double upper = pctile(coefficient), p(97.5)
    bysort term column: gen int n_estimates = _N
    bysort term column: keep if _n == 1
    assert n_estimates == 2 * `split_reps'
    keep term column median lower upper n_estimates
    save `"`split_dir'/disjoint_summary.dta"', replace

    * Six columns with the same reported coefficients and controls as Table 3.
    local tex_path `"`split_dir'/table_disjoint_choice_pilot.tex"'
    if `production' {
        cap mkdir "results/tables"
        local tex_path `"`code_dir'/results/tables/table_disjoint_choice.tex"'
    }
    local bs = char(92)
    gen str12 median_text = strtrim(string(median, "%9.3f"))
    gen str40 range_text = "[" + strtrim(string(lower, "%9.3f")) + ", " + ///
        strtrim(string(upper, "%9.3f")) + "]"
    local label_split_high "Higher`bs' CCEI_i"
    local label_split_gap "CCEI_i-CCEI_j"
    local label_split_M "M_{ig}"
    local label_mathscore_i "Math`bs' score_i"
    local label_mathscore_diff "Math`bs' score_{diff}"
    local label_female_i_male_j "(Female_i, Male_j)"
    local label_male_i_female_j "(Male_i, Female_j)"
    local label_inclass_popularity_i "In`bs'text{-}degree_i"
    local label_inclass_pop_diff "In`bs'text{-}degree_{diff}"
    tempname disjoint_table
    file open `disjoint_table' using `"`tex_path'"', write replace
    file write `disjoint_table' "`bs'resizebox{`bs'textwidth}{!}{%" _n
    file write `disjoint_table' "`bs'begin{tabular}{lcccccc}`bs'toprule" _n
    file write `disjoint_table' "& `bs'multicolumn{3}{c}{Ties Assigned High} & `bs'multicolumn{3}{c}{CCEI Difference}`bs'`bs'" _n
    file write `disjoint_table' "`bs'cmidrule(lr){2-4}`bs'cmidrule(lr){5-7}" _n
    file write `disjoint_table' "Dependent variable: " (char(36)) "I_{ig}" ///
        (char(36)) " & (1) & (2) & (3) & (4) & (5) & (6)`bs'`bs'`bs'midrule" _n
    foreach term in split_high split_gap split_M `selected' {
        foreach stat in median_text range_text {
            local row_label = cond("`stat'" == "median_text", "`label_`term''", "")
            if "`stat'" == "median_text" {
                file write `disjoint_table' (char(36)) "`row_label'" (char(36))
            }
            forvalues column = 1/6 {
                local cell ""
                quietly levelsof `stat' if term == "`term'" & column == `column', local(cell) clean
                file write `disjoint_table' " & `cell'"
            }
            file write `disjoint_table' "`bs'`bs'" _n
        }
    }
    file write `disjoint_table' "`bs'midrule" _n
    foreach stat in n r2 clusters {
        local label = cond("`stat'" == "n", "Median N", cond("`stat'" == "r2", "Median R-squared", "Median class clusters"))
        file write `disjoint_table' "`label'"
        forvalues column = 1/6 {
            file write `disjoint_table' " & ``stat'`column''"
        }
        file write `disjoint_table' "`bs'`bs'" _n
    }
    file write `disjoint_table' "Fixed effects & Class & Class & Individual & Class & Class & Individual`bs'`bs'" _n
    foreach label in "Individual and friendship controls" "Corner/midpoint share controls" {
        file write `disjoint_table' "`label' & & " (char(36)) "`bs'checkmark" ///
            (char(36)) " & " (char(36)) "`bs'checkmark" (char(36)) " & & " ///
            (char(36)) "`bs'checkmark" (char(36)) " & " (char(36)) ///
            "`bs'checkmark" (char(36)) "`bs'`bs'" _n
    }
    local n_estimates = 2 * `split_reps'
    file write `disjoint_table' "Coefficient estimates per column & `n_estimates' & `n_estimates' & `n_estimates' & `n_estimates' & `n_estimates' & `n_estimates'`bs'`bs'" _n
    file write `disjoint_table' "`bs'bottomrule`bs'end{tabular}}" _n
    file write `disjoint_table' "`bs'par`bs'vspace{6pt}`bs'begin{minipage}{`bs'textwidth}`bs'footnotesize" _n
    file write `disjoint_table' "`bs'emph{Notes}: Each member's 18 individual choices and the group's 18 choices are independently split into two sets of nine in each of `split_reps' partitions. In each direction, one set supplies individual CCEIs and exact corner/equal-allocation shares, and the other supplies actual distance and its placebo benchmark. The benchmark uses all " (`split_pairs' - 1) " non-own same-wave donor pairs, using only their group choices from the outcome set; undefined donor distances receive 0.5. Both directions are pooled, giving `n_estimates' coefficient estimates per column. Entries report median coefficients, with the 2.5th and 97.5th percentiles of their split distributions in brackets. These ranges are not confidence intervals obtained by pooling partition observations. Columns match Table 3: ties are assigned high in Columns (1)--(3); Columns (4)--(6) use the signed within-pair CCEI difference. All columns control for the outcome-set benchmark. Columns (2), (3), (5), and (6) include individual, friendship, missing-value, and choice-share controls; risk-aversion controls are excluded. Gender-composition controls enter Columns (2) and (5) and are absorbed by individual fixed effects in Columns (3) and (6). Starting from Table 3's balanced sample, each direction retains students with defined outcome-set distance in both waves, using a common sample for all six fits. Each regression clusters standard errors by class. Median sample sizes, R-squared values, and class-cluster counts are reported." _n
    file write `disjoint_table' "`bs'end{minipage}" _n
    file close `disjoint_table'
    if `production' {
        copy `"`tex_path'"' "../Overleaf/tables_2025/table_disjoint_choice.tex", replace
    }
    di as result "SUCCESS: exported six-column Table A7 (`split_reps' partitions; `n_estimates' fits per column)."
    log close disjoint_choice
}
