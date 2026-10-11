********************************************************************************
* 5_Tables_Appendix.do: Locked appendix Tables A1--A7 and figure estimates.
* Last updated: 2026-10-11. Run from Code after the canonical panel preparation.
* Tables: buffers, baseline correlations, mover status, CCEI changes,
* alternative measures, strictly higher CCEI, and collective-outcome categories.
* Figure inputs: A7 (Table 3 R2), A10 (joint-outcome R2), A11 (multinomial AMEs),
* and A12 (alternative-index LPMs). 6 draws the corresponding panels.
* Manuscript tables go to results/tables and their existing Overleaf paths.
* Unused analyses are retained in 99_9_Tables_Appendix_Backup.do.
********************************************************************************

clear all
set more off
set matsize 8000

local code_dir `"`c(pwd)'"'
local data_dir `"`code_dir'/data"'
local tex_dir `"`code_dir'/results/tables"'
cap mkdir `"`code_dir'/results"'
cap mkdir `"`tex_dir'"'

foreach required in panel_individual.dta base_raw.dta end_raw.dta {
    capture confirm file `"`data_dir'/`required'"'
    if _rc {
        di as error "Missing required input: `data_dir'/`required'"
        exit 601
    }
}

********************************************************************************
* Table A1: Sensitivity to Buffers around Corner and Equal-Allocation Choices
********************************************************************************
* Run this section from Code. Use actual I and control for the saved CCEI benchmark M.
local code_dir `"`c(pwd)'"'
local data_dir `"`code_dir'/data"'
local tex_dir `"`code_dir'/results/tables"'
cap mkdir `"`tex_dir'"'
cap mkdir "Logs"
capture log close choice_buffers
log using "Logs/5_choice_buffers.log", name(choice_buffers) text replace

* Reuse Table 3's raw-choice definitions, adding inclusive payoff-share buffers.
tempfile baseline_shares choice_shares partner_shares
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
keep if !missing(Ihat_ig)
assert _N == 2560

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
* M and exact-choice controls are carried by the canonical individual panel.
assert n_ccei_donors == 651 & !missing(M_ccei)

local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local gender "female_i_male_j male_i_female_j"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
label var HighCCEI_both_high "\$Higher\ CCEI_i\$"
label var ccei_gap_ij "\$CCEI_i-CCEI_j\$"
label var M_ccei "\$M_{ig}\$"

local panel = 0
foreach suffix in 0 025 05 {
    local ++panel
    capture drop candidate_corner_i candidate_corner_diff candidate_mid_i candidate_mid_diff
    gen double candidate_corner_i = corner_`suffix'_i
    gen double candidate_corner_diff = corner_`suffix'_i - corner_`suffix'_j
    gen double candidate_mid_i = mid_`suffix'_i
    gen double candidate_mid_diff = mid_`suffix'_i - mid_`suffix'_j
    local shares "candidate_corner_i candidate_corner_diff candidate_mid_i candidate_mid_diff"
    eststo clear
    local col = 0
    foreach focal in HighCCEI_both_high ccei_gap_ij {
        forvalues spec = 2/3 {
            local ++col
            local controls "`individual' `friendship' `missing_controls' `shares'"
            local fe id_fe
            local singleton_option "keepsingletons"
            if `spec' == 2 {
                local controls "`controls' `gender'"
                local fe class
                local singleton_option ""
            }
            quietly reghdfe Ihat_ig `focal' M_ccei `controls', absorb(`fe') vce(cluster class) `singleton_option'
            assert e(sample) == 1
            assert e(N) == 2560 & e(N_clust) == 64
            quietly summarize Ihat_ig if e(sample)
            estadd scalar ymean = r(mean)
            estadd scalar ysd = r(sd)
            estimates store buffer`col'
        }
    }
    local title "Panel A: No buffer"
    local mode replace
    if `panel' == 2 local title "Panel B: 2.5-percentage-point buffer"
    if `panel' == 3 local title "Panel C: 5-percentage-point buffer"
    if `panel' > 1 local mode append
    local panel_header ""
    if `panel' == 1 {
        local outcome_mean : display %3.1f e(ymean)
        local outcome_sd : display %4.2f e(ysd)
        local panel_header "\multicolumn{5}{l}{\hspace{1em} Mean: `outcome_mean', SD: `outcome_sd'} \\ \midrule"
    }
    local panel_separator "\hline"
    if `panel' == 3 local panel_separator "\midrule"
    esttab buffer1 buffer2 buffer3 buffer4 using `"`tex_dir'/table_bargainingCCEI_buffers_M.tex"', `mode' ///
        b(3) se(3) star(+ 0.1 * 0.05 ** 0.01) label nogap compress noomitted substitute(\_ _) ///
        keep(HighCCEI_both_high ccei_gap_ij M_ccei) order(HighCCEI_both_high ccei_gap_ij M_ccei) ///
        fragment nomtitles nonumbers nolines ///
        stats(r2, fmt(3) labels("R-squared")) ///
        prehead("`panel_header'" "\multicolumn{5}{l}{\emph{`title'}} \\") prefoot("") postfoot("`panel_separator'")
}
* All twelve fits use the same dependent variable and estimation sample.
* Export the shared footer from the fitted models rather than hard-coding values.
tempname buffer_footer
file open `buffer_footer' using `"`tex_dir'/table_bargainingCCEI_buffers_M.tex"', write append
file write `buffer_footer' "N"
forvalues column = 1/4 {
    estimates restore buffer`column'
    file write `buffer_footer' " & " %9.0f (e(N))
}
file write `buffer_footer' "\\" _n
file write `buffer_footer' "Fixed effects & Class & Individual & Class & Individual \\" _n
file write `buffer_footer' "Individual and friendship controls & \checkmark & \checkmark & \checkmark & \checkmark \\" _n
file write `buffer_footer' "Corner/midpoint share controls & \checkmark & \checkmark & \checkmark & \checkmark \\" _n
file write `buffer_footer' "\bottomrule" _n
file close `buffer_footer'
copy `"`tex_dir'/table_bargainingCCEI_buffers_M.tex"' ///
    `"`code_dir'/../Overleaf/tables_2025/table_bargainingCCEI_buffers_M.tex"', replace
di as result "SUCCESS: exported the M-controlled buffer appendix table; N=2560, clusters=64."
log close choice_buffers


********************************************************************************
* Table A2: Baseline correlations, using placebo-adjusted preference distance I-M
********************************************************************************
local code_dir `"`c(pwd)'"'
local tex_dir `"`code_dir'/results/tables"'
use "`code_dir'/data/panel_individual.dta", clear
isid id post
assert n_ccei_donors == 651 & !missing(M_ccei)
keep if post == 0
assert _N == 1304
capture drop analysis_distance
gen double analysis_distance = Ihat_ig - M_ccei
label var ccei_i "CCEI"
label var RA_i "Risk attitude"
label var analysis_distance "Preference Distance"
label var inclass_n_friends_i "Out-degree"
label var inclass_popularity_i "In-degree"
label var male_i "Male"
label var height_i "Height"
label var mathscore_i "Math score"
label var RAT_strict_i "RAT score"
label var outgoing_i "Outgoing"
label var opened_i "Openness"
label var agreeable_i "Agreeableness"
label var conscientious_i "Conscientiousness"
label var stable_i "Emotional stability"
local col_vars "ccei_i RA_i analysis_distance inclass_n_friends_i inclass_popularity_i"
local row_vars "`col_vars' male_i height_i mathscore_i RAT_strict_i outgoing_i opened_i agreeable_i conscientious_i stable_i"

capture file close corrfile
file open corrfile using "`tex_dir'/table_correlation_IminusM.tex", write replace
file write corrfile "\begin{tabular}{lccccc}\toprule \hline" _n
file write corrfile " "
foreach variable of local col_vars {
    local column_label : variable label `variable'
    file write corrfile " & `column_label'"
}
file write corrfile " \\" _n "\midrule" _n
local row = 1
foreach var1 of local row_vars {
    local row_label : variable label `var1'
    file write corrfile "`row_label'"
    local col = 1
    foreach var2 of local col_vars {
        if `col' < `row' {
            quietly pwcorr `var1' `var2', sig
            local rho = r(rho)
            local pval = r(sig)[2,1]
            local stars ""
            if `pval' < 0.01 local stars "**"
            else if `pval' < 0.05 local stars "*"
            else if `pval' < 0.10 local stars "+"
            local significance ""
            if "`stars'" != "" local significance "\sym{`stars'}"
            file write corrfile " & " %5.3f (`rho') "`significance'"
        }
        else file write corrfile " & "
        local ++col
    }
    file write corrfile " \\" _n
    local ++row
}
file write corrfile "\bottomrule\end{tabular}" _n
file close corrfile
copy "`tex_dir'/table_correlation_IminusM.tex" ///
    "`code_dir'/../Overleaf/tables_2025/table_correlation_IminusM.tex", replace

********************************************************************************
* Table A3: Individual Rationality and Revealed-Preference Distance by Mover Status
********************************************************************************
* Actual I is the outcome; all columns control for the full 651-donor M.
* Individual, friendship and choice-share controls match 3; RA is excluded.
local code_dir `"`c(pwd)'"'
local tex_dir `"`code_dir'/results/tables"'
local distance_var "Ihat_ig"
cap mkdir `"`tex_dir'"'
use "data/panel_individual.dta", clear
local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"
cap mkdir "Logs"
capture log close table3_extensions
log using "Logs/5_table3_extensions.log", name(table3_extensions) text replace
preserve
* Use the validated benchmark panel; the available-distance mask matches Table 3.
use "data/panel_individual.dta", clear
isid id post
assert _N == 2608
gen byte available_t3 = !missing(Ihat_ig)
assert missing(I_ccei) == missing(Ihat_ig)
assert I_ccei == Ihat_ig if !missing(Ihat_ig)
assert n_ccei_donors == 651 & !missing(M_ccei)
local full_controls "`individual' `friendship' `missing_controls' `shares'"
local gender "female_i_male_j male_i_female_j"
local selected "mathscore_i mathscore_diff female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff"
label var HighCCEI_both_high "\$Higher\ CCEI_i\$"
label var HighCCEI_both_low "\$Higher\ CCEI_i\$"
label var ccei_gap_ij "\$CCEI_i-CCEI_j\$"
label var M_ccei "\$M_{ig}\$"
label var female_i_male_j "\$(Female_i, Male_j)\$"
label var male_i_female_j "\$(Male_i, Female_j)\$"
label var mathscore_i "\$Math\ score_i\$"
label var mathscore_diff "\$Math\ score_{diff}\$"
label var inclass_popularity_i "\$In\text{-}degree_i\$"
label var inclass_pop_diff "\$In\text{-}degree_{diff}\$"

do "programs/hdfe_cluster_vce.do"

* Mover status: augment each of Table 3's six models with mover and its interaction.
assert inlist(mover, 0, 1)
bysort group_id post: assert _N == 2 & available_t3 == available_t3[1]
by group_id post: assert mover[1] + mover[2] == 1
by group_id post: assert abs(Ihat_ig[1] + Ihat_ig[2] - 1) < 1e-12 if available_t3
by group_id post: assert abs(M_ccei[1] + M_ccei[2] - 1) < 1e-12
capture drop higher_mover gap_mover
gen double higher_mover = HighCCEI_both_high * mover
gen double gap_mover = ccei_gap_ij * mover
label var mover "\$Mover_i\$"
label var higher_mover "\$Higher\ CCEI_i\times Mover_i\$"
label var gap_mover "\$(CCEI_i-CCEI_j)\times Mover_i\$"
eststo clear
local column = 0
foreach focal in HighCCEI_both_high ccei_gap_ij {
    local interaction higher_mover
    if "`focal'" == "ccei_gap_ij" local interaction gap_mover
    forvalues spec = 1/3 {
        local ++column
        if `spec' == 1 {
            quietly reghdfe `distance_var' `focal' mover `interaction' M_ccei if available_t3, ///
                absorb(class) vce(cluster class) keepmata
        }
        if `spec' == 2 {
            quietly reghdfe `distance_var' `focal' mover `interaction' M_ccei `full_controls' `gender' if available_t3, ///
                absorb(class) vce(cluster class) keepmata
        }
        if `spec' == 3 {
            quietly reghdfe `distance_var' `focal' mover `interaction' M_ccei `full_controls' if available_t3, ///
                absorb(id_fe) vce(cluster class) keepsingletons keepmata
        }
        local zero_option ""
        if `column' == 4 local zero_option "zero(gap_mover)"
        hdfe_cluster_vce class, `zero_option'
        assert !missing(_se[`focal'], _se[M_ccei])
        assert e(N) == 2560 & e(N_clust) == 64
        assert e(sample) == available_t3
        quietly summarize `distance_var' if e(sample)
        estadd scalar ymean = r(mean)
        estadd scalar ysd = r(sd)
        local fixed_label Class
        if `spec' == 3 local fixed_label Individual
        local check ""
        if `spec' >= 2 local check "\checkmark"
        estadd local fixed_effects "`fixed_label'"
        estadd local characteristics "`check'"
        estadd local choice_shares "`check'"
        estimates store mover`column'
    }
}
* The structural zero in Column (4) displays (0.000), without a test or star.
local outcome_mean : display %3.1f e(ymean)
local outcome_sd : display %4.2f e(ysd)
esttab mover1 mover2 mover3 mover4 mover5 mover6 using `"`tex_dir'/table_bargainingCCEI_mover_M.tex"', replace ///
    b(3) se(3) star(+ 0.1 * 0.05 ** 0.01) label nogap compress noomitted substitute(\_ _ "(.)" "(0.000)") ///
    keep(HighCCEI_both_high ccei_gap_ij mover higher_mover gap_mover M_ccei `selected') ///
    order(HighCCEI_both_high ccei_gap_ij mover higher_mover gap_mover M_ccei `selected') ///
    stats(N r2 fixed_effects characteristics choice_shares, fmt(0 3) ///
        labels("N" "R-squared" "Fixed effects" "Individual and friendship controls" "Corner/midpoint share controls")) ///
    fragment nomtitles nonumbers nolines ///
    prehead("\multicolumn{7}{l}{\hspace{1em} Mean: `outcome_mean', SD: `outcome_sd'} \\" "\midrule") ///
    prefoot("\midrule") postfoot("\bottomrule")
copy `"`tex_dir'/table_bargainingCCEI_mover_M.tex"' ///
    `"`code_dir'/../Overleaf/tables_2025/table_bargainingCCEI_mover_M.tex"', replace
restore
di as result "SUCCESS: exported Table A3; N=2560, clusters=64."
log close table3_extensions

********************************************************************************
* Table A4: Individual Characteristics and Changes in CCEI
* Baseline controls, then baseline controls plus changes, for 1,256 students.
* No fixed effects, partner characteristics, or within-pair differences enter.
********************************************************************************
preserve
local data_dir "data"
local out_dir "results/tables"
capture mkdir "results"
capture mkdir "`out_dir'"
tempfile coefficients
capture log close ccei_change
log using "`out_dir'/individual_ccei_change.log", name(ccei_change) text replace

use "`data_dir'/panel_individual.dta", clear
isid id post
bysort id (post): assert _N == 2 & post[1] == 0 & post[2] == 1
assert !missing(ccei_i)
bysort id (post): gen double ccei_change = ccei_i[2] - ccei_i[1]
bysort id: egen n_distance = total(!missing(Ihat_ig))
gen byte table3_sample = (n_distance == 2)


local student "male_i height_i mathscore_i outgoing_i opened_i agreeable_i conscientious_i stable_i inclass_n_friends_i inclass_popularity_i"
local missing_controls "mathscore_i_missing outgoing_i_missing opened_i_missing agreeable_i_missing conscientious_i_missing stable_i_missing"

foreach var in mathscore_i inclass_n_friends_i inclass_popularity_i {
    bysort id (post): gen double `var'_change = `var'[2] - `var'[1]
}

local student_chng "mathscore_i_change inclass_n_friends_i_change inclass_popularity_i_change"

keep if post == 0
isid id
assert _N == 1304
count if table3_sample
assert r(N) == 1256

label var male_i "Male"
label var height_i "Height"
label var mathscore_i "Math score"
label var outgoing_i "Outgoingness"
label var opened_i "Openness"
label var agreeable_i "Agreeableness"
label var conscientious_i "Conscientiousness"
label var stable_i "Emotional stability"
label var inclass_n_friends_i "Out-degree"
label var inclass_popularity_i "In-degree"
label var mathscore_i_change "Change in math score"
label var inclass_n_friends_i_change "Change in out-degree"
label var inclass_popularity_i_change "Change in in-degree"

eststo clear
eststo change1: regress ccei_change `student' `missing_controls' ///
    if table3_sample == 1, vce(cluster class)
eststo change2: regress ccei_change `student' `student_chng' `missing_controls' ///
    if table3_sample == 1, vce(cluster class)

postfile coefficient_file byte column str32 variable double beta se p N ///
    df clusters using `coefficients'
forvalues column = 1/2 {
    estimates restore change`column'
    assert e(N) == 1256 & e(N_clust) == 64
    local reported "`student'"
    if `column' == 2 local reported "`student' `student_chng'"
    foreach variable in `reported' {
        post coefficient_file (`column') ("`variable'") (_b[`variable']) ///
            (_se[`variable']) (2 * ttail(e(df_r), abs(_b[`variable'] / _se[`variable']))) ///
            (e(N)) (e(df_r)) (e(N_clust))
    }
    quietly summarize ccei_change if e(sample)
    estadd scalar mean_change = r(mean)
    estadd local class_fe "No"
    estadd local missing_indicators "Yes"
    estadd scalar class_clusters = e(N_clust)
}
postclose coefficient_file

esttab change1 change2, b(3) se(3) label ///
    keep(`student' `student_chng') order(`student' `student_chng') ///
    mtitles("Baseline" "Baseline + changes") star(+ 0.1 * 0.05 ** 0.01) ///
    stats(N r2 class_fe, labels("Individuals" "R-squared" "Class fixed effects") fmt(0 3 %9s))

esttab change1 change2 using "`out_dir'/table_individual_ccei_change.tex", replace ///
    b(3) se(3) keep(`student' `student_chng') order(`student' `student_chng') ///
    stats(N r2 mean_change, ///
    labels("N" "R-squared" "Mean change in CCEI") fmt(0 3 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    nomtitles fragment nonumbers nolines prefoot("\midrule") postfoot("\bottomrule")
copy "`out_dir'/table_individual_ccei_change.tex" ///
    "../Overleaf/tables_2025/table_individual_ccei_change.tex", replace

use `coefficients', clear
export delimited using "`out_dir'/individual_ccei_change_coefficients.csv", replace
log close ccei_change
restore

********************************************************************************
* Table A5: alternative measures with full same-wave donor benchmarks.
* Run independently from Code after section 7 of 1_1_calculate_indices.R.
* HM/MaxMPI use results/benchmarks; RA retains its verified 651-donor benchmark.
* Actual distance is the outcome; six specifications match Table 3, excluding RA
* controls. All panels use Table 3's 2,560 defined CCEI-distance observations,
* retaining its 48 single-wave students under individual fixed effects.
********************************************************************************
local a5_code_dir `"`c(pwd)'"'
local a5_tex_dir `"`a5_code_dir'/results/tables"'
cap mkdir "Logs"
cap mkdir `"`a5_tex_dir'"'
capture log close alternative_full
log using "Logs/5_alternative_full.log", name(alternative_full) text replace
preserve
use "data/panel_individual.dta", clear

tempfile a5_panel a5_benchmark a5_results
gen byte available_t3 = !missing(Ihat_ig)
count if available_t3
assert r(N) == 2560
save `a5_panel'
foreach a5_measure in hm maxmpi ra {
    local a5_source "results/benchmarks/`a5_measure'/placebo_normalized_member_wave.dta"
    if "`a5_measure'" == "ra" local a5_source "results/benchmarks/retained_ra/ra_placebo_member_wave.dta"
    use "`a5_source'", clear
    tostring group_id, replace format(%14.0f)
    tostring id, replace format(%7.0f)
    isid group_id post id
    assert _N == 2608 & n_all == 651 & !missing(M_all_imp)
    keep group_id post id I_actual M_all_imp
    rename I_actual a5_cached_`a5_measure'
    rename M_all_imp M_`a5_measure'
    save `a5_benchmark', replace
    use `a5_panel', clear
    capture drop M_`a5_measure'
    merge 1:1 group_id post id using `a5_benchmark', assert(match) nogen
    save `a5_panel', replace
}

gen double a5_I_hm = Ihat_hm_ig
gen double a5_I_maxmpi = Ihat_maxmpi_ig
gen double a5_RA_denom = (RA_i - RA_g)^2 + (RA_j - RA_g)^2
gen double a5_I_ra = (RA_i - RA_g)^2 / a5_RA_denom if a5_RA_denom > 0
foreach a5_measure in hm maxmpi ra {
    assert missing(a5_I_`a5_measure') == missing(a5_cached_`a5_measure')
    assert abs(a5_I_`a5_measure' - a5_cached_`a5_measure') < 2e-6 if !missing(a5_I_`a5_measure')
    bysort group_id post: assert abs(M_`a5_measure'[1] + M_`a5_measure'[2] - 1) < 1e-7
    assert !missing(a5_I_`a5_measure') if available_t3
}

local a5_individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local a5_friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local a5_missing "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local a5_shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"
local a5_gender "female_i_male_j male_i_female_j"
postfile a5_post str8 measure byte column double beta double se double p ///
    double beta_M double se_M double p_M double outcome_sd double focal_sd ///
    double standardized long N long clusters double r2 using `a5_results', replace
local a5_write "replace"
foreach a5_measure in hm maxmpi ra {
    local a5_high "HighHM_both_high"
    local a5_gap "hm_gap_ij"
    local a5_panel_name "A: HM-based distance"
    if "`a5_measure'" == "maxmpi" {
        local a5_high "HighMaxMPI_both_high"
        local a5_gap "maxmpi_gap_ij"
        local a5_panel_name "B: MaxMPI-based distance"
    }
    if "`a5_measure'" == "ra" {
        local a5_high "HighCCEI_both_high"
        local a5_gap "ccei_gap_ij"
        local a5_panel_name "C: Risk-aversion distance"
    }
    label var `a5_high' "Higher rationality"
    label var `a5_gap' "Rationality difference"
    label var M_`a5_measure' "\$M_{ig}\$"
    forvalues a5_column = 1/6 {
        local a5_spec = mod(`a5_column' - 1, 3) + 1
        local a5_focal "`a5_high'"
        if `a5_column' > 3 local a5_focal "`a5_gap'"
        local a5_controls ""
        local a5_effect "class"
        local a5_singletons ""
        if `a5_spec' > 1 local a5_controls "`a5_individual' `a5_friendship' `a5_missing' `a5_shares'"
        if `a5_spec' == 2 local a5_controls "`a5_controls' `a5_gender'"
        if `a5_spec' == 3 {
            local a5_effect "id_fe"
            local a5_singletons "keepsingletons"
        }
        quietly reghdfe a5_I_`a5_measure' `a5_focal' M_`a5_measure' `a5_controls' ///
            if available_t3, absorb(`a5_effect') vce(cluster class) `a5_singletons'
        assert e(N) == 2560 & e(N_clust) == 64
        assert e(sample) == available_t3
        local a5_b = _b[`a5_focal']
        local a5_s = _se[`a5_focal']
        local a5_p = 2 * ttail(e(df_r), abs(`a5_b' / `a5_s'))
        local a5_bM = _b[M_`a5_measure']
        local a5_sM = _se[M_`a5_measure']
        local a5_pM = 2 * ttail(e(df_r), abs(`a5_bM' / `a5_sM'))
        quietly summarize a5_I_`a5_measure' if e(sample)
        local a5_ymean = r(mean)
        local a5_ysd = r(sd)
        quietly summarize `a5_focal' if e(sample)
        local a5_xsd = r(sd)
        post a5_post ("`a5_measure'") (`a5_column') (`a5_b') (`a5_s') (`a5_p') ///
            (`a5_bM') (`a5_sM') (`a5_pM') (`a5_ysd') (`a5_xsd') ///
            (`a5_b' * `a5_xsd' / `a5_ysd') (e(N)) (e(N_clust)) (e(r2))
        estadd scalar ymean = `a5_ymean'
        estadd scalar ysd = `a5_ysd'
        estimates store a5_`a5_measure'_`a5_column'
    }
    local a5_mean : display %3.1f `a5_ymean'
    local a5_sd : display %4.2f `a5_ysd'
    esttab a5_`a5_measure'_1 a5_`a5_measure'_2 a5_`a5_measure'_3 ///
        a5_`a5_measure'_4 a5_`a5_measure'_5 a5_`a5_measure'_6 ///
        using `"`a5_tex_dir'/table_bargaining_alternatives_M.tex"', `a5_write' ///
        b(3) se(3) star(+ 0.1 * 0.05 ** 0.01) label fragment nomtitles nonumbers nolines nogap substitute(\_ _) ///
        keep(`a5_high' `a5_gap' M_`a5_measure') order(`a5_high' `a5_gap' M_`a5_measure') ///
        stats(N r2, fmt(0 3) labels("N" "R-squared")) ///
        prehead("\multicolumn{7}{l}{\emph{Panel `a5_panel_name'}} \\" ///
            "\multicolumn{7}{l}{\hspace{1em} Mean: `a5_mean', SD: `a5_sd'} \\") ///
        posthead("") prefoot("") postfoot("\midrule")
    local a5_write "append"
}
postclose a5_post
file open a5_footer using `"`a5_tex_dir'/table_bargaining_alternatives_M.tex"', write append
file write a5_footer "Fixed effects & Class & Class & Individual & Class & Class & Individual \\" _n
file write a5_footer "Individual and friendship controls & & \checkmark & \checkmark & & \checkmark & \checkmark \\" _n
file write a5_footer "Corner/midpoint share controls & & \checkmark & \checkmark & & \checkmark & \checkmark \\" _n
file write a5_footer "\bottomrule" _n
file close a5_footer
use `a5_results', clear
isid measure column
assert _N == 18
export delimited using `"`a5_tex_dir'/table_bargaining_alternatives_M.csv"', replace
copy `"`a5_tex_dir'/table_bargaining_alternatives_M.tex"' ///
    `"`a5_code_dir'/../Overleaf/tables_2025/table_bargaining_alternatives_M.tex"', replace
restore
di as result "SUCCESS: exported Table A5; all 18 fits match Table 3's N=2560 and 64 class clusters."
log close alternative_full

********************************************************************************
* Table A6: Individual Rationality and Revealed-Preference Distance: Strictly Higher CCEI
********************************************************************************
* Actual I is the outcome; all columns control for the full 651-donor M.
* Individual, friendship and choice-share controls match 3; RA is excluded.
local code_dir `"`c(pwd)'"'
local distance_var "Ihat_ig"
local tex_dir "results/tables"
cap mkdir "Logs"
capture log close ties_low
log using "Logs/5_ties_low.log", name(ties_low) text replace
preserve
use "data/panel_individual.dta", clear
gen byte available_t3 = !missing(`distance_var')
assert n_ccei_donors == 651 & !missing(M_ccei)
assert HighCCEI_both_low == (ccei_i > ccei_j)
local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"
local full_controls "`individual' `friendship' `missing_controls' `shares'"
local gender "female_i_male_j male_i_female_j"
label var HighCCEI_both_low "\$Higher\ CCEI_i\$"
label var M_ccei "\$M_{ig}\$"

* Ties assigned Low: the three higher-CCEI specifications from Table 3.
eststo clear
eststo low1: reghdfe `distance_var' HighCCEI_both_low M_ccei if available_t3, absorb(class) vce(cluster class)
eststo low2: reghdfe `distance_var' HighCCEI_both_low M_ccei `full_controls' `gender' if available_t3, absorb(class) vce(cluster class)
eststo low3: reghdfe `distance_var' HighCCEI_both_low M_ccei `full_controls' if available_t3, absorb(id_fe) vce(cluster class) keepsingletons
foreach model in low1 low2 low3 {
    estimates restore `model'
    assert e(N) == 2560 & e(N_clust) == 64
    assert e(sample) == available_t3
}
foreach model in low1 low2 {
    estadd local fixed_effects "Class" : `model'
}
estadd local fixed_effects "Individual" : low3
foreach model in low2 low3 {
    estadd local characteristics "\checkmark" : `model'
    estadd local choice_shares "\checkmark" : `model'
}
esttab low1 low2 low3 using `"`tex_dir'/table_bargainingCCEI_bothlow_M.tex"', replace ///
    b(3) se(3) star(+ 0.1 * 0.05 ** 0.01) label nogap compress noomitted substitute(\_ _) ///
    keep(HighCCEI_both_low M_ccei) order(HighCCEI_both_low M_ccei) ///
    stats(N r2 fixed_effects characteristics choice_shares, fmt(0 3) ///
        labels("N" "R-squared" "Fixed effects" "Individual and friendship controls" "Corner/midpoint share controls")) ///
    fragment nomtitles nonumbers nolines prefoot("\midrule") postfoot("\bottomrule")

copy `"`tex_dir'/table_bargainingCCEI_bothlow_M.tex"' ///
    `"`code_dir'/../Overleaf/tables_2025/table_bargainingCCEI_bothlow_M.tex"', replace
restore
di as result "SUCCESS: exported Table A6 on Table 3's exact sample; N=2560, clusters=64."
log close ties_low

* Table A7: Collective outcomes; Low--High is the omitted category.
* Run this section independently from Code; it prepares and estimates its inputs.
set more off
adopath ++ "programs"
local out "results/collective_quality"
cap mkdir "results"
cap mkdir "`out'"
cap mkdir "results/tables"
cap mkdir "Logs"
capture log close collective_categories
log using "Logs/5_collective_categories.log", name(collective_categories) text replace
do "programs/build_collective_analysis_sample.do" "`c(pwd)'/data"

* Appendix categorical table: Low--High is omitted in all three specifications.
tempname coefficients diagnostics
tempfile coefficient_results model_results
postfile `coefficients' str4 outcome byte column str12 term ///
    double estimate double se double p using `coefficient_results'
postfile `diagnostics' str4 outcome byte column double r2 ///
    long n long clusters using `model_results'
foreach outcome in ccei ceiv {
    forvalues column=1/3 {
        local controls ""
        if `column'>1 local controls "$t5_group $t5_friend $t5_share"
        local fe class_fe
        if `column'==3 local fe pair_fe
        reghdfe `outcome'_g low_low high_high `controls', ///
            absorb(`fe') vce(cluster class_fe)
        assert e(N)==1304 & e(N_clust)==64
        foreach term in low_low high_high {
            quietly lincom `term'
            post `coefficients' ("`outcome'") (`column') ("`term'") ///
                (r(estimate)) (r(se)) (r(p))
        }
        post `diagnostics' ("`outcome'") (`column') (e(r2)) (e(N)) (e(N_clust))
    }
}
postclose `coefficients'
postclose `diagnostics'
preserve
    use `coefficient_results', clear
    export delimited using "`out'/table5_coefficients.csv", replace
    use `model_results', clear
    export delimited using "`out'/table5_diagnostics.csv", replace
restore

local code_dir `"`c(pwd)'"'
local collective_dir "results/collective_quality"
local tex_dir "results/tables"
tempfile collective_diagnostics
import delimited using "`collective_dir'/table5_diagnostics.csv", clear asdouble
keep if inrange(column, 1, 3)
isid outcome column
assert _N == 6 & n == 1304 & clusters == 64 & !missing(r2)
save `collective_diagnostics'
import delimited using "`collective_dir'/table5_coefficients.csv", clear asdouble
keep if inrange(column, 1, 3)
isid outcome column term
assert _N == 12 & inlist(outcome, "ccei", "ceiv")
assert inlist(term, "low_low", "high_high")
assert !missing(estimate, se, p) & se > 0 & inrange(p, 0, 1)
merge m:1 outcome column using `collective_diagnostics', assert(match) nogen
local bs = char(92)
gen str40 b_text = strtrim(string(estimate, "%9.3f")) + "`bs'textsuperscript{" + ///
    cond(p < .01, "**", cond(p < .05, "*", cond(p < .1, "+", ""))) + "}"
gen str14 se_text = "(" + strtrim(string(se, "%9.3f")) + ")"
gen str12 n_text = strtrim(string(n, "%9.0fc"))
gen str12 r2_text = strtrim(string(r2, "%9.3f"))
tempname category_table
file open `category_table' using "`tex_dir'/collective_ccei_ceiv_categories.tex", write replace
file write `category_table' "`bs'begin{tabular}{lccc}`bs'toprule" _n
file write `category_table' "& `bs'multicolumn{3}{c}{Individual CCEI category dummies}`bs'`bs'" _n
file write `category_table' "`bs'cmidrule(lr){2-4}" _n "& (1) & (2) & (3)`bs'`bs'`bs'midrule" _n
foreach outcome in ccei ceiv {
    local panel = cond("`outcome'" == "ccei", "A: Group CCEI", "B: Group CEIV")
    file write `category_table' "`bs'multicolumn{4}{l}{`bs'textit{Panel `panel'}}`bs'`bs'[3pt]" _n
    foreach term in low_low high_high {
        local label = cond("`term'" == "low_low", "Low--Low pair", "High--High pair")
        foreach stat in b_text se_text {
            local row_label = cond("`stat'" == "b_text", "`label'", "")
            file write `category_table' "`row_label'"
            forvalues column = 1/3 {
                quietly levelsof `stat' if outcome == "`outcome'" & column == `column' & term == "`term'", local(cell) clean
                file write `category_table' " & `cell'"
            }
            file write `category_table' "`bs'`bs'" _n
        }
    }
    foreach stat in n_text r2_text {
        local label = cond("`stat'" == "n_text", "Observations", "R-squared")
        file write `category_table' "`label'"
        forvalues column = 1/3 {
            quietly levelsof `stat' if outcome == "`outcome'" & column == `column' & term == "high_high", local(cell) clean
            file write `category_table' " & `cell'"
        }
        file write `category_table' "`bs'`bs'" _n
    }
    file write `category_table' "`bs'midrule" _n
}
file write `category_table' "Fixed effects & Class & Class & Pair`bs'`bs'" _n
file write `category_table' "Student and friendship controls &  & " (char(36)) "`bs'checkmark" ///
    (char(36)) " & " (char(36)) "`bs'checkmark" (char(36)) "`bs'`bs'" _n
file write `category_table' "Corner/midpoint share controls &  & " (char(36)) "`bs'checkmark" ///
    (char(36)) " & " (char(36)) "`bs'checkmark" (char(36)) "`bs'`bs'" _n
file write `category_table' "`bs'bottomrule`bs'end{tabular}" _n
file close `category_table'
copy "`tex_dir'/collective_ccei_ceiv_categories.tex" "../Overleaf/tables_2025/collective_ccei_ceiv_categories.tex", replace
di as result "SUCCESS: exported collective-outcomes appendix table with Low--High omitted."
log close collective_categories


********************************************************************************
* Figure A7: Table 3 Columns (2), (3), (5), (6), with FE as a Shapley block.
* Run independently from Code. 6 exports separate panels for the manuscript.
* All 32 coalitions use the same sample; contributions sum to overall R-squared.
********************************************************************************
clear
set more off
cap mkdir "Logs"
capture log close distance_shapley_review
log using "Logs/5_shapley_distance_review.log", name(distance_shapley_review) text replace
use "data/panel_individual.dta", clear
assert n_ccei_donors == 651 & !missing(M_ccei)
keep if !missing(Ihat_ig)
assert _N == 2560
local characteristics "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"
local gender "female_i_male_j male_i_female_j"
foreach variable in Ihat_ig HighCCEI_both_high ccei_gap_ij M_ccei `characteristics' `shares' `gender' {
    assert !missing(`variable')
}
tempfile review_results
tempname review_output coalition_r2
postfile `review_output' byte column str40 block double shapley_value double shapley_percent ///
    double total_r2 double focal_beta double focal_se int N int clusters using `review_results', replace
foreach column in 2 3 5 6 {
    local focal "HighCCEI_both_high"
    if `column' > 3 local focal "ccei_gap_ij"
    local fe "class"
    local block3 "`characteristics' `gender'"
    local singleton_option ""
    if inlist(`column', 3, 6) {
        local fe "id_fe"
        local block3 "`characteristics'"
        local singleton_option "keepsingletons"
    }
    quietly reghdfe Ihat_ig `focal' M_ccei `block3' `shares', ///
        absorb(`fe') vce(cluster class) `singleton_option'
    assert e(N) == 2560 & e(N_clust) == 64
    scalar review_r2 = e(r2)
    scalar review_beta = _b[`focal']
    scalar review_se = _se[`focal']
    local block1 "`focal'"
    local block2 "M_ccei"
    local block4 "`shares'"
    matrix `coalition_r2' = J(32, 1, 0)
    forvalues mask = 1/31 {
        local rhs ""
        forvalues block = 1/4 {
            if mod(floor(`mask' / 2^(`block' - 1)), 2) local rhs "`rhs' `block`block''"
        }
        if `mask' >= 16 quietly reghdfe Ihat_ig `rhs', absorb(`fe') vce(cluster class) `singleton_option'
        else quietly regress Ihat_ig `rhs', vce(cluster class)
        assert e(N) == 2560
        matrix `coalition_r2'[`mask' + 1, 1] = e(r2)
    }
    assert abs(e(r2) - review_r2) < 1e-10
    assert abs(_b[`focal'] - review_beta) < 1e-10
    assert abs(_se[`focal'] - review_se) < 1e-10
    local label1 "Rationality"
    local label2 "M benchmark"
    local label3 "Individual/Friendship"
    local label4 "Corner/Midpoint shares"
    local label5 "Class FE"
    if inlist(`column', 3, 6) local label5 "Individual FE"
    scalar review_sum = 0
    forvalues block = 1/5 {
        scalar contribution = 0
        forvalues mask = 0/31 {
            if !mod(floor(`mask' / 2^(`block' - 1)), 2) {
                local size = 0
                forvalues other = 1/5 {
                    local size = `size' + mod(floor(`mask' / 2^(`other' - 1)), 2)
                }
                local weight = cond(inlist(`size', 0, 4), 1/5, cond(inlist(`size', 1, 3), 1/20, 1/30))
                scalar increment = `coalition_r2'[`mask' + 1 + 2^(`block' - 1), 1] - `coalition_r2'[`mask' + 1, 1]
                assert increment > -1e-10
                scalar contribution = contribution + `weight' * increment
            }
        }
        scalar review_sum = review_sum + contribution
        post `review_output' (`column') ("`label`block''") ///
            (contribution) (100 * contribution / review_r2) (review_r2) ///
            (review_beta) (review_se) (2560) (64)
    }
    assert abs(review_sum - review_r2) < 1e-10
}
postclose `review_output'
use `review_results', clear
isid column block
bysort column: egen double share_sum = total(shapley_percent)
assert abs(share_sum - 100) < 1e-8
drop share_sum
cap mkdir "results/figure_A7_review"
export delimited using "results/figure_A7_review/table3_shapley.csv", replace
list column block shapley_value shapley_percent total_r2, noobs sepby(column) abbreviate(25)
di as result "SUCCESS: Figure A7; four exact five-block decompositions, N=2560, clusters=64."
log close distance_shapley_review


********************************************************************************
* Figures A10 and A11 estimates: exact overall-R2 Shapley and multinomial AMEs.
* Run this section independently from Code. 6 draws the appendix panels.
********************************************************************************
set more off
adopath ++ "programs"
local out "results/collective_quality"
local extension_out "`out'/extensions"
cap mkdir "results"
cap mkdir "`out'"
cap mkdir "`extension_out'"
cap mkdir "Logs"
capture log close collective_appendix
log using "Logs/5_collective_outcomes.log", name(collective_appendix) text replace
do "programs/build_collective_analysis_sample.do" "`c(pwd)'/data"

* Appendix multinomial-logit AMEs: class FE and class FE with full controls.
tempname ames
tempfile ame_results
postfile `ames' str13 specification byte outcome str7 member double estimate double se double low ///
    double high double share long n long pairs long clusters using `ame_results'
foreach specification in class_fe full_controls {
    local controls ""
    if "`specification'"=="full_controls" {
        local controls "controls($t5_group $t5_friend $t5_share)"
    }
    collective_multinomial_ame joint_category, base(1) `controls'
    assert r(n)==1304 & r(pairs)==652 & r(clusters)==64
    matrix effects = r(effects)
    * Independently evaluate derivatives of fitted category probabilities.
    forvalues member=1/2 {
        local x ccei_max_01
        if `member'==2 local x ccei_min_01
        gen double original_x = `x'
        replace `x' = original_x+1e-5
        forvalues category=1/4 {
            predict double hi`category', pr outcome(`category')
        }
        replace `x' = original_x-1e-5
        forvalues category=1/4 {
            predict double lo`category', pr outcome(`category')
            gen double derivative = (hi`category'-lo`category')/(2e-5)
            quietly summarize derivative, meanonly
            assert abs(r(mean)-effects[`category',4*(`member'-1)+1])<1e-8
            drop derivative hi`category' lo`category'
        }
        replace `x' = original_x
        drop original_x
    }
    forvalues category=1/4 {
        forvalues member=1/2 {
            local name = cond(`member'==1,"maximum","minimum")
            local start = 4*(`member'-1)
            post `ames' ("`specification'") (`category') ("`name'") ///
                (effects[`category',`start'+1]) (effects[`category',`start'+2]) ///
                (effects[`category',`start'+3]) (effects[`category',`start'+4]) ///
                (effects[`category',9]) (1304) (652) (64)
        }
    }
}
postclose `ames'
preserve
    use `ame_results', clear
    export delimited using "`out'/joint_outcome_ame.csv", replace
restore
local student "$t5_group"
local friendship "$t5_friend"
local shares "$t5_share"

* Figure A10: exact overall-R2 decomposition of Figure 8's controlled LPMs.
* Five blocks, including fixed effects, imply 32 coalitions on the same sample.
preserve
    local block1 "ccei_max_01 ccei_min_01"
    local block2 "`student'"
    local block3 "`friendship'"
    local block4 "`shares'"
    foreach x of varlist `block1' `block2' `block3' `block4' class_fe pair_fe joint_category {
        assert !missing(`x')
    }
    egen byte pairtag = tag(group_id)
    quietly count if pairtag
    assert r(N)==652
    local label1 "Individual CCEI"
    local label2 "Student characteristics"
    local label3 "Friendship measures"
    local label4 "Corner/Midpoint shares"
    local label5 "Fixed effects"
    tempname output fits coalitions coalition_r2
    tempfile decomposition full_models subset_models
    postfile `output' str13 specification byte outcome str32 block double shapley_value ///
        double shapley_percent double total_r2 int n pairs clusters using `decomposition'
    postfile `fits' str13 specification byte outcome double total_r2 double within_r2 double share ///
        double maximum_beta double maximum_se double minimum_beta double minimum_se int n pairs clusters using `full_models'
    postfile `coalitions' str13 specification byte outcome byte mask double r2 int n using `subset_models'
    gen byte indicator = .
    foreach specification in full_controls pair_fe {
        local fe class_fe
        if "`specification'"=="pair_fe" local fe pair_fe
        forvalues outcome=1/4 {
            quietly replace indicator = joint_category==`outcome'
            quietly summarize indicator
            scalar share = r(mean)
            quietly reghdfe indicator `block1' `block2' `block3' `block4', ///
                absorb(`fe') vce(cluster class_fe)
            assert e(N)==1304 & e(N_clust)==64 & e(df_r)==63
            scalar target_r2 = e(r2)
            scalar maximum_beta = _b[ccei_max_01]
            scalar maximum_se = _se[ccei_max_01]
            scalar minimum_beta = _b[ccei_min_01]
            scalar minimum_se = _se[ccei_min_01]
            post `fits' ("`specification'") (`outcome') (e(r2)) (e(r2_within)) ///
                (share) (maximum_beta) (maximum_se) (minimum_beta) (minimum_se) (1304) (652) (64)
            matrix `coalition_r2' = J(32,1,0)
            post `coalitions' ("`specification'") (`outcome') (0) (0) (1304)
            forvalues mask=1/31 {
                local rhs ""
                forvalues block=1/4 {
                    if mod(floor(`mask'/2^(`block'-1)),2) local rhs "`rhs' `block`block''"
                }
                if `mask'>=16 quietly reghdfe indicator `rhs', absorb(`fe') vce(cluster class_fe)
                else quietly regress indicator `rhs', vce(cluster class_fe)
                assert e(N)==1304
                matrix `coalition_r2'[`mask'+1,1] = e(r2)
                post `coalitions' ("`specification'") (`outcome') (`mask') (e(r2)) (e(N))
            }
            assert abs(e(r2)-target_r2)<1e-10
            assert abs(_b[ccei_max_01]-maximum_beta)<1e-10
            assert abs(_se[ccei_max_01]-maximum_se)<1e-10
            assert abs(_b[ccei_min_01]-minimum_beta)<1e-10
            assert abs(_se[ccei_min_01]-minimum_se)<1e-10
            scalar contribution_sum = 0
            forvalues block=1/5 {
                scalar contribution = 0
                forvalues mask=0/31 {
                    if !mod(floor(`mask'/2^(`block'-1)),2) {
                        local size = 0
                        forvalues other=1/5 {
                            local size = `size'+mod(floor(`mask'/2^(`other'-1)),2)
                        }
                        local weight = cond(inlist(`size',0,4),1/5,cond(inlist(`size',1,3),1/20,1/30))
                        scalar increment = `coalition_r2'[`mask'+1+2^(`block'-1),1]-`coalition_r2'[`mask'+1,1]
                        assert increment>=-1e-10
                        scalar contribution = contribution+`weight'*increment
                    }
                }
                scalar contribution_sum = contribution_sum+contribution
                post `output' ("`specification'") (`outcome') ("`label`block''") ///
                    (contribution) (100*contribution/target_r2) (target_r2) (1304) (652) (64)
            }
            assert abs(contribution_sum-target_r2)<1e-10
        }
    }
    postclose `output'
    postclose `fits'
    postclose `coalitions'
    use `decomposition', clear
    isid specification outcome block
    bysort specification outcome: egen double share_sum = total(shapley_percent)
    assert abs(share_sum-100)<1e-8
    drop share_sum
    export delimited using "`extension_out'/joint_outcome_shapley_r2.csv", replace
    list specification outcome block shapley_value shapley_percent total_r2, noobs sepby(specification outcome) abbreviate(24)
    use `full_models', clear
    export delimited using "`extension_out'/joint_outcome_shapley_models.csv", replace
    use `subset_models', clear
    export delimited using "`extension_out'/joint_outcome_shapley_coalitions.csv", replace
restore

di as result "SUCCESS: A10 decompositions and A11 multinomial AMEs validated."
log close collective_appendix

********************************************************************************
* Figure A12 estimates: alternative individual HM and RevMaxMPI rationality.
* Run this section independently from Code. 6 draws the four appendix panels.
********************************************************************************
set more off
adopath ++ "programs"
local out "results/collective_quality/extensions"
cap mkdir "results"
cap mkdir "results/collective_quality"
cap mkdir "`out'"
cap mkdir "Logs"
capture log close collective_alternatives
log using "Logs/5_collective_alternatives.log", name(collective_alternatives) text replace
do "programs/build_collective_analysis_sample.do" "`c(pwd)'/data"
local controls "$t5_group $t5_friend $t5_share"

* Alternative-index LPMs match Figure 8; hold CEIV calibration fixed.
tempname alternatives
tempfile alternative_results
postfile `alternatives' str8 measure str13 specification byte outcome str7 member ///
    double estimate se low high using `alternative_results'
foreach measure in HM MaxMPI {
    preserve
        if "`measure'"=="HM" {
            replace ccei_max_01 = 10*(1-min(hm_i,hm_j)/18)
            replace ccei_min_01 = 10*(1-max(hm_i,hm_j)/18)
            gen byte alternative_category = 1+(hm_g==0)+2*ceiv_at1
        }
        else {
            replace ccei_max_01 = 10*(1-min(maxmpi_i,maxmpi_j))
            replace ccei_min_01 = 10*(1-max(maxmpi_i,maxmpi_j))
            gen byte alternative_category = 1+(maxmpi_g<=1e-9)+2*ceiv_at1
        }
        assert alternative_category==joint_category
        foreach specification in class_fe full_controls pair_fe {
            local regressors "`controls'"
            if "`specification'"=="class_fe" local regressors ""
            local fe class_fe
            if "`specification'"=="pair_fe" local fe pair_fe
            collective_linear_ame alternative_category, controls(`regressors') fe(`fe')
            assert r(n)==1304 & r(pairs)==652 & r(clusters)==64
            matrix effects = r(effects)
            forvalues category=1/4 {
                forvalues member=1/2 {
                    local name = cond(`member'==1,"maximum","minimum")
                    local start = 4*(`member'-1)
                    post `alternatives' ("`measure'") ("`specification'") (`category') ("`name'") ///
                        (effects[`category',`start'+1]) (effects[`category',`start'+2]) ///
                        (effects[`category',`start'+3]) (effects[`category',`start'+4])
                }
            }
        }
    restore
}
postclose `alternatives'
use `alternative_results', clear
export delimited using "`out'/alternative_linear_ame.csv", replace
di as result "SUCCESS: A12 alternative-index LPMs validated on the common sample."
log close collective_alternatives
