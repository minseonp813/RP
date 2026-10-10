clear all
set more off
cd "/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code"
adopath ++ "programs"
local a5_code_dir `"`c(pwd)'"'
local a5_tex_dir `"`a5_code_dir'/results/tables"'
cap mkdir "Logs"
cap mkdir `"`a5_tex_dir'"'
capture log close alternative_full
log using "Logs/08_alternative_full.log", name(alternative_full) text replace
preserve
do "programs/prepare_table3_controls.do"

tempfile a5_panel a5_benchmark a5_results
save `a5_panel'
foreach a5_measure in hm maxmpi ra {
    local a5_source "results/benchmarks/`a5_measure'/placebo_normalized_member_wave.dta"
    if "`a5_measure'" == "ra" local a5_source "IminusM_review/outputs/data/ra_placebo_member_wave.dta"
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
    bysort id: egen a5_waves_`a5_measure' = total(!missing(a5_I_`a5_measure'))
    gen byte a5_sample_`a5_measure' = (a5_waves_`a5_measure' == 2)
    if "`a5_measure'" == "ra" replace a5_sample_ra = !missing(a5_I_ra)
    count if a5_sample_`a5_measure'
    assert r(N) == cond("`a5_measure'" == "ra", 2604, 2512)
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
            if "`a5_measure'" == "ra" local a5_singletons "keepsingletons"
        }
        quietly reghdfe a5_I_`a5_measure' `a5_focal' M_`a5_measure' `a5_controls' ///
            if a5_sample_`a5_measure', absorb(`a5_effect') vce(cluster class) `a5_singletons'
        assert e(N) == cond("`a5_measure'" == "ra", 2604, 2512) & e(N_clust) == 64
        local a5_b = _b[`a5_focal']
        local a5_s = _se[`a5_focal']
        local a5_p = 2 * ttail(e(df_r), abs(`a5_b' / `a5_s'))
        local a5_bM = _b[M_`a5_measure']
        local a5_sM = _se[M_`a5_measure']
        local a5_pM = 2 * ttail(e(df_r), abs(`a5_bM' / `a5_sM'))
        quietly summarize a5_I_`a5_measure' if e(sample)
        local a5_ysd = r(sd)
        quietly summarize `a5_focal' if e(sample)
        local a5_xsd = r(sd)
        post a5_post ("`a5_measure'") (`a5_column') (`a5_b') (`a5_s') (`a5_p') ///
            (`a5_bM') (`a5_sM') (`a5_pM') (`a5_ysd') (`a5_xsd') ///
            (`a5_b' * `a5_xsd' / `a5_ysd') (e(N)) (e(N_clust)) (e(r2))
        estimates store a5_`a5_measure'_`a5_column'
    }
    esttab a5_`a5_measure'_1 a5_`a5_measure'_2 a5_`a5_measure'_3 ///
        a5_`a5_measure'_4 a5_`a5_measure'_5 a5_`a5_measure'_6 ///
        using `"`a5_tex_dir'/table_bargaining_alternatives_M.tex"', `a5_write' ///
        b(3) se(3) star(+ 0.1 * 0.05 ** 0.01) label fragment nomtitles nonumbers nolines nogap substitute(\_ _) ///
        keep(`a5_high' `a5_gap' M_`a5_measure') order(`a5_high' `a5_gap' M_`a5_measure') ///
        stats(N r2, fmt(0 3) labels("N" "R-squared")) ///
        prehead("\multicolumn{7}{l}{\emph{Panel `a5_panel_name' (651-donor benchmark)}} \\") ///
        posthead("") prefoot("") postfoot("\midrule")
    local a5_write "append"
}
postclose a5_post
file open a5_footer using `"`a5_tex_dir'/table_bargaining_alternatives_M.tex"', write append
file write a5_footer "Fixed effects & Class & Class & Individual & Class & Class & Individual \\" _n
file write a5_footer "Individual, friendship, and choice-share controls & & \checkmark & \checkmark & & \checkmark & \checkmark \\" _n
file write a5_footer "\bottomrule" _n
file close a5_footer
use `a5_results', clear
isid measure column
assert _N == 18
export delimited using `"`a5_tex_dir'/table_bargaining_alternatives_M.csv"', replace
copy `"`a5_tex_dir'/table_bargaining_alternatives_M.tex"' ///
    `"`a5_code_dir'/../Overleaf/tables_2025/table_bargaining_alternatives_M.tex"', replace
restore
di as result "SUCCESS: exported Table A5; 651 donors for all measures, 64 class clusters."
log close alternative_full


exit, clear
