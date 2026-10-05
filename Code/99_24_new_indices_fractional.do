* Run from Code after 99_23_new_indices.do. Also callable on its own.
* Match the original fractional-response robustness specifications.
local out "results/new_indices"
capture log close fractional
log using "`out'/fractional.log", text replace name(fractional)
do "programs/prepare_collective_sample.do" "`c(pwd)'/data"
merge 1:1 group_id post using "`out'/analysis_sample.dta", ///
    keepusing(ceiv_g ceic_g) assert(match) nogen
gen double ccei_min = ccei_max - ccei_dist
label var ccei_max "\$\text{CCEI}_{\text{max},gt}\$"
label var ccei_min "\$\text{CCEI}_{\text{min},gt}\$"

* Same pair-mean controls as 99_1_Tables_Main.do's panel fractional probit.
local tv_controls ccei_max ccei_min mathscore_max mathscore_dist ///
    outgoing_max outgoing_dist opened_max opened_dist ///
    agreeable_max agreeable_dist conscientious_max conscientious_dist ///
    stable_max stable_dist mathscore_diff_missing outgoing_diff_missing ///
    opened_diff_missing agreeable_diff_missing conscientious_diff_missing ///
    stable_diff_missing inclass_n_friends_max inclass_n_friends_dist ///
    inclass_popularity_max inclass_popularity_dist friend ///
    corner_share_max corner_share_dist mid_share_max mid_share_dist
local means
local j = 0
foreach x of local tv_controls {
    local ++j
    bysort pair_fe: egen double frac_mean`j' = mean(`x')
    local means `means' frac_mean`j'
}

local panel_number = 0
foreach outcome in ccei cei ceiv ceic {
    local ++panel_number
    local panel = char(64 + `panel_number')
    local title = upper("`outcome'")
    forvalues spec = 1/4 {
        local controls ""
        if `spec' >= 2 local controls "$t5_group $t5_friend"
        if `spec' >= 3 local controls "`controls' $t5_share"
        if `spec' <= 3 {
            fracreg logit `outcome'_g c.ccei_max c.ccei_min ///
                `controls' i.class_fe, vce(cluster class_fe)
        }
        else {
            glm `outcome'_g c.ccei_max c.ccei_min ///
                `controls' `means' i.post i.class_fe, ///
                family(binomial) link(probit) vce(cluster class_fe) nolog
        }
        assert e(converged) == 1
        assert e(N) == 1304
        assert e(N_clust) == 64
        local model_n = e(N)
        margins, dydx(ccei_max ccei_min) post
        assert !missing(_b[ccei_max], _b[ccei_min], _se[ccei_max], _se[ccei_min])
        estadd scalar N_model = `model_n'
        estimates store frac_`outcome'`spec'
        estimates save "`out'/table5_fractional_`outcome'_`spec'.ster", replace
    }
    local mode append
    local head ""
    if `panel_number' == 1 {
        local mode replace
        local head "& (1) & (2) & (3) & (4) \\"
    }
    local footer ""
    if `panel_number' == 4 {
        local footer "\midrule Link & Logit & Logit & Logit & Probit \\ Class fixed effects & \checkmark & \checkmark & \checkmark & \checkmark \\ Student and friendship controls & & \checkmark & \checkmark & \checkmark \\ Corner/midpoint share controls & & & \checkmark & \checkmark \\ Pair means and wave effect & & & & \checkmark \\ \bottomrule"
    }
    esttab frac_`outcome'1 frac_`outcome'2 frac_`outcome'3 frac_`outcome'4 ///
        using "`out'/table5_fractional.tex", `mode' ///
        b(3) se(3) stats(N_model, labels("N") fmt(0)) ///
        nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
        keep(ccei_max ccei_min) order(ccei_max ccei_min) ///
        fragment nomtitles nonumbers nolines substitute(\_ _) ///
        prehead("`head'" "\midrule" "\multicolumn{5}{l}{\textit{Panel `panel': Group `title'}}\\") ///
        prefoot("\midrule") postfoot("`footer'")
}
di as result "SUCCESS: all 16 fractional-response models converged; N=1304, clusters=64; finite APEs and SEs."
log close fractional
