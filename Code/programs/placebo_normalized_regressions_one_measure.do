clear all
set more off
set matsize 8000

args measure
if !inlist("`measure'", "ccei", "hm", "maxmpi") exit 198

local code_dir "`c(pwd)'"
local replication_dir "`code_dir'"
local data_dir "`replication_dir'/data"
local measure_dir = cond("`measure'"=="ccei", "placebo_normalized", "placebo_normalized_`measure'")
local actual_var = cond("`measure'"=="ccei", "Ihat_ig", cond("`measure'"=="hm", "Ihat_hm_ig", "Ihat_maxmpi_ig"))
local high_var = cond("`measure'"=="ccei", "HighCCEI_both_high", cond("`measure'"=="hm", "HighHM_both_high", "HighMaxMPI_both_high"))
local low_var = cond("`measure'"=="ccei", "HighCCEI_both_low", cond("`measure'"=="hm", "HighHM_both_low", "HighMaxMPI_both_low"))
local gap_var = cond("`measure'"=="ccei", "ccei_gap_ij", cond("`measure'"=="hm", "hm_gap_ij", "maxmpi_gap_ij"))
local gap_suffix = cond("`measure'"=="ccei", "_cceidiff", cond("`measure'"=="hm", "_hmdiff", "_revmaxmpidiff"))
local index_label = cond("`measure'"=="ccei", "CCEI", cond("`measure'"=="hm", "HM Index", "RevMaxMPI"))
local diagonal_tolerance = cond("`measure'"=="ccei", 1e-4, 2e-6)
local input_file "`code_dir'/results/`measure_dir'/placebo_normalized_member_wave.dta"
local output_dir "`code_dir'/results/`measure_dir'/regressions"
local table_dir "`code_dir'/results/tables/`measure_dir'"
cap mkdir "`code_dir'/results"
cap mkdir "`code_dir'/results/`measure_dir'"
cap mkdir "`output_dir'"
cap mkdir "`code_dir'/results/tables"
cap mkdir "`table_dir'"

confirm file "`data_dir'/panel_individual.dta"
confirm file "`data_dir'/base_raw.dta"
confirm file "`data_dir'/end_raw.dta"
confirm file "`input_file'"

tempfile placebo_member_string_ids
use "`input_file'", clear
tostring group_id, replace format(%14.0f)
tostring id, replace format(%7.0f)
save `placebo_member_string_ids'

use "`data_dir'/panel_individual.dta", clear
merge 1:1 group_id post id using `placebo_member_string_ids', ///
    keep(match) nogen keepusing(I_actual M_* Istar_* n_* nvalid_* degfrac_* P_*)

assert abs(I_actual-`actual_var')<`diagonal_tolerance' if !missing(I_actual,`actual_var')
assert missing(I_actual)==missing(`actual_var')

tempfile choice_shares partner_choice_shares baseline_choice_shares
preserve
    use "`data_dir'/base_raw.dta", clear
    keep if game_type == 1
    gen byte post = 0
    gen byte corner_share = (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    save `baseline_choice_shares'

    use "`data_dir'/end_raw.dta", clear
    keep if game_type == 1
    gen byte post = 1
    gen byte corner_share = (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    append using `baseline_choice_shares'
    save `choice_shares'
    rename id partner_id
    save `partner_choice_shares'
restore

merge m:1 id post using `choice_shares', keep(master match) nogen
rename corner_share corner_share_i
rename mid_share mid_share_i
merge m:1 partner_id post using `partner_choice_shares', keep(master match) nogen
rename corner_share corner_share_j
rename mid_share mid_share_j

capture drop corner_share_diff mid_share_diff female_i_male_j male_i_female_j
gen double corner_share_diff = corner_share_i - corner_share_j
gen double mid_share_diff = mid_share_i - mid_share_j
gen byte female_i_male_j = (male_i == 0 & male_j == 1)
gen byte male_i_female_j = (male_i == 1 & male_j == 0)

global pn_group "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff female_i_male_j male_i_female_j"
global pn_group_nogender "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
global pn_friend "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
global pn_missing "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global pn_ra "RA_i RA_diff"
global pn_share "corner_share_i corner_share_diff mid_share_i mid_share_diff"

bysort id: egen byte n_distance = total(!missing(I_actual))
gen byte balanced_pn = (n_distance == 2)
drop n_distance
egen long id_fe = group(id)

label var I_actual "Actual revealed-preference distance"
label var M_all_imp "Mean placebo benchmark"
label var Istar_all_imp "Placebo-normalized distance"
label var `high_var' "\$Higher `index_label'_i\$"
label var `low_var' "\$Higher `index_label'_i\$"
label var `gap_var' "\$`index_label'_i-`index_label'_j\$"

* Main tables.

local variants "both_high both_low diff"
local outcomes "I M Istar"

foreach variant of local variants {
    if "`variant'"=="both_high" {
        local xvar "`high_var'"
        local file_suffix ""
    }
    if "`variant'"=="both_low" {
        local xvar "`low_var'"
        local file_suffix "_bothlow"
    }
    if "`variant'"=="diff" {
        local xvar "`gap_var'"
        local file_suffix "`gap_suffix'"
    }

    foreach outcome of local outcomes {
        if "`outcome'"=="I"     local yvar "I_actual"
        if "`outcome'"=="M"     local yvar "M_all_imp"
        if "`outcome'"=="Istar" local yvar "Istar_all_imp"

        eststo clear
        eststo m1: reghdfe `yvar' `xvar' if balanced_pn, ///
            absorb(class) vce(cluster class)
        eststo m2: reghdfe `yvar' `xvar' $pn_group $pn_friend $pn_missing ///
            if balanced_pn, absorb(class) vce(cluster class)
        eststo m3: reghdfe `yvar' `xvar' $pn_group $pn_friend $pn_missing ///
            $pn_ra $pn_share if balanced_pn, absorb(class) vce(cluster class)
        eststo m4: reghdfe `yvar' `xvar' $pn_group_nogender $pn_friend ///
            $pn_missing $pn_ra $pn_share if balanced_pn, ///
            absorb(id_fe) vce(cluster class)

        foreach m in m1 m2 m3 {
            estadd local fixed_effects "Class" : `m'
        }
        estadd local fixed_effects "Individual" : m4
        foreach m in m2 m3 m4 {
            estadd local individual_controls "\checkmark" : `m'
            estadd local friendship_controls "\checkmark" : `m'
        }
        foreach m in m3 m4 {
            estadd local ra_controls "\checkmark" : `m'
            estadd local share_controls "\checkmark" : `m'
        }

        esttab m1 m2 m3 m4 using ///
            "`table_dir'/primary_`outcome'`file_suffix'.tex", replace ///
            b(3) se(3) stats(N r2 fixed_effects individual_controls ///
            friendship_controls ra_controls share_controls, ///
            labels("N" "R-squared" "Fixed effects" ///
            "Other individual characteristics" "Other friendship characteristics" ///
            "\$RA_i\$ controls" "Corner/midpoint share controls") ///
            fmt(0 3 %9s %9s %9s %9s %9s)) ///
            nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
            substitute(\_ _) keep(`xvar') nomtitles fragment nonumbers nolines ///
            prefoot("\hline") postfoot("\hline \bottomrule")
    }
}

* Accounting checks.

tempname accounting
postfile `accounting' str12 variant str8 donor_pool str8 deg_rule ///
    byte specification double beta_I beta_M beta_Istar identity_error ///
    long N using "`output_dir'/coefficient_identity.dta", replace

local poolrules "all_imp all_drop cls_imp cls_drop"
foreach variant of local variants {
    if "`variant'"=="both_high" local xvar "`high_var'"
    if "`variant'"=="both_low"  local xvar "`low_var'"
    if "`variant'"=="diff"      local xvar "`gap_var'"

    foreach poolrule of local poolrules {
        gettoken pool rule : poolrule, parse("_")
        local rule = subinstr("`rule'","_","",.)
        local yM "M_`poolrule'"
        local yS "Istar_`poolrule'"

        forvalues specification=1/4 {
            if `specification'==1 {
                local rhs "`xvar'"
                local absorb "class"
            }
            if `specification'==2 {
                local rhs "`xvar' $pn_group $pn_friend $pn_missing"
                local absorb "class"
            }
            if `specification'==3 {
                local rhs "`xvar' $pn_group $pn_friend $pn_missing $pn_ra $pn_share"
                local absorb "class"
            }
            if `specification'==4 {
                local rhs "`xvar' $pn_group_nogender $pn_friend $pn_missing $pn_ra $pn_share"
                local absorb "id_fe"
            }

            tempvar common_sample
            gen byte `common_sample'=balanced_pn & ///
                !missing(I_actual,`yM',`yS',`xvar')

            quietly reghdfe I_actual `rhs' if `common_sample', ///
                absorb(`absorb') vce(cluster class)
            local bI=_b[`xvar']
            local sampleN=e(N)
            tempvar esample
            gen byte `esample'=e(sample)

            quietly reghdfe `yM' `rhs' if `esample', ///
                absorb(`absorb') vce(cluster class)
            local bM=_b[`xvar']
            quietly reghdfe `yS' `rhs' if `esample', ///
                absorb(`absorb') vce(cluster class)
            local bS=_b[`xvar']
            local identity_error=`bS'-(`bI'-`bM')
            assert abs(`identity_error')<1e-10

            post `accounting' ("`variant'") ("`pool'") ("`rule'") ///
                (`specification') (`bI') (`bM') (`bS') ///
                (`identity_error') (`sampleN')
            drop `common_sample' `esample'
        }
    }
}
postclose `accounting'

preserve
    use "`output_dir'/coefficient_identity.dta", clear
    export delimited using "`output_dir'/coefficient_identity.csv", replace
restore

* Diagnostics.

preserve
keep group_id post id partner_id class `high_var' `low_var' `gap_var' ///
    I_actual M_* Istar_* ///
        n_* nvalid_* degfrac_* P_* balanced_pn
    save "`output_dir'/placebo_normalized_analysis.dta", replace
    export delimited using "`output_dir'/placebo_normalized_analysis.csv", replace
restore

di as result "Placebo-normalized analysis completed: `measure'."
di as result "Primary TeX tables and accounting checks: `output_dir'"
