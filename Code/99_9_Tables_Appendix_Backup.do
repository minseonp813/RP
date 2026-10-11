********************************************************************************
* 99_9_Tables_Appendix_Backup.do: Analyses omitted from the active draft.
* Moved from 5_Tables_Appendix.do on 2026-10-11; run from Code.
* Retains alternative ties-low tables, legacy collective-index/c_Ng tables and
* decompositions, and leave-one-choice-out validation. Historical definitions
* and result paths are preserved. This backup does not copy assets to Overleaf.
********************************************************************************
clear all
set more off
set matsize 8000
local result_dir "results/tables"
cap mkdir "results"
cap mkdir "`result_dir'"

* Recreate the canonical panel and controls previously supplied by 5.
local code_dir `"`c(pwd)'"'
local data_dir `"`code_dir'/data"'
local tex_dir `"`code_dir'/results/tables"'
local distance_var "Ihat_ig"
cap mkdir `"`tex_dir'"'
use "data/panel_individual.dta", clear

global ai_group = "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff female_i_male_j male_i_female_j"
global ai_group_nogender = "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
global ai_friend = "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
global ai_missing = "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global ai_ra = "RA_i RA_diff"
global ai_share = "corner_share_i corner_share_diff mid_share_i mid_share_diff"
label var HighCCEI_both_low "\$Higher CCEI_i\$"
label var mathscore_i "\$Math score_i\$"
label var mathscore_diff "\$Math score_{diff}\$"
label var inclass_popularity_i "\$In-degree_i\$"
label var inclass_pop_diff "\$In-degree_{diff}\$"
label var female_i_male_j "\$(Female_i, Male_j)\$"
label var male_i_female_j "\$(Male_i, Female_j)\$"

* Alternative-index Table 3
foreach measure in hm maxmpi {
    if "`measure'" == "hm" {
        local title "HM"
        local outcome "Ihat_hm_ig"
        local low "HighHM_both_low"
        label var HighHM_both_low "\$Higher (1-HM)_i\$"
    }
    else {
        local title "MaxMPI"
        local outcome "Ihat_maxmpi_ig"
        local low "HighMaxMPI_both_low"
        label var HighMaxMPI_both_low "\$Higher (1-MaxMPI)_i\$"
    }

    capture drop n_alt_distance balanced_alt
    bysort id: egen n_alt_distance = total(!missing(`outcome'))
    gen byte balanced_alt = (n_alt_distance == 2)

    eststo clear
    eststo al1: reghdfe `outcome' `low' if balanced_alt, absorb(class) vce(cluster class)
    * eststo al2: reghdfe `outcome' `low' $ai_group $ai_friend $ai_missing if balanced_alt, absorb(class) vce(cluster class)
    eststo al3: reghdfe `outcome' `low' $ai_group $ai_friend $ai_missing $ai_ra $ai_share if balanced_alt, absorb(class) vce(cluster class)
    eststo al4: reghdfe `outcome' `low' $ai_group_nogender $ai_friend $ai_missing $ai_ra $ai_share if balanced_alt, absorb(id_fe) vce(cluster class)
    foreach m in al1 al3 {
        estadd local fixed_effects "Class" : `m'
    }
    estadd local fixed_effects "Individual" : al4
    foreach m in al3 al4 {
        estadd local individual_controls "\checkmark" : `m'
        estadd local friendship_controls "\checkmark" : `m'
        estadd local ra_controls "\checkmark" : `m'
        estadd local share_controls "\checkmark" : `m'
    }
    esttab al1 al3 al4 using `"`tex_dir'/table_bargaining`title'_bothlow.tex"', replace ///
        b(3) se(3) stats(N r2 fixed_effects individual_controls friendship_controls ra_controls share_controls, ///
        labels("N" "R-squared" "Fixed effects" "Other individual characteristics" "Other friendship characteristics" "\$RA_i\$ controls" "Corner/midpoint share controls") fmt(0 3 %9s %9s %9s %9s %9s)) ///
        nogap compress star(+ 0.1 * 0.05 ** 0.01) label substitute(\_ _) ///
        keep(`low' mathscore_i mathscore_diff inclass_popularity_i inclass_pop_diff female_i_male_j male_i_female_j) ///
        order(`low' mathscore_i mathscore_diff female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff) ///
        nomtitles fragment nonumbers nolines prefoot("\hline") postfoot("\hline \bottomrule")
}

tempfile table3_shapley_data
save `table3_shapley_data'

* Prepare supplementary collective-CCEI tables and Shapley data

use `"`data_dir'/panel_individual.dta"', clear

foreach v in mathscore outgoing opened agreeable conscientious stable {
    replace `v'_i = 0 if missing(`v'_i)
    replace `v'_j = 0 if missing(`v'_j)
}
foreach v in ccei RA mathscore height outgoing opened agreeable conscientious stable inclass_n_friends inclass_popularity corner_share mid_share {
    capture drop `v'_max
    capture drop `v'_dist
    egen `v'_max = rowmax(`v'_i `v'_j)
    gen `v'_dist = abs(`v'_i - `v'_j)
}
capture drop male_diff
capture drop friend
gen male_diff = (male_i != male_j)
gen friend = (friendship >= 1)
bysort group_id post: keep if _n == 1
isid group_id post
egen long class_fe = group(class)
egen long pair_fe = group(group_id)

global ag_group = "mathscore_max mathscore_dist height_max height_dist male_diff outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_max conscientious_dist stable_max stable_dist mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global ag_friend = "inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist friend"
global ag_ra = "RA_max RA_dist"
global ag_share = "corner_share_max corner_share_dist mid_share_max mid_share_dist"

label var ccei_max "\$\text{CCEI}_{\text{max},gt}\$"
label var ccei_dist "\$\text{CCEI}_{\text{dist},gt}\$"
label var mathscore_max "\$\text{Math Score}_{\text{max},gt}\$"
label var mathscore_dist "\$\text{Math Score}_{\text{dist},gt}\$"
label var height_max "Max height"
label var height_dist "Diff. in height"
label var male_diff "Different gender"
label var outgoing_max "Max extraversion"
label var outgoing_dist "Diff. in extraversion"
label var opened_max "Max openness"
label var opened_dist "Diff. in openness"
label var agreeable_max "Max agreeableness"
label var agreeable_dist "Diff. in agreeableness"
label var conscientious_max "Max conscientiousness"
label var conscientious_dist "Diff. in conscientiousness"
label var stable_max "Max emotional stability"
label var stable_dist "Diff. in emotional stability"
label var inclass_n_friends_max "Max number of friends"
label var inclass_n_friends_dist "Diff. in number of friends"
label var inclass_popularity_max "Max in-degree"
label var inclass_popularity_dist "Diff. in in-degree"
label var friend "Friendship tie"
label var RA_max "\$\text{RA}_{\text{max},gt}\$"
label var RA_dist "\$\text{RA}_{\text{dist},gt}\$"
label var corner_share_max "Max corner share"
label var corner_share_dist "Diff. in corner share"
label var mid_share_max "Max midpoint share"
label var mid_share_dist "Diff. in midpoint share"

* Supplementary table: full collective-CCEI specification

eststo clear
eststo: reghdfe ccei_g ccei_max ccei_dist, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe ccei_g ccei_max ccei_dist $ag_group $ag_friend, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe ccei_g ccei_max ccei_dist $ag_group $ag_friend $ag_ra $ag_share, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe ccei_g ccei_max ccei_dist $ag_group $ag_friend $ag_ra $ag_share, absorb(pair_fe) vce(cluster class_fe)
esttab using `"`tex_dir'/final_collective_ccei_full.tex"', replace ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    order(ccei_max ccei_dist mathscore_max mathscore_dist height_max height_dist male_diff outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_max conscientious_dist stable_max stable_dist inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist friend RA_max RA_dist corner_share_max corner_share_dist mid_share_max mid_share_dist) ///
    drop(*_missing) noomitted nobaselevels nomtitles fragment nonumbers nolines ///
    prefoot("\midrule") postfoot("\bottomrule") substitute(\_ _)

* Alternative-index Table 5
gen double hm_r_i = 1 - hm_i / 18
gen double hm_r_j = 1 - hm_j / 18
gen double hm_r_g = 1 - hm_g / 18
gen double maxmpi_r_i = 1 - maxmpi_i
gen double maxmpi_r_j = 1 - maxmpi_j
gen double maxmpi_r_g = 1 - maxmpi_g

foreach measure in hm maxmpi {
    if "`measure'" == "hm" {
        local title "hm"
        local label "HM"
    }
    else {
        local title "revmaxmpi"
        local label "RevMaxMPI"
    }
    egen `measure'_r_max = rowmax(`measure'_r_i `measure'_r_j)
    egen `measure'_r_min = rowmin(`measure'_r_i `measure'_r_j)
    gen double `measure'_r_dist = abs(`measure'_r_i - `measure'_r_j)
    label var `measure'_r_max "\$\text{`label'}_{\text{max},gt}\$"
    label var `measure'_r_min "\$\text{`label'}_{\text{min},gt}\$"
    label var `measure'_r_dist "\$\text{`label'}_{\text{dist},gt}\$"

    foreach form in highlow maxdist {
        if "`form'" == "highlow" local focal "`measure'_r_max `measure'_r_min"
        else local focal "`measure'_r_max `measure'_r_dist"
        eststo clear
        eststo a1: reghdfe `measure'_r_g `focal', absorb(class_fe) vce(cluster class_fe)
        eststo a2: reghdfe `measure'_r_g `focal' $ag_group $ag_friend, absorb(class_fe) vce(cluster class_fe)
        eststo a3: reghdfe `measure'_r_g `focal' $ag_group $ag_friend $ag_ra $ag_share, absorb(class_fe) vce(cluster class_fe)
        eststo a4: reghdfe `measure'_r_g `focal' $ag_group $ag_friend $ag_ra $ag_share, absorb(pair_fe) vce(cluster class_fe)
        foreach m in a1 a2 a3 {
            estadd local fixed_effects "Class" : `m'
        }
        estadd local fixed_effects "Pair" : a4
        foreach m in a2 a3 a4 {
            estadd local student_controls "\checkmark" : `m'
        }
        foreach m in a3 a4 {
            estadd local ra_controls "\checkmark" : `m'
            estadd local share_controls "\checkmark" : `m'
        }
        esttab a1 a2 a3 a4 using `"`tex_dir'/final_collective_`title'_`form'.tex"', replace ///
            b(3) se(3) stats(N r2 fixed_effects student_controls ra_controls share_controls, ///
            labels("N" "R-squared" "Fixed effects" "Student and friendship controls" "RA controls" "Corner/midpoint share controls") fmt(0 3 %9s %9s %9s %9s)) ///
            nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
            keep(`focal') order(`focal') prefoot("\midrule") postfoot("\bottomrule") ///
            nomtitles fragment nonumbers nolines substitute(\_ _)
    }
}

tempfile table5_shapley_data
save `table5_shapley_data'

* Table 5 Shapley decomposition

use `table5_shapley_data', clear
capture drop FE_pair_*
quietly tabulate pair_fe, generate(FE_pair_)
unab table5_pair_dummies : FE_pair_*
local table5_pair_base : word 1 of `table5_pair_dummies'
local G_PAIR_FE : list table5_pair_dummies - table5_pair_base
local G_CHAR_T5 "$ag_group $ag_friend"
local G_RA_T5 "$ag_ra"
local G_SHARE_T5 "$ag_share"

foreach measure in ccei hm maxmpi {
    if "`measure'" == "ccei" {
        local outcome "ccei_g"
        local focal "ccei_max ccei_dist"
        local block_label "CCEI"
        local file_stem "shapley_collective_ccei"
    }
    else if "`measure'" == "hm" {
        local outcome "hm_r_g"
        local focal "hm_r_max hm_r_dist"
        local block_label "HM Index"
        local file_stem "shapley_collective_hm"
    }
    else {
        local outcome "maxmpi_r_g"
        local focal "maxmpi_r_max maxmpi_r_dist"
        local block_label "RevMaxMPI"
        local file_stem "shapley_collective_revmaxmpi"
    }

    reghdfe `outcome' `focal' $ag_group $ag_friend $ag_ra $ag_share, ///
        absorb(pair_fe) vce(cluster class_fe)
    scalar target_r2 = e(r2)
    scalar target_n = e(N)

    local TABLE5_GROUPS "`focal', `G_CHAR_T5', `G_RA_T5', `G_SHARE_T5', `G_PAIR_FE'"
    reg `outcome' `focal' `G_CHAR_T5' `G_RA_T5' `G_SHARE_T5' `G_PAIR_FE', ///
        vce(cluster class_fe)
    assert e(N) == target_n
    assert abs(e(r2) - target_r2) < 1e-10
    shapley2, stat(r2) group("`TABLE5_GROUPS'")

    matrix GCSh = e(shapley)
    matrix GCSr = e(shapley_rel)
    if rowsof(GCSh) == 1 matrix GCSh = GCSh'
    if rowsof(GCSr) == 1 matrix GCSr = GCSr'

    preserve
    clear
    set obs 5
    gen str40 block = ""
    replace block = "`block_label'" in 1
    replace block = "Group/Friendship" in 2
    replace block = "Risk Aversion" in 3
    replace block = "Corner/Midpoint Shares" in 4
    replace block = "Pair FE" in 5
    svmat double GCSh
    rename GCSh1 shapley_value
    svmat double GCSr
    rename GCSr1 shapley_share
    gen shapley_percent = 100 * shapley_share
    egen total_r2 = total(shapley_value)
    export excel block shapley_value shapley_percent total_r2 ///
        using `"`result_dir'/`file_stem'.xlsx"', firstrow(variables) replace
    export delimited block shapley_value shapley_percent total_r2 ///
        using `"`result_dir'/`file_stem'.csv"', replace
    restore
}

* Table 3 Shapley decomposition

use `table3_shapley_data', clear
local G_CHAR_T3 "$ai_group_nogender $ai_friend $ai_missing"
local G_RA_T3 "$ai_ra"
local G_SHARE_T3 "$ai_share"

foreach measure in ccei hm maxmpi {
    if "`measure'" == "ccei" {
        local outcome "`distance_var'"
        local high_var "HighCCEI_both_high"
        local low_var "HighCCEI_both_low"
        local gap_var "ccei_gap_ij"
        local block_label "CCEI"
        local file_stem "shapley_bargaining_index"
        local gap_suffix "_cceidiff"
    }
    else if "`measure'" == "hm" {
        local outcome "Ihat_hm_ig"
        local high_var "HighHM_both_high"
        local low_var "HighHM_both_low"
        local gap_var "hm_gap_ij"
        local block_label "HM Index"
        local file_stem "shapley_bargaining_hm"
        local gap_suffix "_hmdiff"
    }
    else {
        local outcome "Ihat_maxmpi_ig"
        local high_var "HighMaxMPI_both_high"
        local low_var "HighMaxMPI_both_low"
        local gap_var "maxmpi_gap_ij"
        local block_label "RevMaxMPI"
        local file_stem "shapley_bargaining_revmaxmpi"
        local gap_suffix "_revmaxmpidiff"
    }

    capture drop balanced_main n_main_distance FE_id_*
    bysort id: egen n_main_distance = total(!missing(`outcome'))
    gen byte balanced_main = (n_main_distance == 2)
    quietly tabulate id_fe if balanced_main, generate(FE_id_)
    unab table3_id_dummies : FE_id_*
    local table3_id_base : word 1 of `table3_id_dummies'
    local G_INDIVIDUAL_FE : list table3_id_dummies - table3_id_base

    foreach definition in bothhigh bothlow gap {
        if "`definition'" == "bothhigh" {
            local focal "`high_var'"
            local file_suffix ""
        }
        else if "`definition'" == "bothlow" {
            local focal "`low_var'"
            local file_suffix "_bothlow"
        }
        else {
            local focal "`gap_var'"
            local file_suffix "`gap_suffix'"
        }

        reghdfe `outcome' `focal' $ai_group_nogender $ai_friend $ai_missing $ai_ra $ai_share ///
            if balanced_main, absorb(id_fe) vce(cluster class)
        scalar target_r2 = e(r2)
        scalar target_n = e(N)

        local TABLE3_GROUPS "`focal', `G_CHAR_T3', `G_RA_T3', `G_SHARE_T3', `G_INDIVIDUAL_FE'"
        reg `outcome' `focal' `G_CHAR_T3' `G_RA_T3' `G_SHARE_T3' `G_INDIVIDUAL_FE' ///
            if balanced_main, vce(cluster class)
        assert e(N) == target_n
        assert abs(e(r2) - target_r2) < 1e-10
        shapley2, stat(r2) group("`TABLE3_GROUPS'")

        matrix BISh = e(shapley)
        matrix BISr = e(shapley_rel)
        if rowsof(BISh) == 1 matrix BISh = BISh'
        if rowsof(BISr) == 1 matrix BISr = BISr'

        preserve
        clear
        set obs 5
        gen str40 block = ""
        replace block = "`block_label'" in 1
        replace block = "Individual/Friendship" in 2
        replace block = "Risk Aversion" in 3
        replace block = "Corner/Midpoint Shares" in 4
        replace block = "Individual FE" in 5
        svmat double BISh
        rename BISh1 shapley_value
        svmat double BISr
        rename BISr1 shapley_share
        gen shapley_percent = 100 * shapley_share
        egen total_r2 = total(shapley_value)
        export excel block shapley_value shapley_percent total_r2 ///
            using `"`result_dir'/`file_stem'`file_suffix'.xlsx"', firstrow(variables) replace
        export delimited block shapley_value shapley_percent total_r2 ///
            using `"`result_dir'/`file_stem'`file_suffix'.csv"', replace
        restore
    }
}
* c_Ng sensitivity analysis
local cng_dir `"`tex_dir'/c_Ng"'
cap mkdir `"`cng_dir'"'

use `"`data_dir'/panel_individual.dta"', clear

foreach v in mathscore outgoing opened agreeable conscientious stable {
    replace `v'_i = 0 if missing(`v'_i)
    replace `v'_j = 0 if missing(`v'_j)
}
foreach v in ccei RA mathscore height outgoing opened agreeable conscientious stable inclass_n_friends inclass_popularity corner_share mid_share {
    capture drop `v'_max
    capture drop `v'_dist
    egen `v'_max = rowmax(`v'_i `v'_j)
    gen `v'_dist = abs(`v'_i - `v'_j)
}
capture drop male_diff
capture drop friend
gen male_diff = (male_i != male_j)
gen friend = (friendship >= 1)
bysort group_id post: keep if _n == 1
isid group_id post

gen double hm_r_i = 1 - hm_i / 18
gen double hm_r_j = 1 - hm_j / 18
gen double maxmpi_r_i = 1 - maxmpi_i
gen double maxmpi_r_j = 1 - maxmpi_j
gen double c_hm_r_Ng = 1 - c_hm_Ng / 18
gen double c_maxmpi_r_Ng = 1 - c_maxmpi_Ng
foreach measure in hm maxmpi {
    egen `measure'_r_max = rowmax(`measure'_r_i `measure'_r_j)
    gen double `measure'_r_dist = abs(`measure'_r_i - `measure'_r_j)
}

assert !missing(c_Ng)
assert inrange(c_Ng, 0, 1)

egen long class_fe = group(class)
egen long pair_fe = group(group_id)

global cng_group = "mathscore_max mathscore_dist height_max height_dist male_diff outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_max conscientious_dist stable_max stable_dist mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global cng_friend = "inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist friend"
global cng_ra = "RA_max RA_dist"
global cng_share = "corner_share_max corner_share_dist mid_share_max mid_share_dist"

label var c_Ng "\$c_{Ng,t}\$"
label var ccei_max "\$\text{CCEI}_{\text{max},gt}\$"
label var ccei_dist "\$\text{CCEI}_{\text{dist},gt}\$"

quietly summarize c_Ng, detail
di as result "c_Ng observations = " r(N)
di as result "c_Ng mean = " %9.4f r(mean) ", sd = " %9.4f r(sd)
count if c_Ng <= 1e-6
di as result "c_Ng approximately zero = " r(N)

eststo clear
eststo cng1: reghdfe c_Ng ccei_max ccei_dist, absorb(class_fe) vce(cluster class_fe)
estadd local fixed_effects "Class"
estadd local student_controls ""
estadd local ra_controls ""
estadd local share_controls ""
eststo cng2: reghdfe c_Ng ccei_max ccei_dist $cng_group $cng_friend, absorb(class_fe) vce(cluster class_fe)
estadd local fixed_effects "Class"
estadd local student_controls "\checkmark"
estadd local ra_controls ""
estadd local share_controls ""
eststo cng3: reghdfe c_Ng ccei_max ccei_dist $cng_group $cng_friend $cng_ra $cng_share, absorb(class_fe) vce(cluster class_fe)
estadd local fixed_effects "Class"
estadd local student_controls "\checkmark"
estadd local ra_controls "\checkmark"
estadd local share_controls "\checkmark"
eststo cng4: reghdfe c_Ng ccei_max ccei_dist $cng_group $cng_friend $cng_ra $cng_share, absorb(pair_fe) vce(cluster class_fe)
estadd local fixed_effects "Pair"
estadd local student_controls "\checkmark"
estadd local ra_controls "\checkmark"
estadd local share_controls "\checkmark"

esttab cng1 cng2 cng3 cng4 using `"`cng_dir'/final_collective_cNg.tex"', replace ///
    b(3) se(3) ///
    stats(N r2 fixed_effects student_controls ra_controls share_controls, ///
        labels("N" "R-squared" "Fixed effects" ///
            "Student and friendship controls" "RA controls" ///
            "Corner/midpoint share controls") ///
        fmt(0 3 0 0 0 0)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(ccei_max ccei_dist) prefoot("\hline") postfoot("\hline \bottomrule") ///
    nomtitles fragment nonumbers nolines substitute(\_ _)

esttab cng1 cng2 cng3 cng4 using `"`cng_dir'/final_collective_cNg_full.tex"', replace ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    drop(*_missing) noomitted nobaselevels nomtitles fragment nonumbers nolines ///
    prefoot("\midrule") postfoot("\bottomrule") substitute(\_ _)

estimates restore cng4

capture drop FE_pair_*
quietly tabulate pair_fe, generate(FE_pair_)
unab cng_pair_dummies : FE_pair_*
local cng_pair_base : word 1 of `cng_pair_dummies'
local G_PAIR_FE : list cng_pair_dummies - cng_pair_base
local G_CHAR "$cng_group $cng_friend"
local G_RA "$cng_ra"
local G_SHARE "$cng_share"

foreach measure in ccei hm maxmpi {
    if "`measure'" == "ccei" {
        local outcome "c_Ng"
        local focal "ccei_max ccei_dist"
        local block_label "Individual CCEI"
        local file_stem "shapley_collective_cNg"
    }
    else if "`measure'" == "hm" {
        local outcome "c_hm_r_Ng"
        local focal "hm_r_max hm_r_dist"
        local block_label "Individual HM Index"
        local file_stem "shapley_collective_cNg_hm"
    }
    else {
        local outcome "c_maxmpi_r_Ng"
        local focal "maxmpi_r_max maxmpi_r_dist"
        local block_label "Individual RevMaxMPI"
        local file_stem "shapley_collective_cNg_revmaxmpi"
    }

    reghdfe `outcome' `focal' $cng_group $cng_friend $cng_ra $cng_share, ///
        absorb(pair_fe) vce(cluster class_fe)
    scalar target_r2 = e(r2)
    scalar target_n = e(N)

    local CNG_GROUPS "`focal', `G_CHAR', `G_RA', `G_SHARE', `G_PAIR_FE'"
    reg `outcome' `focal' `G_CHAR' `G_RA' `G_SHARE' `G_PAIR_FE', ///
        vce(cluster class_fe)
    assert e(N) == target_n
    assert abs(e(r2) - target_r2) < 1e-10
    shapley2, stat(r2) group("`CNG_GROUPS'")

    matrix CNGSh = e(shapley)
    matrix CNGSr = e(shapley_rel)
    if rowsof(CNGSh) == 1 matrix CNGSh = CNGSh'
    if rowsof(CNGSr) == 1 matrix CNGSr = CNGSr'

    preserve
    clear
    set obs 5
    gen str40 block = ""
    replace block = "`block_label'" in 1
    replace block = "Group/Friendship" in 2
    replace block = "Risk Aversion" in 3
    replace block = "Corner/Midpoint Shares" in 4
    replace block = "Pair FE" in 5
    svmat double CNGSh
    rename CNGSh1 shapley_value
    svmat double CNGSr
    rename CNGSr1 shapley_share
    gen shapley_percent = 100 * shapley_share
    egen total_r2 = total(shapley_value)
    export excel block shapley_value shapley_percent total_r2 ///
        using `"`cng_dir'/`file_stem'.xlsx"', firstrow(variables) replace
    export delimited block shapley_value shapley_percent total_r2 ///
        using `"`cng_dir'/`file_stem'.csv"', replace
    restore
}

* Alternative-index c_Ng tables
foreach measure in hm maxmpi {
    if "`measure'" == "hm" {
        local outcome "c_hm_r_Ng"
        local focal "hm_r_max hm_r_dist"
        local title "hm"
        label var hm_r_max "\$\text{HM}_{\text{max},gt}\$"
        label var hm_r_dist "\$\text{HM}_{\text{dist},gt}\$"
    }
    else {
        local outcome "c_maxmpi_r_Ng"
        local focal "maxmpi_r_max maxmpi_r_dist"
        local title "revmaxmpi"
        label var maxmpi_r_max "\$\text{RevMaxMPI}_{\text{max},gt}\$"
        label var maxmpi_r_dist "\$\text{RevMaxMPI}_{\text{dist},gt}\$"
    }

    eststo clear
    eststo cng1: reghdfe `outcome' `focal', absorb(class_fe) vce(cluster class_fe)
    estadd local fixed_effects "Class"
    estadd local student_controls ""
    estadd local ra_controls ""
    estadd local share_controls ""
    eststo cng2: reghdfe `outcome' `focal' $cng_group $cng_friend, absorb(class_fe) vce(cluster class_fe)
    estadd local fixed_effects "Class"
    estadd local student_controls "\checkmark"
    estadd local ra_controls ""
    estadd local share_controls ""
    eststo cng3: reghdfe `outcome' `focal' $cng_group $cng_friend $cng_ra $cng_share, absorb(class_fe) vce(cluster class_fe)
    estadd local fixed_effects "Class"
    estadd local student_controls "\checkmark"
    estadd local ra_controls "\checkmark"
    estadd local share_controls "\checkmark"
    eststo cng4: reghdfe `outcome' `focal' $cng_group $cng_friend $cng_ra $cng_share, absorb(pair_fe) vce(cluster class_fe)
    estadd local fixed_effects "Pair"
    estadd local student_controls "\checkmark"
    estadd local ra_controls "\checkmark"
    estadd local share_controls "\checkmark"

    esttab cng1 cng2 cng3 cng4 using `"`cng_dir'/final_collective_cNg_`title'.tex"', replace ///
        b(3) se(3) stats(N r2 fixed_effects student_controls ra_controls share_controls, ///
        labels("N" "R-squared" "Fixed effects" "Student and friendship controls" "RA controls" "Corner/midpoint share controls") ///
        fmt(0 3 %9s %9s %9s %9s)) ///
        nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
        keep(`focal') order(`focal') prefoot("\hline") postfoot("\hline \bottomrule") ///
        nomtitles fragment nonumbers nolines substitute(\_ _)
}


********************************************************************************
* Leave-one-choice-out validation: six Table 3 specifications, 18 folds.
* Run independently from Code after programs/calculate_indices_disjoint.R.
* A full 18-fold run is required for the complete table; no reversed directions.
********************************************************************************
clear
set more off
do "programs/hdfe_cluster_vce.do"
local code_dir `"`c(pwd)'"'
local holdout_dir : environment LOCO_OUTPUT_DIR
if `"`holdout_dir'"' == "" local holdout_dir `"`code_dir'/results/tests/disjoint_choice"'
capture confirm file `"`holdout_dir'/run_config.dta"'
if _rc {
    di as text "Hold-out table not regenerated: run programs/calculate_indices_disjoint.R first."
}
else {
    cap mkdir "Logs"
    capture log close leave_one_choice_out
    log using "Logs/99_9_leave_one_choice_out.log", name(leave_one_choice_out) text replace
    use `"`holdout_dir'/run_config.dta"', clear
    assert _N == 1 & version == 3 & planned_folds == 18 & inrange(folds, 1, 18)
    assert training_choices == 17 & heldout_choices == 1 & group_choices == 18
    local folds = folds[1]
    local pairs = n_pairs[1]
    local production = (`folds' == 18 & `pairs' == 652)
    forvalues fold = 1/`folds' {
        local suffix : display %02.0f `fold'
        confirm file `"`holdout_dir'/indices/fold_`suffix'.dta"'
    }
    tempfile table3_panel fitted_results
    use "data/panel_individual.dta", clear
    keep if balanced_t3
    assert _N == 2512
    egen long loco_class_fe = group(class)
    drop corner_share_i corner_share_diff mid_share_i mid_share_diff
    save `table3_panel'
    local individual "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
    local gender "female_i_male_j male_i_female_j"
    local friendship "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
    local missing_controls "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
    local shares "corner_share_i corner_share_diff mid_share_i mid_share_diff"
    local full_controls "`individual' `friendship' `missing_controls' `shares'"
    tempname fitted
    postfile `fitted' byte fold byte column str32 term double coefficient double se ///
        double p double n double r2 double clusters double df_r byte omitted using `fitted_results', replace
    forvalues fold = 1/`folds' {
        local suffix : display %02.0f `fold'
        use `table3_panel', clear
        merge 1:1 id post using `"`holdout_dir'/indices/fold_`suffix'.dta"', ///
            keep(master match) assert(match using) nogen
        assert fold == `fold' & n_donors == `pairs'-1
        assert !missing(ccei_train, partner_ccei_train, M_holdout)
        bysort id: egen valid_waves = total(!missing(I_holdout))
        keep if valid_waves == 2
        assert _N > 0
        local sample_n = _N
        gen byte loco_high = (ccei_train >= partner_ccei_train)
        gen double loco_gap = ccei_train-partner_ccei_train
        gen double corner_share_i = corner_share_train
        gen double corner_share_diff = corner_diff_train
        gen double mid_share_i = mid_share_train
        gen double mid_share_diff = mid_diff_train
        local column = 0
        foreach focal in loco_high loco_gap {
            forvalues spec = 1/3 {
                local ++column
                local controls ""
                local fe "class"
                if `spec' > 1 local controls "`full_controls'"
                if `spec' == 2 local controls "`controls' `gender'"
                if `spec' == 3 local fe "id_fe"
                quietly reghdfe I_holdout `focal' M_holdout `controls', ///
                    absorb(`fe') vce(cluster class) keepmata
                hdfe_cluster_vce loco_class_fe
                assert e(N) == `sample_n'
                assert _se[`focal'] > 0 & _se[M_holdout] > 0
                local fitted_terms : colnames e(b)
                foreach term of local fitted_terms {
                    local p = cond(_se[`term'] > 0, 2*ttail(e(df_r), abs(_b[`term']/_se[`term'])), .)
                    post `fitted' (`fold') (`column') ("`term'") ///
                        (_b[`term']) (_se[`term']) (`p') (e(N)) (e(r2)) (e(N_clust)) (e(df_r)) (_se[`term']==0)
                }
            }
        }
        di as text "Hold-out: estimated fold `fold'/`folds'; N=`sample_n'."
    }
    postclose `fitted'
    use `fitted_results', clear
    isid fold column term
    save `"`holdout_dir'/fold_estimates.dta"', replace
    preserve
        keep if inlist(term, "loco_high", "loco_gap")
        bysort column: assert _N == `folds'
        assert !missing(p) & se > 0
        export delimited using `"`holdout_dir'/focal_fit_inference.csv"', replace
        collapse (p50) n r2 clusters (min) n_min=n (max) n_max=n, by(column)
        forvalues column = 1/6 {
            foreach stat in n r2 clusters n_min n_max {
                quietly summarize `stat' if column == `column', meanonly
                local fmt "%9.0fc"
                if "`stat'" == "r2" local fmt "%9.3f"
                if "`stat'" == "clusters" local fmt "%9.1f"
                local `stat'`column' = strtrim(string(r(mean), "`fmt'"))
            }
        }
    restore
    keep if inlist(term, "loco_high", "loco_gap", "M_holdout")
    bysort term column: egen double median = median(coefficient)
    bysort term column: egen double lower = pctile(coefficient), p(2.5)
    bysort term column: egen double upper = pctile(coefficient), p(97.5)
    bysort term column: gen int n_estimates = _N
    bysort term column: keep if _n == 1
    assert n_estimates == `folds'
    keep term column median lower upper n_estimates
    save `"`holdout_dir'/fold_summary.dta"', replace
    local tex_path `"`holdout_dir'/table_leave_one_choice_out_partial.tex"'
    if `production' local tex_path "results/tables/table_leave_one_choice_out.tex"
    local bs = char(92)
    gen str12 median_text = strtrim(string(median, "%9.3f"))
    gen str12 lower_text = strtrim(string(lower, "%9.3f"))
    gen str12 upper_text = strtrim(string(upper, "%9.3f"))
    local label_loco_high "Higher`bs' CCEI_i"
    local label_loco_gap "CCEI_i-CCEI_j"
    local label_M_holdout "M_{ig}"
    tempname holdout_table
    file open `holdout_table' using `"`tex_path'"', write replace
    file write `holdout_table' "`bs'resizebox{`bs'textwidth}{!}{%" _n
    file write `holdout_table' "`bs'begin{tabular}{lcccccc}`bs'toprule" _n
    file write `holdout_table' "& `bs'multicolumn{3}{c}{Higher-CCEI indicator} & `bs'multicolumn{3}{c}{CCEI difference}`bs'`bs'" _n
    file write `holdout_table' "`bs'cmidrule(lr){2-4}`bs'cmidrule(lr){5-7}" _n
    file write `holdout_table' "Held-out distance & (1) & (2) & (3) & (4) & (5) & (6)`bs'`bs'`bs'midrule" _n
    foreach term in loco_high loco_gap M_holdout {
        foreach stat in median_text lower_text upper_text {
            if "`stat'" == "median_text" file write `holdout_table' (char(36)) "`label_`term''" (char(36))
            if "`stat'" == "lower_text" file write `holdout_table' "2.5th fold percentile"
            if "`stat'" == "upper_text" file write `holdout_table' "97.5th fold percentile"
            forvalues column = 1/6 {
                local cell ""
                quietly levelsof `stat' if term == "`term'" & column == `column', local(cell) clean
                file write `holdout_table' " & `cell'"
            }
            file write `holdout_table' "`bs'`bs'" _n
        }
    }
    file write `holdout_table' "`bs'midrule" _n
    foreach stat in n n_min n_max r2 clusters {
        local label = cond("`stat'" == "n", "Median N", cond("`stat'" == "n_min", "Minimum N", cond("`stat'" == "n_max", "Maximum N", cond("`stat'" == "r2", "Median R-squared", "Median class clusters"))))
        file write `holdout_table' "`label'"
        forvalues column = 1/6 {
            file write `holdout_table' " & ``stat'`column''"
        }
        file write `holdout_table' "`bs'`bs'" _n
    }
    file write `holdout_table' "Fixed effects & Class & Class & Individual & Class & Class & Individual`bs'`bs'" _n
    foreach label in "Student/friendship controls" "Training-choice share controls" {
        file write `holdout_table' "`label' & & " (char(36)) "`bs'checkmark" ///
            (char(36)) " & " (char(36)) "`bs'checkmark" (char(36)) " & & " ///
            (char(36)) "`bs'checkmark" (char(36)) " & " (char(36)) ///
            "`bs'checkmark" (char(36)) "`bs'`bs'" _n
    }
    file write `holdout_table' "Folds & `folds' & `folds' & `folds' & `folds' & `folds' & `folds'`bs'`bs'" _n
    file write `holdout_table' "`bs'bottomrule`bs'end{tabular}}" _n
    file close `holdout_table'
    di as result "SUCCESS: six hold-out specifications in each of `folds' folds; complete individual-fit inference saved."
    log close leave_one_choice_out
}
