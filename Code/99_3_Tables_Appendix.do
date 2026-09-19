clear all
set more off
set matsize 8000

local distance_var "Ihat_ig"
local code_dir `"`c(pwd)'"'

local replication_dir `"`code_dir'"'
local data_dir `"`replication_dir'/data"'
local tex_dir `"`code_dir'/results/tables"'
local result_dir `"`code_dir'/results/tables"'
cap mkdir `"`code_dir'/results"'
cap mkdir `"`tex_dir'"'
cap mkdir `"`result_dir'"'

foreach required in panel_individual.dta base_raw.dta end_raw.dta {
    capture confirm file `"`data_dir'/`required'"'
    if _rc {
        di as error "Missing required input: `data_dir'/`required'"
        exit 601
    }
}

* Table A1

use `"`data_dir'/panel_individual.dta"', clear
keep if post == 0

label var ccei_i "CCEI"
label var RA_i "Risk Attitude"
capture drop analysis_distance
gen double analysis_distance = `distance_var'
label var analysis_distance "Revealed Preference Distance Index"
label var inclass_n_friends_i "Out-Degree"
label var inclass_popularity_i "In-Degree"
label var male_i "Male"
label var height_i "Height"
label var mathscore_i "Math Score"
label var RAT_strict_i "RAT Score"
label var outgoing_i "Outgoing"
label var opened_i "Opened"
label var agreeable_i "Agreeable"
label var conscientious_i "Conscientious"
label var stable_i "Stable"

local col_vars "ccei_i RA_i analysis_distance inclass_n_friends_i inclass_popularity_i"
local group1_vars "ccei_i RA_i analysis_distance"
local group1_label "Experimental Measures"
local group2_vars "inclass_n_friends_i inclass_popularity_i"
local group2_label "Friendship Network"
local group3_vars "male_i height_i"
local group3_label "Demographics"
local group4_vars "mathscore_i RAT_strict_i"
local group4_label "Cognitive Score"
local group5_vars "outgoing_i opened_i agreeable_i conscientious_i stable_i"
local group5_label "Big 5 Personality"
local ncols : word count `col_vars'

local j = 1
foreach var of local col_vars {
    local collabel`j' : variable label `var'
    local ++j
}

cap file close corrfile
file open corrfile using `"`result_dir'/table_correlation.tex"', write replace
file write corrfile "\begin{tabular}{l*{`ncols'}{c}}" _n
file write corrfile "\toprule" _n
file write corrfile " "
forvalues j = 1/`ncols' {
    file write corrfile " & (`j')"
}
file write corrfile " \\" _n
file write corrfile " "
forvalues j = 1/`ncols' {
    file write corrfile " & `collabel`j''"
}
file write corrfile " \\" _n
file write corrfile "\midrule" _n

local row = 1
foreach group in 1 2 3 4 5 {
    file write corrfile "\multicolumn{" (`ncols' + 1) "}{l}{\textit{`group`group'_label':}} \\" _n
    foreach var1 of local group`group'_vars {
        local rowlabel : variable label `var1'
        file write corrfile "\quad `rowlabel'"
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
                file write corrfile " & " %5.3f (`rho') "`stars'"
            }
            else file write corrfile " & "
            local ++col
        }
        file write corrfile " \\" _n
        local ++row
    }
}
file write corrfile "\bottomrule" _n
file write corrfile "\end{tabular}" _n
file close corrfile

* Alternative-index summary
use `"`data_dir'/panel_individual.dta"', clear
gen double hm_r_i = 1 - hm_i / 18
gen double hm_r_g = 1 - hm_g / 18
gen double maxmpi_r_i = 1 - maxmpi_i
gen double maxmpi_r_g = 1 - maxmpi_g

foreach period in 0 1 {
    local suffix = cond(`period' == 0, "base", "end")
    foreach var in hm_r_i Ihat_hm_ig maxmpi_r_i Ihat_maxmpi_ig {
        quietly summarize `var' if post == `period', detail
        foreach stat in mean sd p10 p50 p90 N {
            local `var'_`suffix'_`stat' = r(`stat')
        }
    }
    preserve
        keep if post == `period'
        bysort group_id post: keep if _n == 1
        foreach var in hm_r_g maxmpi_r_g {
            quietly summarize `var', detail
            foreach stat in mean sd p10 p50 p90 N {
                local `var'_`suffix'_`stat' = r(`stat')
            }
        }
    restore
}

capture file close alt_summary
file open alt_summary using `"`tex_dir'/table_alternative_indices_summary.tex"', write replace
local bs = char(92)
file write alt_summary "& Mean & SD & p10 & p50 & p90 & N `bs'`bs'" _n
file write alt_summary "`bs'midrule" _n
foreach period in base end {
    local panel = cond("`period'" == "base", "Panel A: Baseline", "Panel B: Endline")
    file write alt_summary "`bs'multicolumn{7}{l}{`bs'emph{`panel'}} `bs'`bs'" _n
    foreach item in hm_r_i hm_r_g Ihat_hm_ig maxmpi_r_i maxmpi_r_g Ihat_maxmpi_ig {
        if "`item'" == "hm_r_i" local row_label "Individual HM index"
        if "`item'" == "hm_r_g" local row_label "Group HM index"
        if "`item'" == "Ihat_hm_ig" local row_label "HM-based `bs'ensuremath{I_{ig}}"
        if "`item'" == "maxmpi_r_i" local row_label "Individual RevMaxMPI"
        if "`item'" == "maxmpi_r_g" local row_label "Group RevMaxMPI"
        if "`item'" == "Ihat_maxmpi_ig" local row_label "MaxMPI-based `bs'ensuremath{I_{ig}}"
        file write alt_summary "`bs'hspace{1em}`row_label' & " %6.3f (``item'_`period'_mean') " & " %6.3f (``item'_`period'_sd') " & " %6.3f (``item'_`period'_p10') " & " %6.3f (``item'_`period'_p50') " & " %6.3f (``item'_`period'_p90') " & " %9.0fc (``item'_`period'_N') " `bs'`bs'" _n
    }
    if "`period'" == "base" file write alt_summary "`bs'addlinespace" _n
}
file write alt_summary "`bs'bottomrule" _n
file close alt_summary

* Prepare Table 3 Shapley data

use `"`data_dir'/panel_individual.dta"', clear

tempfile choice_shares partner_choice_shares baseline_choice_shares
preserve
    use `"`data_dir'/base_raw.dta"', clear
    keep if game_type == 1
    gen post = 0
    gen byte corner_share = (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    save `baseline_choice_shares'

    use `"`data_dir'/end_raw.dta"', clear
    keep if game_type == 1
    gen post = 1
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

capture drop corner_share_diff
capture drop mid_share_diff
capture drop female_i_male_j
capture drop male_i_female_j
gen corner_share_diff = corner_share_i - corner_share_j
gen mid_share_diff = mid_share_i - mid_share_j
gen female_i_male_j = (male_i == 0 & male_j == 1)
gen male_i_female_j = (male_i == 1 & male_j == 0)

global ai_group = "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff female_i_male_j male_i_female_j"
global ai_group_nogender = "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
global ai_friend = "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
global ai_missing = "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global ai_ra = "RA_i RA_diff"
global ai_share = "corner_share_i corner_share_diff mid_share_i mid_share_diff"
egen long id_fe = group(id)

bysort id: egen n_distance = total(!missing(`distance_var'))
gen byte balanced_t3 = (n_distance == 2)
drop n_distance

label var HighCCEI_both_low "\$Higher CCEI_i\$"
label var mathscore_i "\$Math score_i\$"
label var mathscore_diff "\$Math score_{diff}\$"
label var inclass_popularity_i "\$In-degree_i\$"
label var inclass_pop_diff "\$In-degree_{diff}\$"
label var female_i_male_j "\$(Female_i, Male_j)\$"
label var male_i_female_j "\$(Male_i, Female_j)\$"

* Appendix table: Ties assigned Low
eststo clear
eststo bl1: reghdfe `distance_var' HighCCEI_both_low if balanced_t3, absorb(class) vce(cluster class)
* eststo bl2: reghdfe `distance_var' HighCCEI_both_low $ai_group $ai_friend $ai_missing if balanced_t3, absorb(class) vce(cluster class)
eststo bl3: reghdfe `distance_var' HighCCEI_both_low $ai_group $ai_friend $ai_missing $ai_ra $ai_share if balanced_t3, absorb(class) vce(cluster class)
eststo bl4: reghdfe `distance_var' HighCCEI_both_low $ai_group_nogender $ai_friend $ai_missing $ai_ra $ai_share if balanced_t3, absorb(id_fe) vce(cluster class)

foreach m in bl1 bl3 {
    estadd local fixed_effects "Class" : `m'
}
estadd local fixed_effects "Individual" : bl4
foreach m in bl3 bl4 {
    estadd local individual_controls "\checkmark" : `m'
    estadd local friendship_controls "\checkmark" : `m'
    estadd local ra_controls "\checkmark" : `m'
    estadd local share_controls "\checkmark" : `m'
}

esttab bl1 bl3 bl4 using `"`tex_dir'/table_bargainingCCEI_bothlow.tex"', replace ///
    b(3) se(3) stats(N r2 fixed_effects individual_controls friendship_controls ra_controls share_controls, ///
    labels("N" "R-squared" "Fixed effects" "Other individual characteristics" ///
    "Other friendship characteristics" "\$RA_i\$ controls" "Corner/midpoint share controls") ///
    fmt(0 3 %9s %9s %9s %9s %9s)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label substitute(\_ _) ///
    keep(HighCCEI_both_low mathscore_i mathscore_diff inclass_popularity_i inclass_pop_diff female_i_male_j male_i_female_j) ///
    order(HighCCEI_both_low mathscore_i mathscore_diff female_i_male_j male_i_female_j inclass_popularity_i inclass_pop_diff) ///
    nomtitles fragment nonumbers nolines prefoot("\hline") postfoot("\hline \bottomrule")

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

    * Both-high and continuous-gap models are generated by 99_21 below.
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

* Refresh the individual-change, mover, and robustness tables, retaining Shapley data.
preserve
    do "99_19_individual_ccei_change.do"
    do "99_20_appendix_mover.do"
    do "99_21_appendix_robustness.do"
restore

tempfile table3_shapley_data
save `table3_shapley_data'

* Prepare Table A2 and Table 5 Shapley data

use `"`data_dir'/panel_individual.dta"', clear

tempfile t5_shares t5_partner_shares t5_base_shares
preserve
    use `"`data_dir'/base_raw.dta"', clear
    keep if game_type == 1
    gen post = 0
    gen byte corner_share = (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    save `t5_base_shares'

    use `"`data_dir'/end_raw.dta"', clear
    keep if game_type == 1
    gen post = 1
    gen byte corner_share = (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    append using `t5_base_shares'
    save `t5_shares'
    rename id partner_id
    save `t5_partner_shares'
restore

merge m:1 id post using `t5_shares', keep(master match) nogen
rename corner_share corner_share_i
rename mid_share mid_share_i
merge m:1 partner_id post using `t5_partner_shares', keep(master match) nogen
rename corner_share corner_share_j
rename mid_share mid_share_j

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

* Table A2

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

tempfile t7_shares t7_partner_shares t7_base_shares
preserve
    use `"`data_dir'/base_raw.dta"', clear
    keep if game_type == 1
    gen post = 0
    gen byte corner_share = (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    save `t7_base_shares'

    use `"`data_dir'/end_raw.dta"', clear
    keep if game_type == 1
    gen post = 1
    gen byte corner_share = (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    append using `t7_base_shares'
    save `t7_shares'
    rename id partner_id
    save `t7_partner_shares'
restore

merge m:1 id post using `t7_shares', keep(master match) nogen
rename corner_share corner_share_i
rename mid_share mid_share_i
merge m:1 partner_id post using `t7_partner_shares', keep(master match) nogen
rename corner_share corner_share_j
rename mid_share mid_share_j

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
