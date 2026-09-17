********************************************************************************
* Reproduce the main bargaining-index regressions with I_ig, then Ihat_ig
*
* Input:
*   data/panel_individual.dta
*   data/base_raw.dta
*   data/end_raw.dta
*
* Output:
*   Tables/table_bargainingCCEI_I_ig.tex
*   Tables/table_bargainingCCEI_Ihat_ig.tex
********************************************************************************

clear all
set more off
set matsize 8000

use "data/panel_individual.dta", clear

foreach required_var in I_ig Ihat_ig HighCCEI class id group_id post partner_id {
    capture confirm variable `required_var'
    if _rc {
        di as error "Required variable is missing: `required_var'"
        exit 111
    }
}

********************************************************************************
* Construct individual and partner corner/midpoint shares
********************************************************************************

tempfile choice_shares partner_choice_shares baseline_choice_shares

preserve
    use "data/base_raw.dta", clear
    keep if game_type == 1
    gen byte post = 0
    gen byte corner_share = ///
        (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = ///
        (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    save `baseline_choice_shares'

    use "data/end_raw.dta", clear
    keep if game_type == 1
    gen byte post = 1
    gen byte corner_share = ///
        (coord_x == 0 | coord_y == 0) if !missing(coord_x, coord_y)
    gen byte mid_share = ///
        (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    append using `baseline_choice_shares'
    save `choice_shares'

    rename id partner_id
    save `partner_choice_shares'
restore

merge m:1 id post using `choice_shares', keep(master match) nogen
rename corner_share corner_share_i
rename mid_share mid_share_i

merge m:1 partner_id post using `partner_choice_shares', ///
    keep(master match) nogen
rename corner_share corner_share_j
rename mid_share mid_share_j

gen double corner_share_diff = corner_share_i - corner_share_j
gen double mid_share_diff = mid_share_i - mid_share_j

gen byte female_i_male_j = (male_i == 0 & male_j == 1)
gen byte male_i_female_j = (male_i == 1 & male_j == 0)

global group_char ///
    "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff female_i_male_j male_i_female_j"

global group_char_no_gender ///
    "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"

global friend_char ///
    "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"

global missing_char ///
    "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"

global RA_char "RA_i RA_diff"
global share_char ///
    "corner_share_i corner_share_diff mid_share_i mid_share_diff"

egen long id_fe = group(id)
sort id group_id post

label var HighCCEI "\$Higher CCEI_i\$"
label var mathscore_i "\$Math score_i\$"
label var mathscore_diff "\$Math score_{diff}\$"
label var inclass_popularity_i "\$In-degree_i\$"
label var inclass_pop_diff "\$In-degree_{diff}\$"
label var female_i_male_j "\$(Female_i, Male_j)\$"
label var male_i_female_j "\$(Male_i, Female_j)\$"
label var RA_diff "Diff in RA"
label var corner_share_i "Corner Share"
label var corner_share_diff "Diff in Corner Share"
label var mid_share_i "Midpoint Share"
label var mid_share_diff "Diff in Midpoint Share"
label var RA_i "Risk Attitude"

********************************************************************************
* Program: same four specifications, index-specific balanced sample
********************************************************************************

capture program drop run_bargaining_table
program define run_bargaining_table
    syntax, Depvar(name) Outfile(string)

    preserve

        * Retain students for whom the selected index is defined in both waves.
        tempvar n_index balanced_index
        bysort id: egen `n_index' = total(!missing(`depvar'))
        gen byte `balanced_index' = (`n_index' == 2)

        quietly count if `balanced_index' == 1
        local n_student_wave = r(N)

        quietly egen byte tag_student = tag(id) if `balanced_index' == 1
        quietly count if tag_student == 1
        local n_students = r(N)

        quietly egen byte tag_pair = tag(group_id) if `balanced_index' == 1
        quietly count if tag_pair == 1
        local n_pairs = r(N)

        di as text "------------------------------------------------------------"
        di as text "Dependent variable: `depvar'"
        di as result "Balanced student-wave observations: `n_student_wave'"
        di as result "Balanced students: `n_students'"
        di as result "Balanced pairs: `n_pairs'"

        eststo clear
        eststo: reghdfe `depvar' HighCCEI ///
            if `balanced_index' == 1, ///
            absorb(class) vce(cluster class)

        eststo: reghdfe `depvar' HighCCEI ///
            $group_char $friend_char $missing_char ///
            if `balanced_index' == 1, ///
            absorb(class) vce(cluster class)

        eststo: reghdfe `depvar' HighCCEI ///
            $group_char $friend_char $missing_char $RA_char $share_char ///
            if `balanced_index' == 1, ///
            absorb(class) vce(cluster class)

        eststo: reghdfe `depvar' HighCCEI ///
            $group_char_no_gender $friend_char $missing_char ///
            $RA_char $share_char ///
            if `balanced_index' == 1, ///
            absorb(id_fe) vce(cluster class)

        esttab, ///
            b(3) se(3) ///
            stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
            nogap compress star(+ 0.1 * 0.05 ** 0.01) ///
            drop(*missing*) label substitute(\_ _)

        esttab using "`outfile'", replace ///
            b(3) se(3) ///
            stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
            nogap compress star(+ 0.1 * 0.05 ** 0.01) ///
            label substitute(\_ _) ///
            keep(HighCCEI mathscore_i mathscore_diff ///
                 inclass_popularity_i inclass_pop_diff ///
                 female_i_male_j male_i_female_j) ///
            nomtitles fragment nonumbers nolines ///
            prefoot("\hline") postfoot("\bottomrule")

    restore
end

********************************************************************************
* Run original index first for reproduction, then the cross-partition index
********************************************************************************

run_bargaining_table, ///
    depvar(I_ig) ///
    outfile("Tables/table_bargainingCCEI_I_ig.tex")

run_bargaining_table, ///
    depvar(Ihat_ig) ///
    outfile("Tables/table_bargainingCCEI_Ihat_ig.tex")

di as text "Finished original and cross-partition bargaining regressions."
