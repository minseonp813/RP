********************************************************************************
* 99_3_Tables_Appendix_Ihat.do
* Standalone appendix-table replication using Ihat_ig where applicable.
*
* Run this file from C:/Users/hahn0/RP/Code.
********************************************************************************

version 17
clear all
set more off
set matsize 8000

local distance_var "Ihat_ig"

local code_dir `"`c(pwd)'"'

* Minseon/Dropbox version (kept for reference):
* local replication_dir `"C:/Users/minseonp/Dropbox/RP/Code_Replication Package_Upload"'
* local tex_dir `"C:/Users/minseonp/Dropbox/OverleafGit/Group Decision/tables_2025"'

* Byunghun/current repository:
local replication_dir `"`code_dir'/../Code_Replication Package_Upload"'
local data_dir `"`replication_dir'/data"'
local tex_dir `"`code_dir'/results"'
local result_dir `"`code_dir'/results"'
local ihat_dir `"`result_dir'/Ihat"'
cap mkdir `"`tex_dir'"'
cap mkdir `"`result_dir'"'
cap mkdir `"`ihat_dir'"'

capture which reghdfe
if _rc {
    di as error "reghdfe is required. Install it with: ssc install reghdfe"
    exit 199
}
capture which esttab
if _rc {
    di as error "estout is required. Install it with: ssc install estout"
    exit 199
}

********************************************************************************
* Table A1: Correlation Table
********************************************************************************

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
file open corrfile using `"`ihat_dir'/table_correlation_Ihat.tex"', write replace
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

********************************************************************************
* Shared individual-level preparation: Table 3 robustness and Figure A8 input
********************************************************************************

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

********************************************************************************
* Appendix robustness corresponding to Table 3: FGARP
********************************************************************************

bysort id: egen n_f_I_ig = total(!missing(f_I_ig))
gen byte complete_fgarp_id = (n_f_I_ig == 2)
bysort group_id: egen byte balanced_fgarp = min(complete_fgarp_id)
label var HighF_CCEI "\$Higher FGARP_i\$"

eststo clear
eststo: reghdfe f_I_ig HighF_CCEI if balanced_fgarp, absorb(class) vce(cluster class)
eststo: reghdfe f_I_ig HighF_CCEI $ai_group $ai_friend $ai_missing if balanced_fgarp, absorb(class) vce(cluster class)
eststo: reghdfe f_I_ig HighF_CCEI $ai_group $ai_friend $ai_missing $ai_ra $ai_share if balanced_fgarp, absorb(class) vce(cluster class)
eststo: reghdfe f_I_ig HighF_CCEI $ai_group_nogender $ai_friend $ai_missing $ai_ra $ai_share if balanced_fgarp, absorb(id_fe) vce(cluster class)
esttab using `"`tex_dir'/table_bargainingFGARP.tex"', replace ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(HighF_CCEI mathscore_i mathscore_diff inclass_popularity_i inclass_pop_diff female_i_male_j male_i_female_j) ///
    nomtitles fragment nonumbers nolines prefoot("\hline") postfoot("\bottomrule") substitute(\_ _)

********************************************************************************
* Appendix robustness corresponding to Table 3: reversed MaxMPI
********************************************************************************

bysort id: egen n_rev_I_ig = total(!missing(I_rev_max_mpi_ig))
gen byte complete_rev_id = (n_rev_I_ig == 2)
bysort group_id: egen byte balanced_rev = min(complete_rev_id)
label var HighRevMaxMPI "\$Higher RevMaxMPI_i\$"

eststo clear
eststo: reghdfe I_rev_max_mpi_ig HighRevMaxMPI if balanced_rev, absorb(class) vce(cluster class)
eststo: reghdfe I_rev_max_mpi_ig HighRevMaxMPI $ai_group $ai_friend $ai_missing if balanced_rev, absorb(class) vce(cluster class)
eststo: reghdfe I_rev_max_mpi_ig HighRevMaxMPI $ai_group $ai_friend $ai_missing $ai_ra $ai_share if balanced_rev, absorb(class) vce(cluster class)
eststo: reghdfe I_rev_max_mpi_ig HighRevMaxMPI $ai_group_nogender $ai_friend $ai_missing $ai_ra $ai_share if balanced_rev, absorb(id_fe) vce(cluster class)
esttab using `"`tex_dir'/table_bargainingRevMaxMPI.tex"', replace ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(HighRevMaxMPI mathscore_i mathscore_diff inclass_popularity_i inclass_pop_diff female_i_male_j male_i_female_j) ///
    nomtitles fragment nonumbers nolines prefoot("\hline") postfoot("\bottomrule") substitute(\_ _)

* Save the prepared Table 3 sample. Its Shapley decomposition is deliberately
* postponed until after every appendix table has been written, because the
* individual-FE decomposition is computationally intensive.
tempfile table3_shapley_data
save `table3_shapley_data'

********************************************************************************
* Shared pair-level preparation: Table A2, Table 5 robustness, Figure A10 input
********************************************************************************

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

********************************************************************************
* Table A2: Collective CCEI, full results
********************************************************************************

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

********************************************************************************
* Appendix robustness corresponding to Table 5
********************************************************************************

label var f_ccei_pair_max "\$\text{FGARP}_{\text{max},gt}\$"
label var f_ccei_dist "\$\text{FGARP}_{\text{dist},gt}\$"
eststo clear
eststo: reghdfe f_ccei_g f_ccei_pair_max f_ccei_dist, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe f_ccei_g f_ccei_pair_max f_ccei_dist $ag_group $ag_friend, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe f_ccei_g f_ccei_pair_max f_ccei_dist $ag_group $ag_friend $ag_ra $ag_share, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe f_ccei_g f_ccei_pair_max f_ccei_dist $ag_group $ag_friend $ag_ra $ag_share, absorb(pair_fe) vce(cluster class_fe)
esttab using `"`tex_dir'/final_collective_fgarp.tex"', replace ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(f_ccei_pair_max f_ccei_dist) prefoot("\midrule") postfoot("\bottomrule") ///
    nomtitles fragment nonumbers nolines substitute(\_ _)

label var rev_max_mpi_pair_max "\$\text{RevMaxMPI}_{\text{max},gt}\$"
label var rev_max_mpi_dist "\$\text{RevMaxMPI}_{\text{dist},gt}\$"
eststo clear
eststo: reghdfe rev_max_mpi_g rev_max_mpi_pair_max rev_max_mpi_dist, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe rev_max_mpi_g rev_max_mpi_pair_max rev_max_mpi_dist $ag_group $ag_friend, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe rev_max_mpi_g rev_max_mpi_pair_max rev_max_mpi_dist $ag_group $ag_friend $ag_ra $ag_share, absorb(class_fe) vce(cluster class_fe)
eststo: reghdfe rev_max_mpi_g rev_max_mpi_pair_max rev_max_mpi_dist $ag_group $ag_friend $ag_ra $ag_share, absorb(pair_fe) vce(cluster class_fe)
esttab using `"`tex_dir'/final_collective_rev_max_mpi.tex"', replace ///
    b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(rev_max_mpi_pair_max rev_max_mpi_dist) prefoot("\midrule") postfoot("\bottomrule") ///
    nomtitles fragment nonumbers nolines substitute(\_ _)

********************************************************************************
* Figure A10 input: Shapley decomposition of Table 5, Column (4)
********************************************************************************

reghdfe ccei_g ccei_max ccei_dist $ag_group $ag_friend $ag_ra $ag_share, ///
    absorb(pair_fe) vce(cluster class_fe)
scalar table5_col4_r2 = e(r2)

capture drop FE_pair_*
quietly tabulate pair_fe, generate(FE_pair_)
unab table5_pair_dummies : FE_pair_*
local table5_pair_base : word 1 of `table5_pair_dummies'
local G_PAIR_FE : list table5_pair_dummies - table5_pair_base
local G_CCEI_T5 "ccei_max ccei_dist"
local G_CHAR_T5 "$ag_group $ag_friend"
local G_RA_T5 "$ag_ra"
local G_SHARE_T5 "$ag_share"
local TABLE5_GROUPS "`G_CCEI_T5', `G_CHAR_T5', `G_RA_T5', `G_SHARE_T5', `G_PAIR_FE'"

reg ccei_g `G_CCEI_T5' `G_CHAR_T5' `G_RA_T5' `G_SHARE_T5' `G_PAIR_FE', ///
    vce(cluster class_fe)
assert e(N) == 1304
assert abs(e(r2) - table5_col4_r2) < 1e-10
shapley2, stat(r2) group("`TABLE5_GROUPS'")

matrix GCSh = e(shapley)
matrix GCSr = e(shapley_rel)
if rowsof(GCSh) == 1 matrix GCSh = GCSh'
if rowsof(GCSr) == 1 matrix GCSr = GCSr'

preserve
clear
set obs 5
gen str40 block = ""
replace block = "CCEI" in 1
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
    using `"`result_dir'/shapley_collective_ccei.xlsx"', firstrow(variables) replace
export delimited block shapley_value shapley_percent total_r2 ///
    using `"`result_dir'/shapley_collective_ccei.csv"', replace
restore

********************************************************************************
* Figure A8 input: Shapley decomposition of Table 3, Column (4)
* This slow block is last so all six appendix-table fragments exist first.
********************************************************************************

use `table3_shapley_data', clear
bysort id: egen n_main_distance = total(!missing(`distance_var'))
gen byte balanced_main = (n_main_distance == 2)

reghdfe `distance_var' HighCCEI $ai_group_nogender $ai_friend $ai_missing $ai_ra $ai_share ///
    if balanced_main, absorb(id_fe) vce(cluster class)
scalar table3_col4_r2 = e(r2)

capture drop FE_id_*
quietly tabulate id_fe if balanced_main, generate(FE_id_)
unab table3_id_dummies : FE_id_*
local table3_id_base : word 1 of `table3_id_dummies'
local G_INDIVIDUAL_FE : list table3_id_dummies - table3_id_base
local G_CCEI_T3 "HighCCEI"
local G_CHAR_T3 "$ai_group_nogender $ai_friend $ai_missing"
local G_RA_T3 "$ai_ra"
local G_SHARE_T3 "$ai_share"
local TABLE3_GROUPS "`G_CCEI_T3', `G_CHAR_T3', `G_RA_T3', `G_SHARE_T3', `G_INDIVIDUAL_FE'"

reg `distance_var' `G_CCEI_T3' `G_CHAR_T3' `G_RA_T3' `G_SHARE_T3' `G_INDIVIDUAL_FE' ///
    if balanced_main, vce(cluster class)
if `"`distance_var'"' == "I_ig" assert e(N) == 2228
assert abs(e(r2) - table3_col4_r2) < 1e-10

capture which shapley2
if _rc {
    di as error "shapley2 is required. Install it with: ssc install shapley2"
    exit 199
}
shapley2, stat(r2) group("`TABLE3_GROUPS'")
matrix BISh = e(shapley)
matrix BISr = e(shapley_rel)
if rowsof(BISh) == 1 matrix BISh = BISh'
if rowsof(BISr) == 1 matrix BISr = BISr'

preserve
clear
set obs 5
gen str40 block = ""
replace block = "CCEI" in 1
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
    using `"`ihat_dir'/shapley_bargaining_index_Ihat.xlsx"', firstrow(variables) replace
export delimited block shapley_value shapley_percent total_r2 ///
    using `"`ihat_dir'/shapley_bargaining_index_Ihat.csv"', replace
restore

di as result "99_3_Tables_Appendix_Ihat.do completed."
di as result "Ihat-dependent outputs: `ihat_dir'"
di as result "Unchanged outputs: `result_dir'"
