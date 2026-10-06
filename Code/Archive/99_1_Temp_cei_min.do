clear all
set more off
set matsize 8000

local code_dir `"`c(pwd)'"'
local data_dir `"`code_dir'/data"'
local table_dir `"`code_dir'/results/tables"'

foreach required in panel_individual.dta base_raw.dta end_raw.dta {
    confirm file `"`data_dir'/`required'"'
}
foreach command in reghdfe esttab eststo estadd {
    capture which `command'
    if _rc {
        di as error "`command' is required."
        exit 199
    }
}

tempfile t5_shares t5_partner_shares t5_base_shares

* Prepare pair-wave data.
use `"`data_dir'/panel_individual.dta"', clear

preserve
    use `"`data_dir'/base_raw.dta"', clear
    keep if game_type == 1
    gen post = 0
    gen byte corner_share = (coord_x == 0 | coord_y == 0) ///
        if !missing(coord_x, coord_y)
    gen byte mid_share = (coord_x == coord_y) if !missing(coord_x, coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    save `t5_base_shares'

    use `"`data_dir'/end_raw.dta"', clear
    keep if game_type == 1
    gen post = 1
    gen byte corner_share = (coord_x == 0 | coord_y == 0) ///
        if !missing(coord_x, coord_y)
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
foreach v in ccei RA mathscore height outgoing opened agreeable ///
    conscientious stable inclass_n_friends inclass_popularity ///
    corner_share mid_share {
    capture drop `v'_max
    capture drop `v'_dist
    egen `v'_max = rowmax(`v'_i `v'_j)
    gen `v'_dist = abs(`v'_i - `v'_j)
}

gen double ccei_min = ccei_max - ccei_dist
assert abs(ccei_min - min(ccei_i, ccei_j)) < 1e-7 ///
    if !missing(ccei_i, ccei_j)

capture drop male_diff
capture drop friend
gen byte male_diff = (male_i != male_j)
gen byte friend = (friendship >= 1)
bysort group_id post: keep if _n == 1
isid group_id post
egen long class_fe = group(class)
egen long pair_fe = group(group_id)

assert inrange(ccei_g, 0, 1)
assert cei_full == (cei_g >= 1 - 1e-9)
assert ccei_full == (ccei_g >= 1 - 1e-9)

gen byte cei_case3 = 1 if cei_full == 0
replace cei_case3 = 2 if cei_full == 1 & ccei_full == 1
replace cei_case3 = 3 if cei_full == 1 & ccei_full == 0
label define cei_case3_lbl 1 "Pareto-inefficient" ///
    2 "Stable-weight" 3 "Varying-weight"
label values cei_case3 cei_case3_lbl
assert inlist(cei_case3, 1, 2, 3)
quietly count if cei_case3 == 1
assert r(N) == 411
quietly count if cei_case3 == 2
assert r(N) == 510
quietly count if cei_case3 == 3
assert r(N) == 383

global t5_group = "mathscore_max mathscore_dist height_max height_dist male_diff outgoing_max outgoing_dist opened_max opened_dist agreeable_max agreeable_dist conscientious_max conscientious_dist stable_max stable_dist mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global t5_friend = "inclass_n_friends_max inclass_n_friends_dist inclass_popularity_max inclass_popularity_dist friend"
global t5_ra = "RA_max RA_dist"
global t5_share = "corner_share_max corner_share_dist mid_share_max mid_share_dist"

label var ccei_max "\$\text{CCEI}_{\text{max},gt}\$"
label var ccei_min "\$\text{CCEI}_{\text{min},gt}\$"
label var ccei_dist "\$\text{CCEI}_{\text{dist},gt}\$"

* Group CCEI and CEI.

capture program drop fit_ols
program define fit_ols
    syntax varname, SECOND(varname) PREFIX(name)

    forvalues spec = 1/4 {
        local controls
        if `spec' >= 2 local controls "$t5_group $t5_friend"
        if `spec' >= 3 local controls "`controls' $t5_ra $t5_share"

        if `spec' <= 3 {
            quietly reghdfe `varlist' ccei_max `second' `controls', ///
                absorb(class_fe) vce(cluster class_fe)
        }
        else {
            quietly reghdfe `varlist' ccei_max `second' `controls', ///
                absorb(pair_fe) vce(cluster class_fe)
        }
        estimates store `prefix'`spec'
    }
end

capture program drop fit_fraclogit
program define fit_fraclogit, eclass
    syntax varname, SECOND(varname) PREFIX(name)

    forvalues spec = 1/3 {
        local controls
        if `spec' >= 2 local controls "$t5_group $t5_friend"
        if `spec' >= 3 local controls "`controls' $t5_ra $t5_share"

        quietly fracreg logit `varlist' c.ccei_max c.`second' ///
            `controls' i.class_fe, vce(cluster class_fe)
        local model_n = e(N)
        quietly margins, dydx(ccei_max `second') post
        ereturn scalar N_model = `model_n'
        estimates store `prefix'`spec'
    }
end

foreach second in ccei_min ccei_dist {
    if "`second'" == "ccei_min" local stem min
    else local stem dist

    fit_ols ccei_g, second(`second') prefix(ols_ccei_`stem')
    fit_ols cei_g, second(`second') prefix(ols_cei_`stem')
    fit_fraclogit ccei_g, second(`second') prefix(frac_ccei_`stem')
    fit_fraclogit cei_g, second(`second') prefix(frac_cei_`stem')
}

* Fractional-response models.
local pw_tv_controls mathscore_max mathscore_dist outgoing_max outgoing_dist ///
    opened_max opened_dist ///
    agreeable_max agreeable_dist conscientious_max conscientious_dist ///
    stable_max stable_dist mathscore_diff_missing outgoing_diff_missing ///
    opened_diff_missing agreeable_diff_missing conscientious_diff_missing ///
    stable_diff_missing inclass_n_friends_max inclass_n_friends_dist ///
    inclass_popularity_max inclass_popularity_dist friend RA_max RA_dist ///
    corner_share_max corner_share_dist mid_share_max mid_share_dist
local pw_control_means
local j = 0
foreach x of local pw_tv_controls {
    local ++j
    bysort pair_fe: egen double pw_c`j' = mean(`x')
    local pw_control_means `pw_control_means' pw_c`j'
}

bysort pair_fe: egen double pw_mean_max = mean(ccei_max)
bysort pair_fe: egen double pw_mean_min = mean(ccei_min)
bysort pair_fe: egen double pw_mean_dist = mean(ccei_dist)

foreach second in ccei_min ccei_dist {
    if "`second'" == "ccei_min" {
        local stem min
        local focal_means pw_mean_max pw_mean_min
    }
    else {
        local stem dist
        local focal_means pw_mean_max pw_mean_dist
    }

    foreach outcome in ccei_g cei_g {
        if "`outcome'" == "ccei_g" local suffix ccei
        else local suffix cei

        quietly glm `outcome' c.ccei_max c.`second' ///
            $t5_group $t5_friend $t5_ra $t5_share ///
            `focal_means' `pw_control_means' i.post i.class_fe, ///
            family(binomial) link(probit) vce(cluster class_fe) nolog
        local model_n = e(N)
        quietly margins, dydx(ccei_max `second') post
        estadd scalar N_model = `model_n'
        estimates store pw_`suffix'_`stem'
    }
}

capture program drop export_table1
program define export_table1
    syntax, STEM(name) SECOND(varname) OUTfile(string)

esttab ols_ccei_`stem'1 ols_ccei_`stem'2 ols_ccei_`stem'3 ///
    ols_ccei_`stem'4 using `"`outfile'"', ///
    replace b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(ccei_max `second') order(ccei_max `second') ///
    fragment nomtitles nonumbers nolines substitute(\_ _) ///
    prehead("&\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}&\multicolumn{1}{c}{(4)}\\" ///
        "\midrule" ///
        "\multicolumn{5}{l}{\textit{Panel A: Group CCEI -- OLS}}\\") ///
    prefoot("\midrule") postfoot("")

esttab ols_cei_`stem'1 ols_cei_`stem'2 ols_cei_`stem'3 ///
    ols_cei_`stem'4 using `"`outfile'"', ///
    append b(3) se(3) stats(N r2, labels("N" "R-squared") fmt(0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(ccei_max `second') order(ccei_max `second') ///
    fragment nomtitles nonumbers nolines substitute(\_ _) ///
    prehead("\midrule" ///
        "\multicolumn{5}{l}{\textit{Panel B: Group CEI -- OLS}}\\") ///
    prefoot("\midrule") postfoot("")

esttab frac_ccei_`stem'1 frac_ccei_`stem'2 frac_ccei_`stem'3 ///
    pw_ccei_`stem' using `"`outfile'"', ///
    append b(3) se(3) stats(N_model, labels("N") fmt(0)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(ccei_max `second') order(ccei_max `second') ///
    fragment nomtitles nonumbers nolines substitute(\_ _) ///
    prehead("\midrule" ///
        "\multicolumn{5}{l}{\textit{Panel C: Group CCEI -- fractional-response APEs}}\\") ///
    prefoot("\midrule") postfoot("")

esttab frac_cei_`stem'1 frac_cei_`stem'2 frac_cei_`stem'3 ///
    pw_cei_`stem' using `"`outfile'"', ///
    append b(3) se(3) stats(N_model, labels("N") fmt(0)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(ccei_max `second') order(ccei_max `second') ///
    fragment nomtitles nonumbers nolines substitute(\_ _) ///
    prehead("\midrule" ///
        "\multicolumn{5}{l}{\textit{Panel D: Group CEI -- fractional-response APEs}}\\") ///
    prefoot("\midrule") postfoot("\bottomrule")
end

local table1_min `"`table_dir'/temp_cei_min_table_cei_ccei_models.tex"'
local table1_dist `"`table_dir'/temp_cei_min_table_cei_ccei_models_dist.tex"'
export_table1, stem(min) second(ccei_min) outfile(`"`table1_min'"')
export_table1, stem(dist) second(ccei_dist) outfile(`"`table1_dist'"')

* Multinomial logit.
gen double ccei_max_01 = 10 * ccei_max
gen double ccei_min_01 = 10 * ccei_min
gen double ccei_dist_01 = 10 * ccei_dist
label var ccei_max_01 "CCEI max (0.1)"
label var ccei_min_01 "CCEI min (0.1)"
label var ccei_dist_01 "CCEI distance (0.1)"

foreach stem in min dist {
    local second01 ccei_`stem'_01

    forvalues spec = 1/3 {
        local controls
        if `spec' >= 2 local controls "$t5_group $t5_friend"
        if `spec' >= 3 local controls "`controls' $t5_ra $t5_share"

        quietly mlogit cei_case3 c.ccei_max_01 c.`second01' ///
            `controls' i.class_fe, baseoutcome(1) vce(cluster class_fe)
        assert e(converged) == 1
        estadd scalar N_pairs = e(N) / 2
        estimates store joint_ml_`stem'`spec'
    }
}

* Pair fixed effects.
xtset pair_fe post
bysort pair_fe (post): gen byte ml_switch = cei_case3[1] != cei_case3[2]
bysort pair_fe (post): gen double d_mid_max = ///
    mid_share_max[2] - mid_share_max[1]
bysort pair_fe (post): gen double d_mid_dist = ///
    mid_share_dist[2] - mid_share_dist[1]
quietly regress d_mid_max d_mid_dist if post == 1 & ml_switch, noconstant
local gamma = _b[d_mid_dist]
gen double mid_share_orth = mid_share_max - `gamma' * mid_share_dist
quietly summarize mid_share_orth
replace mid_share_orth = mid_share_orth / r(sd)

foreach stem in min dist {
    local second01 ccei_`stem'_01
    quietly xtmlogit cei_case3 c.ccei_max_01 c.`second01' ///
        $t5_group $t5_friend $t5_ra corner_share_max corner_share_dist ///
        mid_share_orth mid_share_dist, fe baseoutcome(1) ///
        vce(cluster class_fe)
    assert e(converged) == 1
    local conditional_r2 = 1 - e(ll) / e(ll_0)
    estadd scalar r2_p = `conditional_r2'
    estadd scalar N_pairs = e(N_g)
    estimates store joint_ml_`stem'4
}

capture program drop export_table2
program define export_table2
    syntax, STEM(name) SECOND(name) OUTfile(string)
    local second01 ccei_`second'_01

esttab joint_ml_`stem'1 joint_ml_`stem'2 joint_ml_`stem'3 ///
    joint_ml_`stem'4 using `"`outfile'"', ///
    replace b(3) se(3) ///
    stats(N N_pairs r2_p, ///
        labels("Pair-wave observations" "Pairs" "Pseudo R-squared") ///
        fmt(0 0 3)) ///
    nogap compress star(+ 0.1 * 0.05 ** 0.01) label ///
    keep(Stable_weight:ccei_max_01 Stable_weight:`second01' ///
        Varying_weight:ccei_max_01 Varying_weight:`second01') ///
    order(ccei_max_01 `second01') ///
    eqlabels("Stable-weight vs. Pareto inefficient" ///
        "Varying-weight vs. Pareto inefficient") ///
    prehead("&\multicolumn{1}{c}{(1)}&\multicolumn{1}{c}{(2)}&\multicolumn{1}{c}{(3)}&\multicolumn{1}{c}{(4)}\\" ///
        "\midrule") ///
    prefoot("\midrule") postfoot("\bottomrule") ///
    fragment nomtitles nonumbers nolines
end

local table2_min `"`table_dir'/temp_cei_min_table_joint_mlogit.tex"'
local table2_dist `"`table_dir'/temp_cei_min_table_joint_mlogit_dist.tex"'
export_table2, stem(min) second(min) outfile(`"`table2_min'"')
export_table2, stem(dist) second(dist) outfile(`"`table2_dist'"')

di as result "Created: `table1_min'"
di as result "Created: `table1_dist'"
di as result "Created: `table2_min'"
di as result "Created: `table2_dist'"
