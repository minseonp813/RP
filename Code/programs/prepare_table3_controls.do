* Load the full individual panel and prepare Table 3's exact-choice controls.
* Run from Code. Leaves all student-waves in memory; balanced_t3 marks its sample.
tempfile baseline_shares choice_shares partner_shares
foreach wave in base end {
    use "data/`wave'_raw.dta", clear
    keep if game_type == 1
    gen byte post = ("`wave'" == "end")
    assert !missing(coord_x, coord_y) & coord_x >= 0 & coord_y >= 0
    assert coord_x + coord_y > 0
    bysort id: assert _N == 18
    gen byte corner_share = (coord_x == 0 | coord_y == 0)
    gen byte mid_share = (coord_x == coord_y)
    collapse (mean) corner_share mid_share, by(id post)
    if "`wave'" == "base" save `baseline_shares'
}
append using `baseline_shares'
isid id post
save `choice_shares'
rename id partner_id
save `partner_shares'

use "data/panel_individual.dta", clear
isid id post
merge 1:1 id post using `choice_shares', keep(master match) assert(match using) nogen
rename corner_share corner_share_i
rename mid_share mid_share_i
merge m:1 partner_id post using `partner_shares', keep(master match) assert(match using) nogen
rename corner_share corner_share_j
rename mid_share mid_share_j
gen corner_share_diff = corner_share_i - corner_share_j
gen mid_share_diff = mid_share_i - mid_share_j
gen byte female_i_male_j = (male_i == 0 & male_j == 1)
gen byte male_i_female_j = (male_i == 1 & male_j == 0)
bysort id: egen n_distance = total(!missing(Ihat_ig))
gen byte balanced_t3 = (n_distance == 2)
drop n_distance
egen long id_fe = group(id)
