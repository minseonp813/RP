* Shared pair-wave sample, observed math moderators, and Table 5 controls; run from Code.
use "data/panel_individual.dta", clear
isid id post
assert !missing(ccei_i,ccei_j,mathscore_i_missing)
gen double math_observed = mathscore_i if mathscore_i_missing==0
assert inrange(math_observed,0,5) | missing(math_observed)
gen byte lower_member = ccei_i < ccei_j-1e-9
gen byte ccei_tied = abs(ccei_i-ccei_j)<=1e-9
bysort group_id post: egen byte lower_count = total(lower_member)
assert lower_count == 1-ccei_tied
bysort group_id post: egen byte math_observed_count = count(math_observed)
bysort group_id post: egen double math_pair_mean = mean(math_observed)
replace math_pair_mean = . if math_observed_count<2
gen double math_lower = math_observed if lower_member
bysort group_id post: egen double math_less = max(math_lower)
gen byte math_less_missing = lower_member & missing(math_observed)
bysort group_id post: egen byte math_less_missing_pair = max(math_less_missing)
bysort group_id post: keep if _n==1
isid group_id post
keep group_id post math_pair_mean math_less ccei_tied math_less_missing_pair
tempfile moderators
save `moderators'

do "programs/prepare_collective_sample.do" "`c(pwd)'/data"
merge 1:1 group_id post using "data/panel_group_new_indices.dta", ///
    keepusing(ceiv_g CEIV_lower CEIV_upper) assert(match) nogen
merge 1:1 group_id post using `moderators', assert(match) nogen
assert _N==1304
assert !missing(ccei_g,ceiv_g,ccei_max,ccei_dist,friendship)
assert inlist(friendship,0,1,2)
assert friendship == friendship_i_to_j + friendship_j_to_i
gen double ccei_min = ccei_max-ccei_dist
assert abs(ccei_min-min(ccei_i,ccei_j))<1e-7
gen double ccei_ceiv_gap = ccei_g-ceiv_g
gen byte wave_split = post
gen byte friend_split = friendship
gen byte friend_any_split = friend
foreach m in pair less {
    local score math_pair_mean
    if "`m'"=="less" local score math_less
    egen double median_math_`m' = median(`score'), by(post)
    gen byte math_`m'_split = `score'>=median_math_`m' if !missing(`score')
}
foreach x in $t5_group $t5_friend $t5_share {
    assert !missing(`x')
}
