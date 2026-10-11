* Build shared CEIV inputs and the collective pair-wave sample. Run from Code.
args data_dir

* CEIV is reported to three decimals; endpoint status must use the solver flag.
import delimited "`data_dir'/../../TalkFile_ceiv_fresh_3dp.csv", stringcols(_all) case(preserve) clear
assert regexm(group_wave_id,"^[0-9]+_[01]$")
gen str20 group_id = substr(group_wave_id,1,strpos(group_wave_id,"_")-1)
gen byte post = real(substr(group_wave_id,-1,1))
assert group_wave_id==group_id+"_"+string(post)
rename (CEIV upper_engine2 seconds flag) (ceiv_g ceiv_upper_engine2 ceiv_seconds ceiv_flag)
destring ceiv_g ceiv_seconds, replace
assert inlist(attained_at_1,"TRUE","FALSE")
gen byte ceiv_at1 = attained_at_1=="TRUE"
assert ceiv_upper_engine2==cond(ceiv_at1,"NA","unsat")
assert ceiv_flag==""
assert ceiv_g==1 if ceiv_at1
label var ceiv_g "Collective CEIV (reported to three decimals)"
label var ceiv_at1 "CEIV no-contraction benchmark attained (source flag)"
drop group_wave_id attained_at_1
isid group_id post
assert _N==1304
assert !missing(group_id,ceiv_g,ceiv_seconds)
assert inrange(ceiv_g,0,1) & ceiv_seconds>=0
tempfile indices
save `indices'

use "`data_dir'/panel_group.dta", clear
isid group_id post
merge 1:1 group_id post using `indices', assert(match) nogen
save "`data_dir'/panel_group_new_indices.dta", replace

do "programs/prepare_collective_sample.do" "`data_dir'"
merge 1:1 group_id post using "`data_dir'/panel_group_new_indices.dta", ///
    keepusing(ceiv_g ceiv_at1) assert(match) nogen
gen double ccei_min = ccei_max-ccei_dist
assert _N==1304
assert abs(ccei_min-min(ccei_i,ccei_j))<1e-7
bysort group_id: assert _N==2 & class_fe==class_fe[1]
foreach x in ccei_g ceiv_g ccei_max ccei_min $t5_group $t5_friend $t5_share {
    assert !missing(`x')
}

* The median pools all individual observations across both waves.
preserve
    keep ccei_max ccei_min
    gen long observation = _n
    rename (ccei_max ccei_min) (individual_ccei1 individual_ccei2)
    reshape long individual_ccei, i(observation) j(member)
    quietly summarize individual_ccei, detail
    local median = r(p50)
    assert individual_ccei!=`median'
restore
sort group_id post
gen byte max_high = ccei_max>`median'
gen byte min_high = ccei_min>`median'
gen byte pair_category = max_high+min_high
assert min_high<=max_high
gen byte low_low = pair_category==0
gen byte high_high = pair_category==2
gen double ccei_max_01 = 10*ccei_max
gen double ccei_min_01 = 10*ccei_min
gen byte joint_category = 1+(ccei_g>=1-1e-9)+2*ceiv_at1
gen double pooled_median = `median'
save "`data_dir'/collective_analysis_sample.dta", replace
