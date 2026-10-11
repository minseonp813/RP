clear all
set more off
do "/private/tmp/rp_panel_consolidation_ad3i8_4z/before_code/programs/prepare_collective_sample.do" "/private/tmp/rp_panel_consolidation_ad3i8_4z/old_inputs/data"
sort group_id post
save "/private/tmp/rp_panel_consolidation_ad3i8_4z/collective_before.dta", replace
do "/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code/programs/prepare_collective_sample.do" "/Users/minseonp/Library/CloudStorage/Dropbox/RP/Code/data"
sort group_id post
save "/private/tmp/rp_panel_consolidation_ad3i8_4z/collective_after.dta", replace
