* Common respondent-wave categories and rationality roles; run from Code.
do "programs/load_risk_survey_panel.do"
gen byte ccei_endpoint = ccei_g>=1-1e-9
gen byte ceiv_endpoint = ceiv_g>=1-1e-9
assert ceiv_endpoint == (CEIV_lower>=1-1e-9)
assert ceiv_endpoint == (CEIV_upper>=1-1e-9)
gen byte joint_category = 1+ccei_endpoint+2*ceiv_endpoint
gen byte ccei_tied = abs(ccei_i-ccei_j)<=1e-9
gen byte role = 1 if ccei_i>ccei_j+1e-9
replace role = 2 if ccei_i<ccei_j-1e-9
label define rationality_role 1 "More rational" 2 "Less rational"
label values role rationality_role
assert missing(role) == ccei_tied
bysort group_id post: assert role[1]+role[2]==3 if !ccei_tied
bysort group_id post: assert missing(Ihat_ig[1]) == missing(Ihat_ig[2])
assert inrange(Ihat_ig,0,1) if !missing(Ihat_ig)
gen byte included = !ccei_tied & !missing(Ihat_ig)
gen byte missing_untied = !ccei_tied & missing(Ihat_ig)
egen byte pairwave_tag = tag(group_id post)
