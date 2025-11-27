use Risk_ByIndiv_long.dta, clear
drop if (school==112|school==122|school==212|school==222) | temp==1
ren peer_selfich peer_selfish

la var ccei_ind "CCEI"
la var riskpref_ind "Non-parametricRiskAversion"
la var riskpremium_ind "Risk Premium"
la var rdu_ind "1(Risk Type=RDU)"
la var mathscore "Math Score"
la var pbl_all "PBL_All Subjects"
la var pbl_korean "PBL_Korean"
la var pbl_eng "PBL_English"
la var pbl_math "PBL_Math"
la var pbl_science "PBL_Science"
la var pbl_socialsci "PBL_SocialScience"
la var class_sleep "class_sleep"
la var class_participate "class_participate"
la var teacher_prepare "teacher_prepare"

reg ccei_ind t treatment txtreatment, r cl(class)
outreg2 using "PBL_Individual.xls", bdec(3) cdec(3) label replace
reg riskpref_ind t treatment txtreatment, r cl(class)
outreg2 using "PBL_Individual.xls", bdec(3) cdec(3) label append
reg riskpremium_ind t treatment txtreatment, r cl(class)
outreg2 using "PBL_Individual.xls", bdec(3) cdec(3) label append
reg rdu_ind t treatment txtreatment, r cl(class)
outreg2 using "PBL_Individual.xls", bdec(3) cdec(3) label append

# delimit ;
foreach x in pbl_all pbl_korean pbl_eng pbl_math pbl_science pbl_socialsci mathscore 
class_sleep class_participate teacher_prepare teacher_induce class_practical class_belonged
peer_sociable peer_fair peer_helpful peer_selfish peer_reciprocal class_harass
value_fair value_warmglow value_trust value_cowork 
pbleffect_inc~s pbleffect_out~s pbleffect_fri~d pbleffect_con~e pbleffct_school lifesatisfied { ;
reg `x' t treatment txtreatment, r cl(class) ;
outreg2 using "PBL_Individual.xls", bdec(3) cdec(3) append ;
} ;
# delimit cr





use Risk_ByPair_long.dta, clear
drop if alpha_ind==.
replace school=floor(id_new/10000) if school==.
drop if (school==112|school==122|school==212|school==222) | temp==1
sort id_new t
foreach var of varlist sexpair_all {
bysort id_new: replace `var'=`var'[1]
}
egen pbl_cont_col=rowmean(pbl_cont pbl_cont2)

reg ccei_col t treatment txtreatment, r cl(class)
outreg2 using "PBL_Pair.xls", bdec(3) cdec(3) label replace
reg uloss_col t treatment txtreatment, r cl(class)
outreg2 using "PBL_Pair.xls", bdec(3) cdec(3) label append
reg pe t treatment txtreatment, r cl(class)
outreg2 using "PBL_Pair.xls", bdec(3) cdec(3) label append