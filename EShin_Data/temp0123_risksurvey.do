log using Risksurvey, replace
use Risk_ByIndiv.dta, clear
tab1 PlayerRisk_q*
tab PlayerRisk_q1 PlayerRisk_q2, row
tab PlayerRisk_q2 PlayerRisk_q3, row
tab PlayerRisk_q1 PlayerRisk_q3, row
recode PlayerRisk_q1 (1=1) (2=1) (3=2) (4=3) (5=3)
recode PlayerRisk_q2 (1=1) (2=1) (3=2) (4=2)

forvalues t=1/3 {
forvalues b=1/2 {
tab PlayerRisk_q3 if PlayerRisk_q2==`b' & PlayerRisk_q1==`t'
}
}
use Risk_Bypair.dta, clear
reg riskq1 riskq2, r cl(class)
reg riskq1 i.riskq3, r cl(class)
reg riskq2 i.riskq3, r cl(class)
log close
log translate Risksurvey.smcl Risksurvey.pdf


use Risk_ByPair.dta, clear

forvalues t=1/2{
reg riskq`t' i.friendship, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q`t')
reg riskq`t' i.riskprefpair, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q`t')
reg riskq`t' i.riskpremiumpair, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q`t')
reg riskq`t' i.riskpremium_rrpair, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q`t')
reg riskq`t' i.riskpremiumpair if ccei_both90==2, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q`t')
reg riskq`t' i.riskpremium_rrpair if ccei_both90==2, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q`t')
reg riskq`t' i.ccei_both90, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q`t')
reg riskq`t' i.sexpair_all, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q`t')
reg riskq`t' i.scorepair, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q`t')
reg riskq`t' i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q`t')
reg riskq`t' i.friendship i.riskprefpair i.sexpair_all i.scorepair i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair
}

g riskq3_bi=cond(riskq3==1,1,0)
replace riskq3_bi=. if riskq3==.

reg riskq3_bi i.friendship, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q3)
reg riskq3_bi i.riskprefpair, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q3)
reg riskq3_bi i.riskpremiumpair, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q3)
reg riskq3_bi i.riskpremium_rrpair, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q3)
reg riskq3_bi i.riskpremiumpair if ccei_both90==2, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q3)
reg riskq3_bi i.riskpremium_rrpair if ccei_both90==2, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q3)
reg riskq3_bi i.ccei_both90, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q3)
reg riskq3_bi i.sexpair_all, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q3)
reg riskq3_bi i.scorepair, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q3)
reg riskq3_bi i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair, r cl(class)
outreg2 using risksurvey.xls, bdec(2) cdec(2) label ctitle(q3)
reg riskq3_bi i.friendship i.riskprefpair i.sexpair_all i.scorepair i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair


forvalues t=1/2 {
reg ccei_col riskq`t', r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(ccei_col)
reg ccei_col riskq`t' i.riskprefpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(ccei_col)
reg ccei_col riskq`t' i.riskpremiumpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(ccei_col)
reg ccei_col riskq`t' i.riskpremium_rrpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(ccei_col)

reg rdu_col riskq`t', r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(rdu_col)
reg riskpref_col riskq`t', r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(riskpref_col)
reg hiriskpref_col riskq`t', r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(hiriskpref_col)
reg riskpremium_col riskq`t' if ccei_col90==1, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(riskpremium_col)
reg hiriskpremium_col riskq`t' if ccei_col90==1, r cl (class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(hiriskpremium_col)
reg riskpremium_rr_col riskq`t' if ccei_col90==1, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(riskpremium_rr_col)
reg hiriskpremium_rr_col riskq`t' if ccei_col90==1, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(hiriskpremium_rr_col)

reg pe riskq`t' if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe)
reg pe ccei_col riskq`t' i.riskprefpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe)
reg pe ccei_col riskq`t' i.riskpremiumpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe)
reg pe ccei_col riskq`t' i.riskpremium_rrpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe)
reg pe_rr riskq`t' if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe_rr)
reg pe_rr ccei_col riskq`t' i.riskprefpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe_rr)
reg pe_rr ccei_col riskq`t' i.riskpremiumpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe_rr)
reg pe_rr ccei_col riskq`t' i.riskpremium_rrpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe_rr)
}
reg ccei_col i.riskq3, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(ccei_col)
reg ccei_col i.riskq3 i.riskprefpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(ccei_col)
reg ccei_col i.riskq3 i.riskpremiumpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(ccei_col)
reg ccei_col i.riskq3 i.riskpremium_rrpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(ccei_col)

reg rdu_col i.riskq3, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(rdu_col)
reg riskpref_col i.riskq3, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(riskpref_col)
reg hiriskpref_col i.riskq3, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(hiriskpref_col)
reg riskpremium_col i.riskq3 if ccei_col90==1, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(riskpremium_col)
reg hiriskpremium_col i.riskq3 if ccei_col90==1, r cl (class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(hiriskpremium_col)
reg riskpremium_rr_col i.riskq3 if ccei_col90==1, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(riskpremium_rr_col)
reg hiriskpremium_rr_col i.riskq3 if ccei_col90==1, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(hiriskpremium_rr_col)

reg pe i.riskq3 if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe)
reg pe ccei_col i.riskq3 i.riskprefpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe)
reg pe ccei_col i.riskq3 i.riskpremiumpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe)
reg pe ccei_col i.riskq3 i.riskpremium_rrpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe)
reg pe_rr i.riskq3 if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe_rr)
reg pe_rr ccei_col i.riskq3 i.riskprefpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe_rr)
reg pe_rr ccei_col i.riskq3 i.riskpremiumpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe_rr)
reg pe_rr ccei_col i.riskq3 i.riskpremium_rrpair i.sexpair_all i.scorepair i.friendship i.outgoingpair i.conscientiouspair i.agreeablepair i.stablepair i.opennesspair if ccei_both90==2, r cl(class)
outreg2 using risksurvey2.xls, bdec(2) cdec(2) label ctitle(pe_rr)
