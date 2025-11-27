cd "C:\Users\LG\Dropbox\RP\Data_Post"
use Risk_ByIndiv_post.dta, clear
append using "C:\Users\LG\Dropbox\RP\Data\Risk_ByIndiv", force
replace t=0 if t==.
bysort id_new partner: g temp=_N

g school=floor(id_new/10000)
drop if school==112|school==122|school==212|school==222
g treatment=cond(school>=111 & school<=161,1,0)
g txtreatment=t*treatment
merge 1:1 id_new t using survey_long.dta
save Risk_ByIndiv_long.dta, replace

use Risk_ByIndiv_long.dta, clear
keep id_new partner mover pbl_cont pbl_all pblclass_1 complier treatment txtreatment school temp t
egen group= rowmax(id_new partner)
sort group mover
foreach var of varlist pbl_cont pbl_all complier {
g `var'2=`var' if mover==0
bysort group: replace `var'2=`var'2[1]
} 
save survey_treatment.dta, replace

use Risk_ByPair_post.dta, clear
append using "C:\Users\LG\Dropbox\RP\Data\Risk_ByPair", force
replace t=0 if t==.

merge 1:1 id_new partner t using survey_treatment.dta
ren _merge _surveytreat
drop if school==112|school==122|school==212|school==222
save Risk_byPair_long.dta, replace
