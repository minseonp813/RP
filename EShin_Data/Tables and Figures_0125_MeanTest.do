use "C:\Users\Minseon Park\Dropbox\RP\Data\Risk_ByPair.dta", clear
set more off

* Figure 1
anova ccei_col ccei_both90

preserve
collapse (mean) meanccei= ccei_col (sd) sdccei= ccei_col (count) n= ccei_col, by(ccei_both90)
generate hiccei = meanccei + invttail(n-1,0.025)*(sdccei / sqrt(n))
generate loccei = meanccei - invttail(n-1,0.025)*(sdccei / sqrt(n))

graph twoway (bar meanccei ccei_both90) (rcap hiccei loccei ccei_both90)
gr export Figure1_0125.png, replace
restore

* Figure 2A
anova riskaversion_col riskaversionpair

preserve
collapse (mean) meanriskaversion= riskaversion_col (sd) sdriskaversion= riskaversion_col (count) n= riskaversion_col, by(riskaversionpair)
generate hiriskaversion = meanriskaversion + invttail(n-1,0.025)*(sdriskaversion / sqrt(n))
generate loriskaversion = meanriskaversion - invttail(n-1,0.025)*(sdriskaversion / sqrt(n))

graph twoway (bar meanriskaversion riskaversionpair) (rcap hiriskaversion loriskaversion riskaversionpair)
gr export Figure2A_0125.png, replace
restore

* Figure 2B
anova riskpremium_col riskpremiumpair if ccei_both90==2 & ccei_col90==1

preserve
collapse (mean) meanriskpremium= riskpremium_col (sd) sdriskpremium= riskpremium_col (count) n= riskpremium_col if ccei_both90==2 & ccei_col90==1, by(riskpremiumpair)
generate hiriskpremium = meanriskpremium + invttail(n-1,0.025)*(sdriskpremium / sqrt(n))
generate loriskpremium = meanriskpremium - invttail(n-1,0.025)*(sdriskpremium / sqrt(n))

graph twoway (bar meanriskpremium riskpremiumpair) (rcap hiriskpremium loriskpremium riskpremiumpair)
gr export Figure2B_0125.png, replace
restore

* Figure 4A
ttest uloss, by(ccei_col90)

preserve
collapse (mean) meanuloss= uloss_col (sd) sduloss= uloss_col (count) n= uloss_col, by(ccei_col90)
generate hiuloss = meanuloss + invttail(n-1,0.025)*(sduloss / sqrt(n))
generate louloss = meanuloss - invttail(n-1,0.025)*(sduloss / sqrt(n))

graph twoway (bar meanuloss ccei_col90) (rcap hiuloss louloss ccei_col90)
gr export Figure4A.png, replace
restore

* Figure 4B
ttest uloss if ccei_both90==2, by(ccei_col90)

preserve
collapse (mean) meanuloss= uloss_col (sd) sduloss= uloss_col (count) n= uloss_col if ccei_both90==2, by(ccei_col90)
generate hiuloss = meanuloss + invttail(n-1,0.025)*(sduloss / sqrt(n))
generate louloss = meanuloss - invttail(n-1,0.025)*(sduloss / sqrt(n))

graph twoway (bar meanuloss ccei_col90) (rcap hiuloss louloss ccei_col90)
gr export Figure4B.png, replace
restore

* Correlation between CCEI and Mathscore
use Risk_ByIndiv.dta, clear
corr ccei_ind mathscore /*0.067*/

* Correlation between CCEI and dt, pgg game
use "C:\Users\Minseon Park\Dropbox\RP\Data\dictator_pre.dta" 
br
collapse dt_value, by(id)
br
save dt_pre.dta, replace
use "C:\Users\Minseon Park\Dropbox\RP\Data\publicgoods_pre.dta" 
br
collapse pgg_value, by(id)
br
save pgg_pre.dta, replace

use Risk_ByIndiv.dta, clear
merge 1:1 id using dt_pre.dta
drop _merge
merge 1:1 id using pgg_pre.dta
drop _merge
corr ccei_ind dt_value pgg_value mathscore


/*	ccei_ind	dt_value	pgg_va~e	mathsc~e
				
ccei_ind	1.0000
dt_value	-0.0164	1.0000
pgg_value	-0.0410	0.5011	1.0000
mathscore	0.0669	0.0301	-0.0273	1.0000 */

	