clear all

cd "C:\Users\Administrator\Dropbox\value_of_friendship_2017\Analysis"
*cd "C:\Users\Eungik\Dropbox\value_of_friendship_2017\Analysis"


* Dictator game

use dictator_long.dta, replace
gen district=0
forval i=1/6{
replace district=`i' if school==1`i'|school==2`i'
}

drop if grade==2
bysort id obj_id: egen analysis=count(t)
keep if analysis==2
egen unique=concat(id obj_id)
destring unique, replace
xtset unique t

forval i=1/10{
local j: word `i' of  mathscore outgoing agreeable conscientious stable opened selfesteem n_closeness n_betweenness n_eigenvec
replace `j'=F.`j' if t==0 & `j'==.
}

keep if t==0
drop if outgoing==.

rename dyadic D
gen byte dyadic=D
replace dyadic=0 if dyadic==.





preserve
***** Figure 
replace dyadic=dyadic+1
bysort dyadic: egen mean_share = mean(dt_share)
bysort dyadic: egen count=count(dt_share)
bysort dyadic: egen sd_share=sd(dt_share)

gen high=mean_share+1.96*sd_share*(1/sqrt(count))
gen low=mean_share-1.96*sd_share*(1/sqrt(count))

collapse (mean) mean_share high low, by(dyadic) /* The following is to draw a figure "///" is for line breaks  */
tw (bar mean_share dyadic if dyadic==1, color(gs15) lcolor(gs1) barwidth(0.65)) (rcap high low dyadic if dyadic==1, lcolor(gs1) lwidth(0.5) msize(*3))  ///
   (bar mean_share dyadic if dyadic==2, color(gs10) lcolor(gs1) barwidth(0.65)) (rcap high low dyadic if dyadic==2, lcolor(gs1) lwidth(0.5) msize(*3))  ///
   (bar mean_share dyadic if dyadic==3, color(gs5)  lcolor(gs1) barwidth(0.65)) (rcap high low dyadic if dyadic==3, lcolor(gs1) lwidth(0.5) msize(*3))  ///
   (bar mean_share dyadic if dyadic==4, color(gs1)  lcolor(gs1) barwidth(0.65)) (rcap high low dyadic if dyadic==4, lcolor(gs1) lwidth(0.5) msize(*3)), ///
   yscale(range(0.2 0.45)) ylabel(0.2 (0.1) 0.45, labgap(0.5) labsize(4)) ytick(0.2 0.3 0.4, tlength(0.5)) ///
   xlabel( 1 "Null" 2 "Asymmetric 1" 3 "Asymmetric 2" 4 "Mutual", labsize(4)) ///
   legend(off) ///
   ytitle("Share", size(5) height(6)) xtitle("Dyadic Relationship", size(5) height(8)) /// */ To change font, go to the "Preferences" menu in STATA */
   graphregion(color(white))
gr export Eshin_Figure1.png, replace width(800) height(600)
restore



*Public goods
clear all

use public_long.dta, replace
drop if grade==2
keep if analysis==2
keep if t==0

/* Group 4 and Group 8 다 그리기 */
preserve
***** Figure 
replace dyadic=dyadic+1
replace dyadic=dyadic+5 if groupsize==8
bysort dyadic: egen mean_share = mean(pgg_share)
bysort dyadic: egen count=count(pgg_share)
bysort dyadic: egen sd_share=sd(pgg_share)

gen high=mean_share+1.96*sd_share*(1/sqrt(count))
gen low=mean_share-1.96*sd_share*(1/sqrt(count))

collapse (mean) mean_share high low, by(dyadic)

tw (bar mean_share dyadic if dyadic==1, color(gs15) lcolor(gs1)  barwidth(0.9)) (rcap high low dyadic if dyadic==1, lcolor(gs1) lwidth(0.5) msize(*3)) ///
   (bar mean_share dyadic if dyadic==2, color(gs10) barwidth(0.9)) (rcap high low dyadic if dyadic==2, lcolor(gs10) lwidth(0.5) msize(*3))  ///
   (bar mean_share dyadic if dyadic==3, color(gs5) barwidth(0.9)) (rcap high low dyadic if dyadic==3, lcolor(gs5) lwidth(0.5) msize(*3))  ///
   (bar mean_share dyadic if dyadic==4, color(gs1) barwidth(0.9)) (rcap high low dyadic if dyadic==4, lcolor(gs1) lwidth(0.5) msize(*3)) ///
   (bar mean_share dyadic if dyadic==6, color(gs15) lcolor(gs1) barwidth(0.9)) (rcap high low dyadic if dyadic==6, lcolor(gs1) lwidth(0.5) msize(*3)) ///
   (bar mean_share dyadic if dyadic==7, color(gs10) barwidth(0.9)) (rcap high low dyadic if dyadic==7, lcolor(gs10) lwidth(0.5) msize(*3))  ///
   (bar mean_share dyadic if dyadic==8, color(gs5) barwidth(0.9)) (rcap high low dyadic if dyadic==8, lcolor(gs5) lwidth(0.5) msize(*3))  ///
   (bar mean_share dyadic if dyadic==9, color(gs1) barwidth(0.9)) (rcap high low dyadic if dyadic==9, lcolor(gs1) lwidth(0.5) msize(*3)),  ///
    ylabel(0.2 (0.1) 0.45) ///
	xlabel( 1 "Null" 2 "Asymmetric 1" 3 "Asymmetric 2" 4 "Mutual" 6 "Null" 7 "Asymmetric 1" 8 "Asymmetric 2" 9 "Mutual", labsize(2)) ///
	legend(off) ///
	ytitle("Share", size(5) height(6)) ///
	xtitle("Dyadic Relationship", size(5) height(8)) ///
	graphregion(color(white))

gr export Eshin_Figure2.png, replace width(800) height(600)
restore


/* Group 4 and Group 8 풀링 하기 */


preserve

/* Group size 4 만 그리시려면 아래 명령어 활성화시키시고 돌리세용*/
* drop if groupszie==8


***** Figure 
replace dyadic=dyadic+1
bysort dyadic: egen mean_share = mean(pgg_share)
bysort dyadic: egen count=count(pgg_share)
bysort dyadic: egen sd_share=sd(pgg_share)

gen high=mean_share+1.96*sd_share*(1/sqrt(count))
gen low=mean_share-1.96*sd_share*(1/sqrt(count))




collapse (mean) mean_share high low, by(dyadic)

tw (bar mean_share dyadic if dyadic==1, color(gs15) lcolor(gs1)  barwidth(0.65)) (rcap high low dyadic if dyadic==1, lcolor(gs1) lwidth(0.5) msize(*3)) ///
   (bar mean_share dyadic if dyadic==2, color(gs10) barwidth(0.65)) (rcap high low dyadic if dyadic==2, lcolor(gs10) lwidth(0.5) msize(*3))  ///
   (bar mean_share dyadic if dyadic==3, color(gs5) barwidth(0.65)) (rcap high low dyadic if dyadic==3, lcolor(gs5) lwidth(0.5) msize(*3))  ///
   (bar mean_share dyadic if dyadic==4, color(gs1) barwidth(0.65)) (rcap high low dyadic if dyadic==4, lcolor(gs1) lwidth(0.5) msize(*3)), ///
    ylabel(0.2 (0.1) 0.45) ///
	xlabel( 1 "Null" 2 "Asymmetric 1" 3 "Asymmetric 2" 4 "Mutual", labsize(2)) ///
	legend(off) ///
	ytitle("Share", size(5) height(6)) ///
	xtitle("Dyadic Relationship", size(5) height(8)) ///
	graphregion(color(white))

gr export Eshin_Figure3.png, replace width(800) height(600)
restore







**********************PBL EFFECT************************************************

* Dictator game *

use dictator_long.dta , replace
bysort id obj_id: egen analysis=count(t)
drop if grade==2
keep if analysis==2
rename dyadic D
gen byte dyadic=D
replace dyadic=0 if dyadic==.
drop if outgoing==.
egen unique=concat(id obj_id)
destring unique, replace
xtset unique t
replace dyadic=L.dyadic if t==1
gen dt_share_diff=dt_share - L.dt_share
drop if t==0



/* 관계 상관없는 PBL 효과 Dictator game */

preserve
replace treatment=treatment+1
bysort treatment: egen mean_share = mean(dt_share_diff)
bysort treatment: egen count=count(dt_share_diff)
bysort treatment: egen sd_share=sd(dt_share_diff)

gen high=mean_share+1.96*sd_share*(1/sqrt(count))
gen low=mean_share-1.96*sd_share*(1/sqrt(count))


collapse (mean) mean_share high low, by(treatment)

tw (bar mean_share treatment if treatment==1, color(gs15) lcolor(gs1)  barwidth(0.65)) (rcap high low treatment if treatment==1, lcolor(gs1) lwidth(0.5) msize(*3)) ///
   (bar mean_share treatment if treatment==2, color(gs10) barwidth(0.65)) (rcap high low treatment if treatment==2, lcolor(gs10) lwidth(0.5) msize(*3)),  ///
    ylabel(0 (0.02) -0.04) ///
	xlabel( 1 "Control" 2 "Treatment", labsize(2)) ///
	legend(off) ///
	ytitle("Share", size(5) height(6)) ///
	xtitle(" King Euncheol ", size(5) height(8)) ///
	graphregion(color(white))

gr export Eshin_Figure4.png, replace width(800) height(600)
restore




/* 관계에 따른  PBL 효과 Dictator game */

preserve
***** Figure 
replace dyadic=3*dyadic+1
replace dyadic=dyadic+treatment
bysort dyadic : egen mean_share = mean(dt_share_diff)
bysort dyadic : egen count=count(dt_share_diff)
bysort dyadic : egen sd_share=sd(dt_share_diff)

gen high=mean_share+1.96*sd_share*(1/sqrt(count))
gen low=mean_share-1.96*sd_share*(1/sqrt(count))



collapse (mean) mean_share high low, by(dyadic)

tw (bar mean_share dyadic if dyadic==1, color(gs15) lcolor(gs1)  barwidth(1)) (rcap high low dyadic if dyadic==1, lcolor(gs1) lwidth(0.5) msize(*2)) ///
   (bar mean_share dyadic if dyadic==2, color(gs10) barwidth(1)) (rcap high low dyadic if dyadic==2, lcolor(gs10) lwidth(0.5) msize(*2))  ///
   (bar mean_share dyadic if dyadic==4, color(gs5) barwidth(1)) (rcap high low dyadic if dyadic==4, lcolor(gs5) lwidth(0.5) msize(*2))  ///
   (bar mean_share dyadic if dyadic==5, color(gs1) barwidth(1)) (rcap high low dyadic if dyadic==5, lcolor(gs1) lwidth(0.5) msize(*2)) ///
   (bar mean_share dyadic if dyadic==7, color(gs15) lcolor(gs1)  barwidth(1)) (rcap high low dyadic if dyadic==7, lcolor(gs1) lwidth(0.5) msize(*2)) ///
   (bar mean_share dyadic if dyadic==8, color(gs10) barwidth(1)) (rcap high low dyadic if dyadic==8, lcolor(gs10) lwidth(0.5) msize(*2))  ///
   (bar mean_share dyadic if dyadic==9, color(gs5) barwidth(1)) (rcap high low dyadic if dyadic==9, lcolor(gs5) lwidth(0.5) msize(*2))  ///
   (bar mean_share dyadic if dyadic==10, color(gs1) barwidth(1)) (rcap high low dyadic if dyadic==10, lcolor(gs1) lwidth(0.5) msize(*2)) ///
   (bar mean_share dyadic if dyadic==11, color(gs1) barwidth(1)) (rcap high low dyadic if dyadic==11, lcolor(gs1) lwidth(0.5) msize(*2)), ///
    ylabel(0.04 (0.02) -0.1) ///
	xlabel(1 "" 2 "" 4 "" 5 "" 7 " " 8 " " 10 " " 11 " ", labsize(2)) ///
	legend(off) ///
	ytitle("Share", size(5) height(6)) ///
	xtitle("King King Euncheol", size(5) height(8)) ///
	graphregion(color(white))

gr export Eshin_Figure5.png, replace width(800) height(600)
restore



*Public goods*******************************************************************
clear all
use public_long.dta , replace
drop if grade==2
keep if analysis==2
egen unique=concat(id group_id)
destring unique, replace
xtset unique t

gen pgg_share_diff=pgg_share - L.pgg_share



/* 관계 상관없는 PBL 효과 PGG game */

preserve
replace treatment=treatment+1
bysort treatment: egen mean_share = mean(pgg_share_diff)
bysort treatment: egen count=count(pgg_share_diff)
bysort treatment: egen sd_share=sd(pgg_share_diff)


gen high=mean_share+1.96*sd_share*(1/sqrt(count))
gen low=mean_share-1.96*sd_share*(1/sqrt(count))


collapse (mean) mean_share high low, by(treatment)

tw (bar mean_share treatment if treatment==1, color(gs15) lcolor(gs1)  barwidth(0.65)) (rcap high low treatment if treatment==1, lcolor(gs1) lwidth(0.5) msize(*3)) ///
   (bar mean_share treatment if treatment==2, color(gs10) barwidth(0.65)) (rcap high low treatment if treatment==2, lcolor(gs10) lwidth(0.5) msize(*3)),  ///
    ylabel(0 (0.02) -0.04) ///
	xlabel( 1 "Control" 2 "Treatment", labsize(2)) ///
	legend(off) ///
	ytitle("Share", size(5) height(6)) ///
	xtitle(" King Euncheol ", size(5) height(8)) ///
	graphregion(color(white))

gr export Eshin_Figure6.png, replace width(800) height(600)
restore

/* 관계 상관있는 PBL 효과 PGG game */

preserve
***** Figure 
replace dyadic=3*dyadic+1
replace dyadic=dyadic+treatment
bysort dyadic : egen mean_share = mean(pgg_share_diff)
bysort dyadic : egen count=count(pgg_share_diff)
bysort dyadic : egen sd_share=sd(pgg_share_diff)

gen high=mean_share+1.96*sd_share*(1/sqrt(count))
gen low=mean_share-1.96*sd_share*(1/sqrt(count))



collapse (mean) mean_share high low, by(dyadic)

tw (bar mean_share dyadic if dyadic==1, color(gs15) lcolor(gs1)  barwidth(1)) (rcap high low dyadic if dyadic==1, lcolor(gs1) lwidth(0.5) msize(*2)) ///
   (bar mean_share dyadic if dyadic==2, color(gs10) barwidth(1)) (rcap high low dyadic if dyadic==2, lcolor(gs10) lwidth(0.5) msize(*2))  ///
   (bar mean_share dyadic if dyadic==4, color(gs5) barwidth(1)) (rcap high low dyadic if dyadic==4, lcolor(gs5) lwidth(0.5) msize(*2))  ///
   (bar mean_share dyadic if dyadic==5, color(gs1) barwidth(1)) (rcap high low dyadic if dyadic==5, lcolor(gs1) lwidth(0.5) msize(*2)) ///
   (bar mean_share dyadic if dyadic==7, color(gs15) lcolor(gs1)  barwidth(1)) (rcap high low dyadic if dyadic==7, lcolor(gs1) lwidth(0.5) msize(*2)) ///
   (bar mean_share dyadic if dyadic==8, color(gs10) barwidth(1)) (rcap high low dyadic if dyadic==8, lcolor(gs10) lwidth(0.5) msize(*2))  ///
   (bar mean_share dyadic if dyadic==9, color(gs5) barwidth(1)) (rcap high low dyadic if dyadic==9, lcolor(gs5) lwidth(0.5) msize(*2))  ///
   (bar mean_share dyadic if dyadic==10, color(gs1) barwidth(1)) (rcap high low dyadic if dyadic==10, lcolor(gs1) lwidth(0.5) msize(*2)) ///
   (bar mean_share dyadic if dyadic==11, color(gs1) barwidth(1)) (rcap high low dyadic if dyadic==11, lcolor(gs1) lwidth(0.5) msize(*2)), ///
    ylabel(0.04 (0.02) -0.1) ///
	xlabel(1 "" 2 "" 4 "" 5 "" 7 " " 8 " " 10 " " 11 " ", labsize(2)) ///
	legend(off) ///
	ytitle("Share", size(5) height(6)) ///
	xtitle("King King Euncheol", size(5) height(8)) ///
	graphregion(color(white))

gr export Eshin_Figure7.png, replace width(800) height(600)
restore
