*** Note: default is set to generate relative demand graph for all subejects.
***       If you want to set a specific pair, change " 1/`M'" to "N/N" at line 18
***  	  where N means maximum of two individuals id in a pair. 
*** Written by Minseon Park, 012818
*** Added Euncheol Shin's format, 122319

cd "C:\Users\hahn0\Dropbox\RP\Data"
global grph = "C:\Users\hahn0\Dropbox\RP\Data\graphs"

use Risk_merged.dta, clear 
egen temp=tag(groupid)
sort temp groupid
egen temp1=rank(groupid) if temp==1
gsort groupid -temp
bysort groupid : replace temp1=temp1[1]

quietly: su temp1
local M= r(max)

display "`M'"

* local M=1


********************************************************************************
*                    Relative Price and Consumption Graph
********************************************************************************

forvalues i=1/`M' {

quietly: su id if temp1==`i' & mover=="t" // to add id as a caption in the graph
local id_mover = r(mean)
quietly: su id if temp1==`i' & mover=="f"
local id_stayer = r(mean)

twoway (scatter relconsmp lnrelprice if temp1==`i'& mover=="f" & game_type==1, msize(large) msymbol(circle_hollow) mcolor(red) ) ///
       (scatter relconsmp lnrelprice if temp1==`i'& mover=="f" & game_type==2, msize(large) msymbol(x) mcolor(gs0) ), ///
	   legend(order(1 "Individual" 2 "Collective") col(1) ///
       position(8) ring(0) ///
       size(4)) ///
	   xlabel(-2(0.5)2, labsize(4)) xtitle("Log price ratio (log(p{subscript:2}/p{subscript:1}))", size(4.5) height(4)) ///
       ylabel(0(0.2)1, labsize(4)) ytitle("Share for the second security (x{subscript:2}/(x{subscript:2}+x{subscript:1}))", size(4.5) height(4)) ///
	   xline(2, lcolor(black)) /// *box
	   yline(1, lcolor(black)) /// *box
	   xline(0, lcolor(blue) lstyle(grid)) yline(0.5, lcolor(blue)) /// 
	   graphregion(color(white) lcolor(black)  ) ///
       plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) ///
       saving(temp1, replace) /// 
	   caption(Id: `id_stayer', size(4))

twoway (scatter relconsmp lnrelprice if temp1==`i'& mover=="t" & game_type==1, msize(large) msymbol(circle_hollow) mcolor(red) )  ///
       (scatter relconsmp lnrelprice if temp1==`i'& mover=="t" & game_type==2, msize(large) msymbol(x) mcolor(gs0) ), ///
	   legend(order(1 "Individual" 2 "Collective") col(1) ///
       position(8) ring(0) ///
       size(4)) ///
       xlabel(-2(0.5)2, labsize(4)) xtitle("Log price ratio (log(p{subscript:2}/p{subscript:1}))", size(4.5) height(4)) ///
       ylabel(0(0.2)1, labsize(4)) ytitle("Share for the second security (x{subscript:2}/(x{subscript:2}+x{subscript:1}))", size(4.5) height(4)) ///
	   xline(2, lcolor(black)) ///
	   yline(1, lcolor(black)) ///
	   xline(0, lcolor(blue)) yline(0.5, lcolor(blue)) /// *box
       graphregion(color(white)) ///
       plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) ///
       saving(temp2, replace) ///   
	   caption(Id: `id_mover', size(4))
	   
gr combine temp1.gph temp2.gph, title(Group ID: `i', size(4)) ///
graphregion(color(white)) 
gr export "$grph/relconsumption`i'.png", replace
erase temp1.gph
erase temp2.gph
}
