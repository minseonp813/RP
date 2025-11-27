*** Note: default is set to generate relative demand graph for all subejects.
***       If you want to set a specific pair, change " 1/`M'" to "N/N" at line 18
***  	  where N means maximum of two individuals id in a pair. 
*** Minseon Park, 01282018

use Risk_merged.dta, clear 
egen temp22=tag(groupid)
sort temp22 groupid
egen temp3=rank(groupid) if temp22==1
gsort groupid -temp22
bysort groupid : replace temp3=temp3[1]

form ccei %9.2f
form riskaversion %9.2f

quietly: su temp3
local M= r(max)
*local M=1
*-------------------------------------------------------------------------------
*                    Relative Price and Consumption Graph
*------------------------------------------------------------------------------;

forvalues i=1/`M' {
quietly: su id if temp3==`i' & mover==1
local id_mover = r(mean)
quietly: su id if temp3==`i' & mover==0
local id_stayer = r(mean)

quietly: su ccei if temp3==`i' & mover==0 & game_type==1
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskaversion if temp3==`i' & mover==0 & game_type==1
local RP = trim("`: display %9.2f r(mean)'") 
quietly: su rdu if temp3==`i' & mover==0 & game_type==1
local RDU = trim("`: display %9.2f r(mean)'") 


twoway (scatter relconsmp lnrelprice if temp3==`i'& mover==0 & game_type==1, msize(large) msymbol(circle_hollow) mcolor(red) ) ///
       (scatter relconsmp lnrelprice if temp3==`i'& mover==0 & game_type==2, msize(large) msymbol(x) mcolor(gs0) ), ///
	   legend(order(1 "Individual" 2 "Collective") col(1) ///
       position(8) ring(0) ///
       size(4) ///
	   ) ///
	   xlabel(-2(0.5)2, labsize(4)) xtitle("Log price ratio (log(p{subscript:2}/p{subscript:1}))", size(4.5) height(4)) ///
       ylabel(0(0.2)1, labsize(4)) ytitle("Share for the second security (x{subscript:2}/(x{subscript:2}+x{subscript:1}))", size(4.5) height(4)) ///
	   xline(2, lcolor(black)) /// *box
	   yline(1, lcolor(black)) /// *box
	   xline(0, lcolor(blue) lstyle(grid)) yline(0.5, lcolor(blue)) /// 
	   graphregion(color(white) lcolor(black)  ) ///
       plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) ///
       saving(temp1, replace) /// 
	   note("Individual CCEI: `C'" "Risk Preference: `RP', `RDU'", size(4)) ///
	   caption(Id: `id_stayer', size(4))

quietly: su ccei if temp3==`i' & mover==1 & game_type==1
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskaversion if temp3==`i' & mover==1 & game_type==1
local RP = trim("`: display %9.2f r(mean)'") 

twoway (scatter relconsmp lnrelprice if temp3==`i'& mover==1 & game_type==1, msize(large) msymbol(circle_hollow) mcolor(red) )  ///
       (scatter relconsmp lnrelprice if temp3==`i'& mover==1 & game_type==2, msize(large) msymbol(x) mcolor(gs0) ), ///
	   legend(order(1 "Individual" 2 "Collective") col(1) ///
       position(8) ring(0) ///
       size(4) ///
	   ) ///
       xlabel(-2(0.5)2, labsize(4)) xtitle("Log price ratio (log(p{subscript:2}/p{subscript:1}))", size(4.5) height(4)) ///
       ylabel(0(0.2)1, labsize(4)) ytitle("Share for the second security (x{subscript:2}/(x{subscript:2}+x{subscript:1}))", size(4.5) height(4)) ///
	   xline(2, lcolor(black)) ///
	   yline(1, lcolor(black)) ///
	   xline(0, lcolor(blue)) yline(0.5, lcolor(blue)) /// *box
       graphregion(color(white)) ///
       plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick) ) ///
       saving(temp2, replace) ///   
	   note("Individual CCEI: `C'" ///
	   "Risk Preference: `RP', `RDU'" ///
	   , size(4)) ///
	   caption(Id: `id_mover', size(4))
	   
quietly: su ccei if temp3==`i' & id== `id_mover' & game_type==2 
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskaversion if temp3==`i' & id== `id_mover' & game_type==2
local RP = trim("`: display %9.2f r(mean)'") 

gr combine temp1.gph temp2.gph, title(Group ID: `i', size(4)) ///
graphregion(color(white)) ///
subtitle("Collective CCEI: `C'" "Risk Preference: `RP', `RDU'", size(4)) 
gr export relconsumption`i'.png, replace
}
