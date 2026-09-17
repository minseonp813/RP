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
*local M= r(max)
local M=1
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

twoway (scatter relconsmp lnrelprice if temp3==`i'& mover==0 & game_type==1, msize(large) msymbol(circle_hollow))  ///
       (scatter relconsmp lnrelprice if temp3==`i'& mover==0 & game_type==2, msize(large) msymbol(+)), ///
	   legend(order(1 "Individual" 2 "Collective") col(1) ///
       position(9) ring(0) ///
       size(4) ///
	   ) ///
	   xlabel(-2(0.5)2, labsize(4)) xtitle("Log price ratio (log(p2/p1))", size(5) height(5)) ///
       ylabel(0(0.2)1, labsize(4)) ytitle("Share for the second security", size(5) height(5)) ///
	   graphregion(color(white)) ///
       saving(temp1, replace) /// 
	   note("Individual CCEI: `C'" "Risk Preference: `RP', EUT") caption(Id=`id_stayer')

quietly: su ccei if temp3==`i' & mover==1 & game_type==1
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskaversion if temp3==`i' & mover==1 & game_type==1
local RP = trim("`: display %9.2f r(mean)'") 

twoway (scatter relconsmp lnrelprice if temp3==`i'& mover==1 & game_type==1, msize(large) msymbol(circle_hollow))  ///
       (scatter relconsmp lnrelprice if temp3==`i'& mover==1 & game_type==2, msize(large) msymbol(+)), ///
	   legend(order(1 "Individual" 2 "Collective") col(1) ///
       position(9) ring(0) ///
       size(4) ///
	   ) ///
       xlabel(-2(0.5)2, labsize(4)) xtitle("Log price ratio (log(p2/p1))", size(5) height(5)) ///
       ylabel(0(0.2)1, labsize(4)) ytitle("Share for the second security", size(5) height(5)) ///
       graphregion(color(white)) ///
       saving(temp2, replace) ///   
	   note("Individual CCEI: `C'" "Risk Preference: `RP', RDU") caption(Id=`id_mover')
	   
quietly: su ccei if temp3==`i' & id== `id_mover' & game_type==2 
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskaversion if temp3==`i' & id== `id_mover' & game_type==2
local RP = trim("`: display %9.2f r(mean)'") 

gr combine temp1.gph temp2.gph, title(Group ID=`i') ///
graphregion(color(white)) ///
subtitle("Collective CCEI: `C'" "Risk Preference: `RP', RDU") 
gr export relconsumption`i'.png, replace
}
