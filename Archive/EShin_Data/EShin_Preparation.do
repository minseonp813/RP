*-------------------------------------------------------------------------------
*                            Cleaning Each DataSets
*------------------------------------------------------------------------------;

cd "/Users/euncheolshin/Dropbox/Academic_Sharings/P-RP/Data"


*-------------------------------------------------------------------------------
*                    Relative Price and Consumption Graph
*------------------------------------------------------------------------------;

use Risk_merged.dta 
egen temp22=tag(groupid)
sort temp22 groupid
egen temp3=rank(groupid) if temp22==1
gsort groupid -temp22
bysort groupid : replace temp3=temp3[1]

form ccei_col %9.2f
form ccei_ind %9.2f
form riskaversion %9.2f

quietly: su temp3
local M= r(max)
forvalues i=284/284 {
quietly: su id if temp3==`i' & mover==1
local id_mover = r(mean)
quietly: su id if temp3==`i' & mover==0
local id_stayer = r(mean)

quietly: su ccei_ind if temp3==`i' & mover==0
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskaversion if temp3==`i' & mover==0 & game_type==1
local RP = trim("`: display %9.2f r(mean)'") 

twoway (scatter relconsmp lnrelprice if temp3==`i'& mover==0 & game_type==1, msize(large) msymbol(circle_hollow))  ///
       (scatter relconsmp lnrelprice if temp3==`i'& mover==0 & game_type==2, msize(large) msymbol(+)), ///
	   legend(order(1 "Individual" 2 "collective")) xlabel(-2(1)2) xtitle("log(p2/p1)") ylabel(0(0.2)1) ytitle("x2/(x1+x2)") saving(temp1, replace) /// 
	   note("Individual CCEI=`C'" "Risk Preference=`RP', EUT") caption(Id=`id_stayer')

quietly: su ccei_ind if temp3==`i' & mover==1
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskaversion if temp3==`i' & mover==1 & game_type==1
local RP = trim("`: display %9.2f r(mean)'") 

twoway (scatter relconsmp lnrelprice if temp3==`i'& mover==1 & game_type==1, msize(large) msymbol(circle_hollow))  ///
       (scatter relconsmp lnrelprice if temp3==`i'& mover==1 & game_type==2, msize(large) msymbol(+)), ///
	   legend(order(1 "Individual" 2 "collective")) xlabel(-2(1)2) xtitle("log(p2/p1)") ylabel(0(0.2)1) ytitle("x2/(x1+x2)") saving(temp2, replace) ///   
	   note("Individual CCEI=`C'" "Risk Preference=`RP', RDU") caption(Id=`id_mover')
	   
quietly: su ccei_col if temp3==`i' & id== `id_mover'  
local C = trim("`: display %9.2f r(mean)'") 
quietly: su riskaversion if temp3==`i' & id== `id_mover' & game_type==2
local RP = trim("`: display %9.2f r(mean)'") 

gr combine temp1.gph temp2.gph, title(Group ID=`i') ///
subtitle("Collective CCEI=`C'" "Risk Preference =`RP', RDU") note("Risk Preference=(1/18)%XA2%XB2(x_cheaper/x1+x2)")
gr export relconsumption`i'.png, replace
}

preserve

*Why so many cases where alpha<0.05?
keep if alpha<0.05 //

replace alpha=alpha_cor 
keep if alpha_cor<0.05

egen tag1=tag(id) if game_type=="individual" & ccei_ind>=0.9
egen temp4=rank(id) if game_type=="individual" & tag1==1
gsort id -tag
bysort id: replace temp4 = temp4[1]

egen tag2=tag(temp3) 
egen temp5=rank(temp3) if game_type=="collective" & tag2==1 & ccei_col>=0.9
gsort temp3 -tag2
bysort temp3: replace temp5 = temp5[1]

su temp4 
local M=r(max)
forvalues i=1/`M' {
quietly: su id if temp4==`i'
local id = r(mean)

quietly: su ccei_ind if temp4==`i' 
local C = trim("`: display %9.2f r(mean)'") 
quietly: su alpha if temp4==`i' 
local alpha = trim("`: display %9.2f r(mean)'") 
quietly: su rho if temp4==`i' 
local rho = trim("`: display %9.2f r(mean)'") 

twoway (scatter relconsmp lnrelprice if temp4 ==`i' & game_type=="individual", msize(large) msymbol(circle_hollow)), ///
	   legend(order(1 "Individual")) xlabel(-2(1)2) xtitle("log(p_y/p_x)") ylabel(0(0.2)1) ytitle("y/(x+y)") /// 
	   note(Individual CCEI(Alpha)(Rho)=`C'(`alpha')(`rho')) caption(Id=`id')
gr export a_mmi_ind`i'_cor2.png, replace
}

su temp5
local M=r(max)
forvalues i=1/`M' {
quietly: su id if temp5==`i'
local id_min = r(min)
local id_max = r(max)

su temp3 if temp5==`i'
local group= r(mean)

quietly: su ccei_col if temp5==`i' 
local C = trim("`: display %9.2f r(mean)'") 
quietly: su alpha if temp5==`i' 
local alpha = trim("`: display %9.2f r(mean)'") 
quietly: su rho if temp5==`i' 
local rho = trim("`: display %9.2f r(mean)'") 


twoway (scatter relconsmp lnrelprice if temp5 ==`i' & game_type=="collective", msize(large) msymbol(circle_hollow)), ///
	   legend(order(1 "collective")) xlabel(-2(1)2) xtitle("log(p_y/p_x)") ylabel(0(0.2)1) ytitle("y/(x+y)") /// 
	   note(Collective CCEI(Alpha)(Rho)=`C'(`alpha')(`rho')) caption(Group Id=`group' Ind. Id=`id_min' and`id_max')
gr export alpha_col`i'_cor2.png, replace
}
restore
