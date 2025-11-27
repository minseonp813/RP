use "data\for_graphs_baseline_raw", clear 
global grph "results" 

keep if inlist(group_id, "11101161110107", "11101231110104", "11101281110118", "11102241110221") 

gen new2_I_ig_round = round(new2_I_ig, 0.01) 
sort id 

destring groupid id high_id, replace ignore(" ") force 
gen temp1 = groupid 
levelsof temp1, local(glist) 

foreach gid of local glist { 

    quietly: su id if groupid == `gid' & mover == "t"
    local id_mover = r(mean) 

    quietly: su id if groupid == `gid' & mover == "f"
    local id_stayer = r(mean) 

    quietly: su high_id if groupid == `gid'
    local id_high = r(mean) 

    quietly: su ccei_g if groupid == `gid'
    local cceig = string(floor(r(mean)*100)/100, "%6.2f") 

    quietly: su RA_g if groupid == `gid'
    local RAg = string(floor(r(mean)*100)/100, "%6.2f") 

    if `id_stayer' == `id_high' {
        quietly: su ccei_h if groupid == `gid'
        local cceistayer = string(floor(r(mean)*100)/100, "%6.2f")
    }
    else {
        quietly: su ccei_l if groupid == `gid'
        local cceistayer = string(floor(r(mean)*100)/100, "%6.2f")
    }

    if `id_mover' == `id_high' {
        quietly: su ccei_h if groupid == `gid'
        local cceimover = string(floor(r(mean)*100)/100, "%6.2f")
    }
    else {
        quietly: su ccei_l if groupid == `gid'
        local cceimover = string(floor(r(mean)*100)/100, "%6.2f")
    }

    quietly: su new2_I_ig_round if id == `id_stayer'
    local I_igstayer = string(r(mean), "%6.2f") 

    quietly: su new2_I_ig_round if id == `id_mover'
    local I_igmover = string(r(mean), "%6.2f") 

    quietly: su RA_i if groupid == `gid' & id == `id_stayer'
    local RAstayer = string(r(mean), "%6.2f") 

    quietly: su RA_i if groupid == `gid' & id == `id_mover'
    local RAmover = string(r(mean), "%6.2f") 

    if `id_high' == `id_mover' {
        local color_mover "blue%40"
        local color_stayer "red%40"
    }
    else {
        local color_mover "red%40"
        local color_stayer "blue%40"
    }

    * --- stayer plot ---
    twoway (scatter relconsmp lnrelprice if groupid == `gid' & mover == "f" & game_type == 1, ///
        msize(large) msymbol(circle) mcolor(`color_stayer')) ///
        (scatter relconsmp lnrelprice if groupid == `gid' & mover == "f" & game_type == 2, ///
        msize(large) msymbol(x) mcolor(black)), ///
        legend(order(1 "Individual" 2 "Collective") col(1) position(8) ring(0) size(5)) ///
        xlabel(-2(1)2, labsize(5) format(%4.1f)) ///  <-- x축 라벨 1단위
        xtitle("Log price ratio (log(p{subscript:2}/p{subscript:1}))", size(5)) ///
        ylabel(0(0.2)1, labsize(5) format(%4.1f)) ///
        ytitle("Share for the second security (x{subscript:2}/(x{subscript:2}+x{subscript:1}))", size(5)) ///
        xline(0, lcolor(blue) lstyle(grid)) ///
        yline(0.5, lcolor(blue)) ///
        graphregion(color(white) lcolor(black)) ///
        saving(temp1, replace) ///
        text(-0.25 -0.2 "CCEI{subscript:i}: `cceistayer', I{subscript:ig}: `I_igstayer', RA{subscript:i}: `RAstayer'", size(7))

    * --- mover plot ---
    twoway (scatter relconsmp lnrelprice if groupid == `gid' & mover == "t" & game_type == 1, ///
        msize(large) msymbol(circle) mcolor(`color_mover')) ///
        (scatter relconsmp lnrelprice if groupid == `gid' & mover == "t" & game_type == 2, ///
        msize(large) msymbol(x) mcolor(black)), ///
        legend(order(1 "Individual" 2 "Collective") col(1) position(8) ring(0) size(5)) ///
        xlabel(-2(1)2, labsize(5) format(%4.1f)) ///
        xtitle("Log price ratio (log(p{subscript:2}/p{subscript:1}))", size(5)) ///
        ylabel(0(0.2)1, labsize(5) format(%4.1f)) ///
        ytitle("Share for the second security (x{subscript:2}/(x{subscript:2}+x{subscript:1}))", size(5)) ///
        xline(0, lcolor(blue) lstyle(grid)) ///
        yline(0.5, lcolor(blue)) ///
        graphregion(color(white)) ///
        saving(temp2, replace) ///
        text(-0.25 -0.2 "CCEI{subscript:i}: `cceimover', I{subscript:ig}: `I_igmover', RA{subscript:i}: `RAmover'", size(7))

    gr combine temp1.gph temp2.gph, ///
        graphregion(color(white) margin(b+2)) ///
        title("Group ID: `gid' (Baseline)" "CCEI{subscript:g}: `cceig', RA{subscript:g}: `RAg'", size(5) position(12)) ///
        name(temp3, replace)

    gr export "$grph\relconsumption`gid'_Baseline.png", replace 

    erase temp1.gph 
    erase temp2.gph 
}







************************************************************************
* NOTE : TAKES HOURS TO RUN
* Generate relative demand graph for all 652 groups at baseline and endline.
* results (ZIP file) uploaded at dropbox/RP/RA_Byunghun/finalized_0825
************************************************************************

use "data\for_graphs_baseline_raw", clear

global grph "results"

keep if inlist(group_id, "11101161110107", "11101231110104", "11101281110118", "11102241110221")

gen new2_I_ig_round = round(new2_I_ig, 0.01)
sort id

destring groupid id high_id, replace ignore(" ") force

gen temp1 = groupid
levelsof temp1, local(glist)

foreach gid of local glist {

    quietly: su id if groupid == `gid' & mover == "t"
    local id_mover = r(mean)

    quietly: su id if groupid == `gid' & mover == "f"
    local id_stayer = r(mean)

    quietly: su high_id if groupid == `gid'
    local id_high = r(mean)

    quietly: su ccei_g if groupid == `gid'
    local cceig = string(floor(r(mean)*100)/100, "%6.2f")

    if `id_stayer' == `id_high' {
        quietly: su ccei_h if groupid == `gid'
        local cceistayer = string(floor(r(mean)*100)/100, "%6.2f")
    }
    else {
        quietly: su ccei_l if groupid == `gid'
        local cceistayer = string(floor(r(mean)*100)/100, "%6.2f")
    }

    if `id_mover' == `id_high' {
        quietly: su ccei_h if groupid == `gid'
        local cceimover = string(floor(r(mean)*100)/100, "%6.2f")
    }
    else {
        quietly: su ccei_l if groupid == `gid'
        local cceimover = string(floor(r(mean)*100)/100, "%6.2f")
    }

    quietly: su new2_I_ig_round if id == `id_stayer'
    local I_igstayer = string(r(mean), "%6.2f")

    quietly: su new2_I_ig_round if id == `id_mover'
    local I_igmover  = string(r(mean), "%6.2f")

    if `id_high' == `id_mover' {
        local color_mover "blue%40"
        local color_stayer "red"
    }
    else {
        local color_mover "red"
        local color_stayer "blue%40"
    }

    twoway (scatter relconsmp lnrelprice if groupid == `gid' & mover == "f" & game_type == 1, ///
        msize(medlarge) msymbol(circle) mcolor(`color_stayer')) ///
        (scatter relconsmp lnrelprice if groupid == `gid' & mover == "f" & game_type == 2, ///
        msize(medlarge) msymbol(x) mcolor(black)), ///
        legend(order(1 "Individual" 2 "Collective") col(1) position(8) ring(0) size(4)) ///
        xlabel(-2(0.5)2, labsize(4)) ///
        xtitle("log(p{subscript:2}/p{subscript:1})", size(4.5) height(4)) ///
        ylabel(0(0.2)1, labsize(4)) ///
        ytitle("x{subscript:2}/(x{subscript:2}+x{subscript:1})", size(4.5) height(4)) ///
        xline(0, lcolor(blue) lstyle(grid)) ///
        yline(0.5, lcolor(blue)) ///
        graphregion(color(white) lcolor(black)) ///
        plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick)) ///
        saving(temp1, replace) ///
        caption("{bf:Id: `id_stayer', CCEI_i: `cceistayer', CCEI_g: `cceig', I_ig: `I_igstayer'}", size(3.5))

    twoway (scatter relconsmp lnrelprice if groupid == `gid' & mover == "t" & game_type == 1, ///
        msize(medlarge) msymbol(circle) mcolor(`color_mover')) ///
        (scatter relconsmp lnrelprice if groupid == `gid' & mover == "t" & game_type == 2, ///
        msize(medlarge) msymbol(x) mcolor(black)), ///
        legend(order(1 "Individual" 2 "Collective") col(1) position(8) ring(0) size(4)) ///
        xlabel(-2(0.5)2, labsize(4)) ///
        xtitle("log(p{subscript:2}/p{subscript:1})", size(4.5) height(4)) ///
        ylabel(0(0.2)1, labsize(4)) ///
        ytitle("x{subscript:2}/(x{subscript:2}+x{subscript:1})", size(4.5) height(4)) ///
        xline(0, lcolor(blue) lstyle(grid)) ///
        yline(0.5, lcolor(blue)) ///
        graphregion(color(white)) ///
        plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick)) ///
        saving(temp2, replace) ///
        caption("{bf:Id: `id_mover', CCEI_i: `cceimover', CCEI_g: `cceig', I_ig: `I_igmover'}", size(3.5))

    * Combine & Export (Baseline)
    gr combine temp1.gph temp2.gph, title("Group ID: `gid' (Baseline)", size(4)) graphregion(color(white))
    gr export "$grph\relconsumption`gid'_Baseline.png", replace

    erase temp1.gph
    erase temp2.gph
}

**********************************************************

use "data\for_graphs_endline_raw", clear

global grph "C:\Users\hahn0\RP\Graphs_Endline"

keep if inlist(group_id, "11101161110107", "11101231110104", "11101281110118", "11102241110221")

gen new2_I_ig_round = round(new2_I_ig, 0.01)
sort id

tostring mover, replace
replace mover = "t" if mover == "1"
replace mover = "f" if mover == "0"

destring groupid id high_id, replace ignore(" ") force

gen temp1 = groupid
levelsof temp1, local(glist)

foreach gid of local glist {

    quietly: su id if groupid == `gid' & mover == "t"
    local id_mover = r(mean)

    quietly: su id if groupid == `gid' & mover == "f"
    local id_stayer = r(mean)

    quietly: su high_id if groupid == `gid'
    local id_high = r(mean)

    quietly: su ccei_g if groupid == `gid'
    local cceig = string(floor(r(mean)*100)/100, "%6.2f")

    if `id_stayer' == `id_high' {
        quietly: su ccei_h if groupid == `gid'
        local cceistayer = string(floor(r(mean)*100)/100, "%6.2f")
    }
    else {
        quietly: su ccei_l if groupid == `gid'
        local cceistayer = string(floor(r(mean)*100)/100, "%6.2f")
    }

    if `id_mover' == `id_high' {
        quietly: su ccei_h if groupid == `gid'
        local cceimover = string(floor(r(mean)*100)/100, "%6.2f")
    }
    else {
        quietly: su ccei_l if groupid == `gid'
        local cceimover = string(floor(r(mean)*100)/100, "%6.2f")
    }

    quietly: su new2_I_ig_round if id == `id_stayer'
    local I_igstayer = string(r(mean), "%6.2f")

    quietly: su new2_I_ig_round if id == `id_mover'
    local I_igmover = string(r(mean), "%6.2f")

    if `id_high' == `id_mover' {
        local color_mover "blue%40"
        local color_stayer "red"
    }
    else {
        local color_mover "red"
        local color_stayer "blue%40"
    }

    twoway (scatter relconsmp lnrelprice if groupid == `gid' & mover == "f" & game_type == 1, ///
        msize(medlarge) msymbol(circle) mcolor(`color_stayer')) ///
        (scatter relconsmp lnrelprice if groupid == `gid' & mover == "f" & game_type == 2, ///
        msize(medlarge) msymbol(x) mcolor(black)), ///
        legend(order(1 "Individual" 2 "Collective") col(1) position(8) ring(0) size(4)) ///
        xlabel(-2(0.5)2, labsize(4)) ///
        xtitle("log(p{subscript:2}/p{subscript:1})", size(4.5) height(4)) ///
        ylabel(0(0.2)1, labsize(4)) ///
        ytitle("x{subscript:2}/(x{subscript:2}+x{subscript:1})", size(4.5) height(4)) ///
        xline(0, lcolor(blue) lstyle(grid)) ///
        yline(0.5, lcolor(blue)) ///
        graphregion(color(white) lcolor(black)) ///
        plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick)) ///
        saving(temp1, replace) ///
        caption("{bf:Id: `id_stayer', CCEI_i: `cceistayer', CCEI_g: `cceig', I_ig: `I_igstayer'}", size(3.5))

    twoway (scatter relconsmp lnrelprice if groupid == `gid' & mover == "t" & game_type == 1, ///
        msize(medlarge) msymbol(circle) mcolor(`color_mover')) ///
        (scatter relconsmp lnrelprice if groupid == `gid' & mover == "t" & game_type == 2, ///
        msize(medlarge) msymbol(x) mcolor(black)), ///
        legend(order(1 "Individual" 2 "Collective") col(1) position(8) ring(0) size(4)) ///
        xlabel(-2(0.5)2, labsize(4)) ///
        xtitle("log(p{subscript:2}/p{subscript:1})", size(4.5) height(4)) ///
        ylabel(0(0.2)1, labsize(4)) ///
        ytitle("x{subscript:2}/(x{subscript:2}+x{subscript:1})", size(4.5) height(4)) ///
        xline(0, lcolor(blue) lstyle(grid)) ///
        yline(0.5, lcolor(blue)) ///
        graphregion(color(white)) ///
        plotregion(margin(zero) fcolor(gs0) ifcolor(white) ilwidth(thick)) ///
        saving(temp2, replace) ///
        caption("{bf:Id: `id_mover', CCEI_i: `cceimover', CCEI_g: `cceig', I_ig: `I_igmover'}", size(3.5))

    * Combine & Export (Endline)
    gr combine temp1.gph temp2.gph, title("Group ID: `gid' (Endline)", size(4)) graphregion(color(white))
    gr export "$grph\relconsumption`gid'_Endline.png", replace

    erase temp1.gph
    erase temp2.gph
}

