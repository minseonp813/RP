************************************************************************
* WRITTEN BY Euncheol Shin
* Tables
************************************************************************

cls

* summary statistics of CCEI_i
use "data/panel_individual.dta", clear
sum ccei_i if post == 0, detail
sum ccei_i if post == 1, detail

* summary statistics of CCEI_g
use "data/panel_pbl.dta", clear
sum ccei_g if endline == 0, detail
sum ccei_g if endline == 1, detail
 