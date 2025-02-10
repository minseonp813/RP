log using 1_SimResults_020920.log, replace 

********************************************************************************
* WRITTEN BY MINSEON PARK 02-09-20
* Clean Simulation Results and Get the Cutoff
******************************************************************************** 

set more off
set matsize 8000

cd "C:\Users\minseon\Dropbox\RP\Data"
global mmi = "C:\Users\minseon\Dropbox\RP\Estimation_Money Metric Index\Code Package"
global source = "C:\Users\minseon\Dropbox\RP\Data\SourceDataSets"
global mmi_post = "C:\Users\minseon\Dropbox\RP\Estimation_Money Metric Index\Post Estimation_MS"
global results_int = "C:\Users\minseon\Dropbox\RP\Data\Results_Internal"


**********
* I. Cleaning Estimated Data Sets
* III. Summary Statistics
**********


********************************************************************************
* 				  		I. Cleaning Estimated Data Sets
********************************************************************************

*****
* CCEI
*****

import delimited "$mmi\Indices-Results-31-Jan-2020-819293-Sim.csv", clear
g id=floor(subject/10)
g game_type=mod(subject,10)

ren (afriat_index varian_index_min varian_index_mean varian_index_avgssq) (Iv_afriat Iv_varian1 Iv_varian2 Iv_varian3)

keep id game_type Iv_afriat-isexact
order id game_type Iv_afriat-isexact

save GARPResults_Sim.dta, replace


*****
*Cleaning Function Estimstion Results
*****

*** CARA-DA
import delimited "$mmi\MMIResults-CARA-01-29-2020-831091-DA-Sim.csv", clear
g id=floor(subject/10)
g game_type=mod(subject,10)

# delimit ;
ren (a simulated_sd_a simulated_low_95_a simulated_high_95_a mme_criterion_avgsumofsquareswas)
	(r r_sd r_low95 r_hi95 Im) ;
# delimit cr

g a=1/(2+beta)
g a_low95 =1/(2+simulated_low_95_beta)
g a_hi95 = 1/(2+simulated_high_95_beta)
g a_sd = simulated_sd_beta / (2+beta)^4 // Delta method

keep id game_type a* r* Im
order id game_type a* r* Im
save MMI_CARA_Sim.dta, replace


*** CRRA-DA
import delimited "$mmi\MMIResults-CRRA-01-31-2020-819291-DA-Sim.csv", clear
g id=floor(subject/10)
g game_type=mod(subject,10)

# delimit ;
ren (r simulated_sd_r simulated_low_95_r simulated_high_95_r mme_criterion_avgsumofsquareswas) 
	(r_crra r_sd_crra r_low95_crra r_hi95_crra Im_crra) ;
# delimit cr

g a_crra=1/(2+beta)
g a_low95_crra =1/(2+simulated_low_95_beta)
g a_hi95_crra = 1/(2+simulated_high_95_beta)
g a_sd_crra = simulated_sd_beta / (2+beta)^4

keep id game_type a* r* Im_crra
order id game_type a* r* Im_crra
save MMI_CRRA_Sim.dta, replace


********************************************************************************
* 				  			II. Summary Statistics
********************************************************************************

use GARPResults_Sim.dta, clear
su Iv_afriat-Iv_varian3, de

foreach x of varlist Iv_afriat-Iv_varian3 {
cdfplot `x'
gr export "$results_int\ `x'_cdf.png", replace
}

outreg2 using "$results_int\Summary.xls", replace sum(detail) keep(Iv_afriat-Iv_varian3)

use MMI_CARA_Sim.dta, clear
su Im, de
cdfplot Im
gr export "$results_int\Im_cdf.png", replace

outreg2 using "$results_int\Summary.xls", append sum(detail) keep(a-Im)

use MMI_CRRA_Sim.dta, clear
su Im_crra, de
cdfplot Im_crra
gr export "$results_int\Im_crra_cdf.png", replace

outreg2 using "$results_int\Summary.xls", append sum(detail) keep(a_crra-Im_crra)

log c
