use "data/finalized_panel_individual_250831.dta", clear

collapse new2_I_ig (sd) sd = new2_I_ig (count) n = new2_I_ig, by(risk_q3_i)
generate hi = new2_I_ig + invttail(n-1,0.025)*(sd / sqrt(n))
generate low = new2_I_ig - invttail(n-1,0.025)*(sd / sqrt(n))

twoway (bar new2_I_ig risk_q3_i) (rcap hi low risk_q3_i), ytitle("Mean Revealed Preference Difference") ylabel(0(0.2)0.8) xtitle("Whose Suggestion") xlabel(1 "Mostly Partner's" 2 "Both" 3 "Mostly Mine" 4 "Neither",labsize(med) ) legend(off)
gr export "results/ccei_bargaining_whose_suggestion_s.png", replace

use "data/finalized_panel_individual_250831.dta", clear

g RA_dif = abs(RA_i-RA_j)
su RA_dif, de
local med = r(p50)
g RA_dif_high= cond(RA_dif>=`med',1,0)

collapse new2_I_ig (sd) sd = new2_I_ig (count) n = new2_I_ig, by(risk_q2_i RA_dif_high)
generate hi = new2_I_ig + invttail(n-1,0.025)*(sd / sqrt(n))
generate low = new2_I_ig - invttail(n-1,0.025)*(sd / sqrt(n))

twoway (bar new2_I_ig risk_q2_i if RA_dif_high==1) (rcap hi low risk_q2_i if RA_dif_high==1) , ytitle("Mean Revealed Preference Difference") ylabel(0(0.2)0.8) xtitle("Had Individually Decided") xlabel(1 "Very Differently" 2 "Somewhat Differently" 3 "Somewhat Similar" 4 "Mostly Similar",labsize(med) ) legend(off)
gr export "results/ccei_bargaining_had_individual_s.png", replace

twoway (bar new2_I_ig risk_q2_i if RA_dif_high==0) (rcap hi low risk_q2_i if RA_dif_high==0) , ytitle("Mean Revealed Preference Difference") ylabel(0(0.2)0.8) xtitle("Had Individually Decided") xlabel(1 "Very Differently" 2 "Somewhat Differently" 3 "Somewhat Similar" 4 "Mostly Similar",labsize(med) ) legend(off)
