********************************************************************************
* Generate Descriptive Statistics 
* Written by Minseon Park 09012025
********************************************************************************

set more off
set matsize 8000

* Add path to ado files
adopath + "`c(pwd)'"

********************************************************************************
* Table: Descriptive Stats
********************************************************************************

use "data/finalized_panel_individual_250831.dta", clear

* Calculate correlations first
preserve
keep id ccei_i RA_i new2_I_ig inclass_n_friends_i inclass_popularity_i mathscore_i post
reshape wide ccei_i RA_i new2_I_ig inclass_n_friends_i inclass_popularity_i mathscore_i, i(id) j(post)

* Store correlations and significance using loop
local corr_vars "ccei_i RA_i new2_I_ig inclass_n_friends_i inclass_popularity_i mathscore_i"
local corr_names "ccei ra ig outdeg indeg math"

local j = 1
foreach var of local corr_vars {
    local name : word `j' of `corr_names'
    quietly pwcorr `var'0 `var'1, sig
    local corr_`name' = r(rho)
    local pval_`name' = r(sig)[2,1]
    
    * Add significance stars
    local stars_`name' ""
    if `pval_`name'' < 0.01 {
        local stars_`name' "**"
    }
    else if `pval_`name'' < 0.05 {
        local stars_`name' "*"
    }
    else if `pval_`name'' < 0.1 {
        local stars_`name' "+"
    }
    local ++j
}
restore

* Keep baseline observations only
keep if post==0

* Label variables nicely
label variable ccei_i "CCEI"
label variable RA_i "Risk Attitude"
label variable new2_I_ig "Bargaining Power Index"
label variable male_i "Male"
label variable height_i "Height"
label variable inclass_n_friends_i "Out-Degree"
label variable inclass_popularity_i "In-Degree"
label variable mathscore_i "Math Score"
label variable outgoing_i "Outgoing"
label variable opened_i "Opened"
label variable agreeable_i "Agreeable"
label variable conscientious_i "Conscientious"
label variable stable_i "Stable"

* Create the summary statistics table
* Calculate summary statistics using loops
foreach var in ccei_i RA_i new2_I_ig male_i height_i inclass_n_friends_i inclass_popularity_i mathscore_i outgoing_i opened_i agreeable_i conscientious_i stable_i {
    quietly summarize `var'
    local `var'_mean = r(mean)
    local `var'_sd = r(sd)
    local `var'_N = r(N)
}

* Store N for each panel
local N_panelA = `ccei_i_N'
local N_panelB1 = `male_i_N'
local N_panelB2 = `mathscore_i_N'

* Export to LaTeX using file write
cap file close myfile
file open myfile using "../Tables/table1_summary_stats.tex", write replace

file write myfile "\begin{tabular}{lccc}" _n
file write myfile "\hline \toprule " _n
file write myfile " & (1) & (2) & (3) \\" _n
file write myfile "Outcome Variable & Mean & SD & Baseline-Endline Correlation \\" _n
file write myfile "\midrule" _n

* Panel A: Measures from the Experiment
file write myfile "\multicolumn{4}{l}{\textit{Panel A: Individual-Level Measures from the Experiment}} \\" _n

local panelA_vars "ccei_i RA_i new2_I_ig"
local panelA_labels `" "CCEI" "Risk Attitude" "Bargaining Power Index" "'
local panelA_corrs "`corr_ccei' `corr_ra' `corr_ig'"
local panelA_stars "`stars_ccei' `stars_ra' `stars_ig'"

local i = 1
foreach var of local panelA_vars {
    local label : word `i' of `panelA_labels'
    local corr : word `i' of `panelA_corrs'
    local stars : word `i' of `panelA_stars'
    file write myfile "`label' & " %6.3f (``var'_mean') " & " %6.3f (``var'_sd') " & " %6.3f (`corr') "`stars' \\" _n
    local ++i
}
file write myfile "N & `N_panelA' & &  \\" _n
file write myfile "\midrule" _n

* Panel B: Characteristics
file write myfile "\multicolumn{4}{l}{\textit{Panel B: Individual-Level Other Characteristics}} \\" _n

* Demographics and Friendship Network
local panelB1_vars "male_i height_i"
local panelB1_labels `" "Male" "Height" "'

local i = 1
foreach var of local panelB1_vars {
    local label : word `i' of `panelB1_labels'
    file write myfile "`label' & " %6.3f (``var'_mean') " & " %6.3f (``var'_sd') " & \\" _n
    local ++i
}

file write myfile "\textit{Friendship Network:} & & & \\" _n

local friend_vars "inclass_n_friends_i inclass_popularity_i"
local friend_labels `" "\quad Out-Degree" "\quad In-Degree" "'
local friend_corrs "`corr_outdeg' `corr_indeg'"
local friend_stars "`stars_outdeg' `stars_indeg'"

local i = 1
foreach var of local friend_vars {
    local label : word `i' of `friend_labels'
    local corr : word `i' of `friend_corrs'
    local stars : word `i' of `friend_stars'
    file write myfile "`label' & " %6.3f (``var'_mean') " & " %6.3f (``var'_sd') " & " %6.3f (`corr') "`stars' \\" _n
    local ++i
}
file write myfile " N & `N_panelB1' & & \\" _n

* Math Score and Big 5 Personality
file write myfile "Math Score & " %6.3f (`mathscore_i_mean') " & " %6.3f (`mathscore_i_sd') " & " %6.3f (`corr_math') "`stars_math' \\" _n
file write myfile "\textit{Big 5 Personality:} & & & \\" _n

local big5_vars "outgoing_i opened_i agreeable_i conscientious_i stable_i"
local big5_labels `" "\quad Outgoing" "\quad Opened" "\quad Agreeable" "\quad Conscientious" "\quad Stable" "'

local i = 1
foreach var of local big5_vars {
    local label : word `i' of `big5_labels'
    file write myfile "`label' & " %6.3f (``var'_mean') " & " %6.3f (``var'_sd') " & \\" _n
    local ++i
}
file write myfile "N & `N_panelB2' & & \\" _n
file write myfile "\midrule" _n

* Now add group-level statistics
use "data/finalized_panel_pbl_250831.dta", clear
g friend_oneside = cond(friendship==1,1,0) if !missing(friendship)
g friend_mutual = cond(friendship==2,1,0) if !missing(friendship)

* Calculate correlations first
preserve
keep group_id ccei_g RA_g friend_* endline
reshape wide ccei_g RA_g friend_oneside friend_mutual, i(group_id) j(endline)

* Store correlations and significance using loop
local corr_vars "ccei_g RA_g friend_oneside friend_mutual"
local corr_names "ccei ra oneside mutual"

local j = 1
foreach var of local corr_vars {
    local name : word `j' of `corr_names'
    quietly pwcorr `var'0 `var'1, sig
    local corr_`name' = r(rho)
    local pval_`name' = r(sig)[2,1]
    
    * Add significance stars
    local stars_`name' ""
    if `pval_`name'' < 0.01 {
        local stars_`name' "**"
    }
    else if `pval_`name'' < 0.05 {
        local stars_`name' "*"
    }
    else if `pval_`name'' < 0.1 {
        local stars_`name' "+"
    }
    local ++j
}
restore

* Keep baseline observations only
keep if endline==0

* Label variables nicely - group level
label variable ccei_g "CCEI"
label variable RA_g "Risk Attitude"
label variable friend_oneside "Friendship: One-sided"
label variable friend_mutual "Friendship: Mutual"

* Create the summary statistics table
* Calculate summary statistics using loops - group level
foreach var in ccei_g RA_g friend_oneside friend_mutual {
    quietly summarize `var'
    local `var'_mean = r(mean)
    local `var'_sd = r(sd)
    local `var'_N = r(N)
}

* Store N for each panel - group level
local N_panelC = `ccei_g_N'
local N_panelD = `friend_oneside_N'

* Panel C: Group-Level Measures from the Experiment
file write myfile "\multicolumn{4}{l}{\textit{Panel C: Group-Level Measures from the Experiment}} \\" _n

local panelC_vars "ccei_g RA_g"
local panelC_labels `" "CCEI" "Risk Attitude" "'
local panelC_corrs "`corr_ccei' `corr_ra'"
local panelC_stars "`stars_ccei' `stars_ra'"

local i = 1
foreach var of local panelC_vars {
    local label : word `i' of `panelC_labels'
    local corr : word `i' of `panelC_corrs'
    local stars : word `i' of `panelC_stars'
    file write myfile "`label' & " %6.3f (``var'_mean') " & " %6.3f (``var'_sd') " & " %6.3f (`corr') "`stars' \\" _n
    local ++i
}
file write myfile "N  & `N_panelC' & & \\" _n
file write myfile "\midrule" _n

* Panel D: Group-Level Friendship
file write myfile "\multicolumn{4}{l}{\textit{Panel D: Group-Level Friendship}} \\" _n

local panelD_vars "friend_oneside friend_mutual"
local panelD_labels `" "Friendship: One-sided" "Friendship: Mutual" "'
local panelD_corrs "`corr_oneside' `corr_mutual'"
local panelD_stars "`stars_oneside' `stars_mutual'"

local i = 1
foreach var of local panelD_vars {
    local label : word `i' of `panelD_labels'
    local corr : word `i' of `panelD_corrs'
    local stars : word `i' of `panelD_stars'
    file write myfile "`label' & " %6.3f (``var'_mean') " & " %6.3f (``var'_sd') " & " %6.3f (`corr') "`stars' \\" _n
    local ++i
}
file write myfile "N & `N_panelD' & &  \\" _n

file write myfile "\bottomrule \hline" _n
file write myfile "\end{tabular}" _n

file close myfile


********************************************************************************
* Table: Correlation 
********************************************************************************

use "data/finalized_panel_individual_250831.dta", clear

* Keep baseline observations only
keep if post==0

* Label variables nicely
label variable ccei_i "CCEI"
label variable RA_i "Risk Attitude"
label variable new2_I_ig "Bargaining Power Index"
label variable male_i "Male"
label variable height_i "Height"
label variable inclass_n_friends_i "Out-Degree"
label variable inclass_popularity_i "In-Degree"
label variable mathscore_i "Math Score"
label variable outgoing_i "Outgoing"
label variable opened_i "Opened"
label variable agreeable_i "Agreeable"
label variable conscientious_i "Conscientious"
label variable stable_i "Stable"

* Display correlation matrix
corr ccei_i RA_i new2_I_ig inclass_n_friends_i inclass_popularity_i male_i height_i mathscore_i outgoing_i opened_i agreeable_i conscientious_i stable_i

* Create correlation table with significance stars (transposed)
* Column variables (key experimental/network measures)
local col_vars "ccei_i RA_i new2_I_ig inclass_n_friends_i inclass_popularity_i"
* Row variables (all variables)
local row_vars "ccei_i RA_i new2_I_ig inclass_n_friends_i inclass_popularity_i male_i height_i mathscore_i outgoing_i opened_i agreeable_i conscientious_i stable_i"

local nrows : word count `row_vars'
local ncols : word count `col_vars'

* Get variable labels for columns
local j = 1
foreach var of local col_vars {
    local collabel`j' : variable label `var'
    local ++j
}

* Get variable labels for rows
local i = 1
foreach var of local row_vars {
    local rowlabel`i' : variable label `var'
    local ++i
}

* Open file for correlation table
cap file close corrfile
file open corrfile using "../Tables/table_correlation.tex", write replace

* Write table header
file write corrfile "\begin{tabular}{l*{`ncols'}{c}}" _n
file write corrfile "\hline \toprule" _n
file write corrfile " "
forvalues j = 1/`ncols' {
    file write corrfile " & (`j')"
}
file write corrfile " \\" _n
file write corrfile " "
forvalues j = 1/`ncols' {
    file write corrfile " & `collabel`j''"
}
file write corrfile " \\" _n
file write corrfile "\midrule" _n

* Define variable groups
local group1_vars "ccei_i RA_i new2_I_ig"
local group1_label "Experimental Measures"

local group2_vars "inclass_n_friends_i inclass_popularity_i"
local group2_label "Friendship Network"

local group3_vars "male_i height_i"
local group3_label "Demographics"

local group4_vars "mathscore_i"
local group4_label "Math Score"

local group5_vars "outgoing_i opened_i agreeable_i conscientious_i stable_i"
local group5_label "Big 5 Personality"

* Write correlation coefficients with significance stars and group labels
local row = 1
local current_group = 1

foreach group in 1 2 3 4 5 {
    * Write group header
    local group_label : display "`group`group'_label'"
    file write corrfile "\multicolumn{" (`ncols' + 1) "}{l}{\textit{`group_label':}} \\" _n
    
    * Write variables in this group
    foreach var1 of local group`group'_vars {
        local label : variable label `var1'
        file write corrfile "\quad `label'"
        
        local col = 1
        foreach var2 of local col_vars {
            * Check if we should show the correlation (lower triangle only, no diagonal)
            if `col' < `row' {
                * Use pwcorr with sig option to get significance levels
                quietly pwcorr `var1' `var2', sig
                local rho = r(rho)
                local pval = r(sig)[2,1]
                
                * Add significance stars
                local stars ""
                if `pval' < 0.01 {
                    local stars "**"
                }
                else if `pval' < 0.05 {
                    local stars "*"
                }
                else if `pval' < 0.1 {
                    local stars "+"
                }
                
                file write corrfile " & " %5.3f (`rho') "`stars'"
            }
            else {
                * Upper triangle and diagonal: leave empty
                file write corrfile " & "
            }
            local ++col
        }
        file write corrfile " \\" _n
        local ++row
    }
}

file write corrfile "\bottomrule \hline" _n
file write corrfile "\end{tabular}" _n
file write corrfile _n
file write corrfile "% Notes: ** p<0.01, * p<0.05, + p<0.1" _n
file write corrfile "% Lower triangle shows correlations with key variables" _n

file close corrfile
