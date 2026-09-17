* Disjoint-choice test.

clear all
set more off
set matsize 8000

args split_reps definition
if `"`split_reps'"'=="" local split_reps 500
confirm integer number `split_reps'
assert `split_reps'>=1

local split_seed 20260812
local code_dir `"`c(pwd)'"'
if "`definition'" == "" {
    foreach definition in both_high both_low ccei_diff {
        do "`code_dir'/99_9_disjoint_choice.do" `split_reps' `definition'
    }
    exit
}
if !inlist("`definition'", "both_high", "both_low", "ccei_diff") {
    di as error "Unknown definition: `definition'"
    exit 198
}
local suffix = cond("`definition'"=="both_high", "", cond("`definition'"=="both_low", "_bothlow", "_cceidiff"))
global D_DEFINITION "`definition'"
local replication_dir `"`code_dir'"'
local data_dir `"`replication_dir'/data"'
local results_dir `"`code_dir'/results/tests/disjoint`suffix'"'
cap mkdir `"`code_dir'/results"'
cap mkdir `"`code_dir'/results/tests"'
cap mkdir `"`results_dir'"'

foreach f in panel_individual.dta base_raw.dta end_raw.dta {
    confirm file `"`data_dir'/`f'"'
}
* GARP and Cross-index functions.

mata:
real matrix d_closure(real matrix R)
{
    real scalar k, n
    n = rows(R)
    for (k=1; k<=n; k++) R = R :| (R[,k] * R[k,])
    return(R)
}

real scalar d_garp(real matrix p, real matrix x, real scalar e)
{
    real scalar n
    real colvector budget
    real matrix R, P0, cost
    n = rows(p)
    budget = e :* rowsum(p:*x)
    cost = p*x'
    R = d_closure((budget*J(1,n,1)):>=cost)
    P0 = (budget*J(1,n,1)):>cost
    return(sum(sum(R :& P0'))==0)
}

real scalar d_ccei(real matrix z)
{
    real scalar lo, hi, e, out
    real matrix p, x
    if (rows(z)<1) return(.)
    p=(1:/z[,3]),(1:/z[,4]); x=z[,1],z[,2]
    if (d_garp(p,x,1)) return(1)
    lo=0; hi=1; out=0
    while (hi-lo>1e-6) {
        e=(lo+hi)/2
        if (d_garp(p,x,e)) {
            out=e
            lo=e
        }
        else {
            hi=e
        }
    }
    return(out)
}

real scalar d_cross_garp(real matrix p, real matrix x, real colvector side,
    real scalar e)
{
    real scalar n,i,j
    real colvector budget,comp
    real matrix R,P0,cost
    n=rows(p); budget=e:*rowsum(p:*x); cost=p*x'
    R=d_closure((budget*J(1,n,1)):>=cost)
    P0=(budget*J(1,n,1)):>cost
    for (i=1;i<=n;i++) for (j=1;j<=n;j++) {
        if (P0[i,j] & R[j,i]) {
            comp=(R[i,]':&R[,i]); comp[i]=1; comp[j]=1
            if (sum(comp:&(side:==1))>0 & sum(comp:&(side:==2))>0) return(0)
        }
    }
    return(1)
}

real scalar d_ex(real matrix z)
{
    real scalar lo,hi,e,out,n,i,j,bi,r,top
    real matrix p,x
    real colvector side
    if (cols(z)==6) z=z[,(1,3,4,5,6)]
    if (rows(z)<2 | sum(z[,1]:==3)==0 | sum(z[,1]:!=3)==0) return(.)
    p=(1:/z[,4]),(1:/z[,5]); x=z[,2],z[,3]
    side=1:+(z[,1]:==3)
    if (d_cross_garp(p,x,side,1)) return(1)
    lo=0; hi=1; out=0
    while (hi-lo>1e-6) {
        e=(lo+hi)/2
        if (d_cross_garp(p,x,side,e)) {
            out=e
            lo=e
        }
        else {
            hi=e
        }
    }
    n=rows(p); top=0
    for (i=1;i<=n;i++) {
        bi=p[i,]*x[i,]'
        for (j=1;j<=n;j++) if (j!=i) {
            r=(p[i,]*x[j,]')/bi
            if (r<1-1e-12 & r>top) top=r
        }
    }
    if (out>top+1e-9) return(1)
    return(out)
}

real scalar d_ihat(real scalar ex1, real scalar ex2, real scalar ex12)
{
    real scalar den
    if (missing(ex1)|missing(ex2)|missing(ex12)) return(.)
    den=1-ex12
    if (den<=1e-6) return(.)
    return(.5+((1-ex1)-(1-ex2))/(2*den))
}

void d_compute()
{
    real matrix info,z,q
    real colvector idx,out1,out2,out3,out4,out5,out6,out7,out8
    real scalar g,G,c1a,c2a,c1b,c2b,e1,e2,e12
    info=panelsetup(st_data(.,"case"),1); G=rows(info)
    out1=out2=out3=out4=out5=out6=out7=out8=J(st_nobs(),1,.)
    for (g=1;g<=G;g++) {
        idx=(info[g,1]::info[g,2]); z=st_data(idx,("role","half","coord_x",
            "coord_y","intercept_x","intercept_y"))
        c1a=d_ccei(select(z[,3..6],(z[,1]:==1):&(z[,2]:==1)))
        c2a=d_ccei(select(z[,3..6],(z[,1]:==2):&(z[,2]:==1)))
        c1b=d_ccei(select(z[,3..6],(z[,1]:==1):&(z[,2]:==2)))
        c2b=d_ccei(select(z[,3..6],(z[,1]:==2):&(z[,2]:==2)))

        q=select(z,(z[,2]:==1):&((z[,1]:==1):|(z[,1]:==3))); e1=d_ex(q)
        q=select(z,(z[,2]:==1):&((z[,1]:==2):|(z[,1]:==3))); e2=d_ex(q)
        q=select(z,z[,2]:==1); e12=d_ex(q)
        out5[idx]=J(rows(idx),1,d_ihat(e1,e2,e12))
        out6[idx]=J(rows(idx),1,d_ihat(e2,e1,e12))

        q=select(z,(z[,2]:==2):&((z[,1]:==1):|(z[,1]:==3))); e1=d_ex(q)
        q=select(z,(z[,2]:==2):&((z[,1]:==2):|(z[,1]:==3))); e2=d_ex(q)
        q=select(z,z[,2]:==2); e12=d_ex(q)
        out7[idx]=J(rows(idx),1,d_ihat(e1,e2,e12))
        out8[idx]=J(rows(idx),1,d_ihat(e2,e1,e12))
        out1[idx]=J(rows(idx),1,c1a); out2[idx]=J(rows(idx),1,c2a)
        out3[idx]=J(rows(idx),1,c1b); out4[idx]=J(rows(idx),1,c2b)
    }
    st_addvar("double",("ccei1_A","ccei2_A","ccei1_B","ccei2_B",
        "I1_A","I2_A","I1_B","I2_B"))
    st_store(.,"ccei1_A",out1); st_store(.,"ccei2_A",out2)
    st_store(.,"ccei1_B",out3); st_store(.,"ccei2_B",out4)
    st_store(.,"I1_A",out5); st_store(.,"I2_A",out6)
    st_store(.,"I1_B",out7); st_store(.,"I2_B",out8)
}
end

* Split role-level choices.

tempfile raw both_members measures pair_roster
tempfile full_validation role_raw disjoint_reg_base

use `"`data_dir'/panel_individual.dta"', clear
keep group_id post class
duplicates drop
isid group_id post
save `pair_roster'

use `"`data_dir'/base_raw.dta"', clear
gen byte post=0
append using `"`data_dir'/end_raw.dta"'
replace post=1 if missing(post)
keep if inrange(round_number,1,36)
drop if missing(coord_x,coord_y,intercept_x,intercept_y)
drop if intercept_x==0 | intercept_y==0
save `raw'

keep if inrange(round_number,1,18)
merge m:1 group_id post id using `"`data_dir'/panel_individual.dta"', ///
    keep(match) keepusing(person class) nogen
gen byte role=person
keep group_id class post role round_number coord_x coord_y intercept_x intercept_y
save `both_members'

use `raw', clear
keep if inrange(round_number,19,36) & mover==1
gen byte role=3
replace round_number=round_number-18
keep group_id post role round_number coord_x coord_y intercept_x intercept_y
merge m:1 group_id post using `pair_roster', ///
    keep(match) keepusing(class) nogen
duplicates drop group_id post role round_number, force
append using `both_members'
isid group_id post role round_number
gen byte corner_choice=(coord_x==0|coord_y==0) if role<=2
gen byte mid_choice=(coord_x==coord_y) if role<=2
gen double expensive_choice=cond(intercept_x<intercept_y,coord_x,coord_y) ///
    if role<=2
gen double ra_choice=expensive_choice/(coord_x+coord_y) ///
    if role<=2 & coord_x+coord_y!=0
save `role_raw'

* Validate the index calculation.
preserve
    expand 2, generate(second_copy)
    gen byte half=second_copy+1
    drop second_copy
    sort group_id post role half round_number
    egen long case=group(group_id post)
    mata: d_compute()
    keep group_id post I1_A I2_A
    collapse (firstnm) I1_A I2_A, by(group_id post)
    save `full_validation'
restore

preserve
    use `"`data_dir'/panel_individual.dta"', clear
    merge m:1 group_id post using `full_validation', assert(match) nogen
    gen double Ihat_check=cond(person==1,I1_A,I2_A)
    assert abs(Ihat_check-Ihat_ig)<2e-6 if !missing(Ihat_check,Ihat_ig)
    assert missing(Ihat_check)==missing(Ihat_ig)
restore

set rng mt64
set seed `split_seed'
gen double split_random=runiform()
bysort group_id post role (split_random round_number): gen byte half=cond(_n<=9,1,2)
bysort group_id post role: assert _N==18
bysort group_id post role half: assert _N==9
sort group_id post role half round_number
egen long case=group(group_id post)
mata: d_compute()

bysort group_id post: egen double corner1_A=mean(cond(role==1 & half==1,corner_choice,.))
bysort group_id post: egen double corner2_A=mean(cond(role==2 & half==1,corner_choice,.))
bysort group_id post: egen double corner1_B=mean(cond(role==1 & half==2,corner_choice,.))
bysort group_id post: egen double corner2_B=mean(cond(role==2 & half==2,corner_choice,.))
bysort group_id post: egen double mid1_A=mean(cond(role==1 & half==1,mid_choice,.))
bysort group_id post: egen double mid2_A=mean(cond(role==2 & half==1,mid_choice,.))
bysort group_id post: egen double mid1_B=mean(cond(role==1 & half==2,mid_choice,.))
bysort group_id post: egen double mid2_B=mean(cond(role==2 & half==2,mid_choice,.))
bysort group_id post: egen double ra1_A=mean(cond(role==1 & half==1,ra_choice,.))
bysort group_id post: egen double ra2_A=mean(cond(role==2 & half==1,ra_choice,.))
bysort group_id post: egen double ra1_B=mean(cond(role==1 & half==2,ra_choice,.))
bysort group_id post: egen double ra2_B=mean(cond(role==2 & half==2,ra_choice,.))
foreach v in ra1_A ra2_A ra1_B ra2_B {
    replace `v'=round(`v',.00001)
}

keep case group_id class post ccei1_A ccei2_A ccei1_B ccei2_B ///
    I1_A I2_A I1_B I2_B corner1_A corner2_A corner1_B corner2_B ///
    mid1_A mid2_A mid1_B mid2_B ra1_A ra2_A ra1_B ra2_B
collapse (firstnm) class post ccei1_A ccei2_A ccei1_B ccei2_B ///
    I1_A I2_A I1_B I2_B corner1_A corner2_A corner1_B corner2_B ///
    mid1_A mid2_A mid1_B mid2_B ra1_A ra2_A ra1_B ra2_B, ///
    by(case group_id)
save `measures'

* Construct directional samples.

use `"`data_dir'/panel_individual.dta"', clear
merge m:1 group_id post using `measures', assert(match) nogen

gen double ccei_A_source=cond(person==1,ccei1_A,ccei2_A)
gen double ccei_B_source=cond(person==1,ccei1_B,ccei2_B)
gen double I_A=cond(person==1,I1_A,I2_A)
gen double I_B=cond(person==1,I1_B,I2_B)
if "`definition'" == "both_high" {
    gen double High_A=cond(person==1,ccei1_A>=ccei2_A,ccei2_A>=ccei1_A) if !missing(ccei1_A,ccei2_A)
    gen double High_B=cond(person==1,ccei1_B>=ccei2_B,ccei2_B>=ccei1_B) if !missing(ccei1_B,ccei2_B)
}
else if "`definition'" == "both_low" {
    gen double High_A=cond(person==1,ccei1_A>ccei2_A,ccei2_A>ccei1_A) if !missing(ccei1_A,ccei2_A)
    gen double High_B=cond(person==1,ccei1_B>ccei2_B,ccei2_B>ccei1_B) if !missing(ccei1_B,ccei2_B)
}
else {
    gen double High_A=cond(person==1,ccei1_A-ccei2_A,ccei2_A-ccei1_A)
    gen double High_B=cond(person==1,ccei1_B-ccei2_B,ccei2_B-ccei1_B)
}
gen byte tie_A=(ccei1_A==ccei2_A) if !missing(ccei1_A,ccei2_A)
gen byte tie_B=(ccei1_B==ccei2_B) if !missing(ccei1_B,ccei2_B)

gen double corner_share_A_i=cond(person==1,corner1_A,corner2_A)
gen double corner_share_A_j=cond(person==1,corner2_A,corner1_A)
gen double corner_share_A_diff=corner_share_A_i-corner_share_A_j
gen double mid_share_A_i=cond(person==1,mid1_A,mid2_A)
gen double mid_share_A_j=cond(person==1,mid2_A,mid1_A)
gen double mid_share_A_diff=mid_share_A_i-mid_share_A_j
gen double corner_share_B_i=cond(person==1,corner1_B,corner2_B)
gen double corner_share_B_j=cond(person==1,corner2_B,corner1_B)
gen double corner_share_B_diff=corner_share_B_i-corner_share_B_j
gen double mid_share_B_i=cond(person==1,mid1_B,mid2_B)
gen double mid_share_B_j=cond(person==1,mid2_B,mid1_B)
gen double mid_share_B_diff=mid_share_B_i-mid_share_B_j
gen double RA_A_i=cond(person==1,ra1_A,ra2_A)
gen double RA_A_j=cond(person==1,ra2_A,ra1_A)
gen double RA_A_diff=RA_A_i-RA_A_j
gen double RA_B_i=cond(person==1,ra1_B,ra2_B)
gen double RA_B_j=cond(person==1,ra2_B,ra1_B)
gen double RA_B_diff=RA_B_i-RA_B_j
gen byte female_i_male_j=(male_i==0&male_j==1)
gen byte male_i_female_j=(male_i==1&male_j==0)
egen long id_fe=group(id)

global d_group "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff female_i_male_j male_i_female_j"
global d_group_ng "mathscore_i mathscore_diff height_i height_diff outgoing_i outgoing_diff opened_i opened_diff agreeable_i agreeable_diff conscientious_i conscientious_diff stable_i stable_diff"
global d_friend "inclass_n_friends_i inclass_n_diff inclass_popularity_i inclass_pop_diff"
global d_missing "mathscore_diff_missing outgoing_diff_missing opened_diff_missing agreeable_diff_missing conscientious_diff_missing stable_diff_missing"
global d_ra_A "RA_A_i RA_A_diff"
global d_ra_B "RA_B_i RA_B_diff"
global d_share_A "corner_share_A_i corner_share_A_diff mid_share_A_i mid_share_A_diff"
global d_share_B "corner_share_B_i corner_share_B_diff mid_share_B_i mid_share_B_diff"

label var High_A "Higher CCEI from A"
label var High_B "Higher CCEI from B"

count if !missing(I_B,High_A)
local usable_AB=r(N)
di as text "A->B usable individual observations: " r(N)
count if !missing(I_A,High_B)
local usable_BA=r(N)
di as text "B->A usable individual observations: " r(N)
summarize ccei1_A ccei2_A ccei1_B ccei2_B I1_A I2_A I1_B I2_B

save `"`results_dir'/disjoint_individual_data.dta"', replace
if `usable_AB'==0 | `usable_BA'==0 {
    preserve
        clear
        set obs 2
        gen str4 direction=cond(_n==1,"A-B","B-A")
        gen long usable_individual_observations=.
        replace usable_individual_observations=`usable_AB' in 1
        replace usable_individual_observations=`usable_BA' in 2
        save `"`results_dir'/disjoint_feasibility.dta"', replace
        export delimited using ///
            `"`results_dir'/disjoint_feasibility.csv"', replace
    restore
    di as error "The 9/9 cross index is undefined in at least one direction."
    di as error "No disjoint regression is estimable for this split."
    exit 0
}

save `disjoint_reg_base'

* Run repeated splits.

global D_ROLE_RAW `"`role_raw'"'
global D_REG_BASE `"`disjoint_reg_base'"'

capture program drop disjoint_repetition
program define disjoint_repetition, rclass
    tempfile rep_measures

    use `"$D_ROLE_RAW"', clear
    gen double split_random=runiform()
    bysort group_id post role (split_random round_number): ///
        gen byte half=cond(_n<=9,1,2)
    sort group_id post role half round_number
    egen long case=group(group_id post)
    mata: d_compute()
    bysort group_id post: egen double corner1_A=mean(cond(role==1 & half==1,corner_choice,.))
    bysort group_id post: egen double corner2_A=mean(cond(role==2 & half==1,corner_choice,.))
    bysort group_id post: egen double corner1_B=mean(cond(role==1 & half==2,corner_choice,.))
    bysort group_id post: egen double corner2_B=mean(cond(role==2 & half==2,corner_choice,.))
    bysort group_id post: egen double mid1_A=mean(cond(role==1 & half==1,mid_choice,.))
    bysort group_id post: egen double mid2_A=mean(cond(role==2 & half==1,mid_choice,.))
    bysort group_id post: egen double mid1_B=mean(cond(role==1 & half==2,mid_choice,.))
    bysort group_id post: egen double mid2_B=mean(cond(role==2 & half==2,mid_choice,.))
    bysort group_id post: egen double ra1_A=mean(cond(role==1 & half==1,ra_choice,.))
    bysort group_id post: egen double ra2_A=mean(cond(role==2 & half==1,ra_choice,.))
    bysort group_id post: egen double ra1_B=mean(cond(role==1 & half==2,ra_choice,.))
    bysort group_id post: egen double ra2_B=mean(cond(role==2 & half==2,ra_choice,.))
    foreach v in ra1_A ra2_A ra1_B ra2_B {
        replace `v'=round(`v',.00001)
    }
    keep case group_id class post ccei1_A ccei2_A ccei1_B ccei2_B ///
        I1_A I2_A I1_B I2_B corner1_A corner2_A corner1_B corner2_B ///
        mid1_A mid2_A mid1_B mid2_B ra1_A ra2_A ra1_B ra2_B
    collapse (firstnm) class post ccei1_A ccei2_A ccei1_B ccei2_B ///
        I1_A I2_A I1_B I2_B corner1_A corner2_A corner1_B corner2_B ///
        mid1_A mid2_A mid1_B mid2_B ra1_A ra2_A ra1_B ra2_B, ///
        by(case group_id)
    save `rep_measures'

    use `"$D_REG_BASE"', clear
    drop case ccei1_A ccei2_A ccei1_B ccei2_B I1_A I2_A I1_B I2_B ///
        ccei_A_source ccei_B_source I_A I_B High_A High_B tie_A tie_B ///
        corner1_A corner2_A corner1_B corner2_B mid1_A mid2_A mid1_B mid2_B ///
        ra1_A ra2_A ra1_B ra2_B RA_A_i RA_A_j RA_A_diff ///
        RA_B_i RA_B_j RA_B_diff ///
        corner_share_A_i corner_share_A_j corner_share_A_diff ///
        mid_share_A_i mid_share_A_j mid_share_A_diff ///
        corner_share_B_i corner_share_B_j corner_share_B_diff ///
        mid_share_B_i mid_share_B_j mid_share_B_diff
    merge m:1 group_id post using `rep_measures', assert(match) nogen
    gen double I_A=cond(person==1,I1_A,I2_A)
    gen double I_B=cond(person==1,I1_B,I2_B)
    if "$D_DEFINITION" == "both_high" {
        gen double High_A=cond(person==1,ccei1_A>=ccei2_A,ccei2_A>=ccei1_A) if !missing(ccei1_A,ccei2_A)
        gen double High_B=cond(person==1,ccei1_B>=ccei2_B,ccei2_B>=ccei1_B) if !missing(ccei1_B,ccei2_B)
    }
    else if "$D_DEFINITION" == "both_low" {
        gen double High_A=cond(person==1,ccei1_A>ccei2_A,ccei2_A>ccei1_A) if !missing(ccei1_A,ccei2_A)
        gen double High_B=cond(person==1,ccei1_B>ccei2_B,ccei2_B>ccei1_B) if !missing(ccei1_B,ccei2_B)
    }
    else {
        gen double High_A=cond(person==1,ccei1_A-ccei2_A,ccei2_A-ccei1_A)
        gen double High_B=cond(person==1,ccei1_B-ccei2_B,ccei2_B-ccei1_B)
    }
    gen byte tie_A=(ccei1_A==ccei2_A) if !missing(ccei1_A,ccei2_A)
    gen byte tie_B=(ccei1_B==ccei2_B) if !missing(ccei1_B,ccei2_B)
    gen double corner_share_A_i=cond(person==1,corner1_A,corner2_A)
    gen double corner_share_A_j=cond(person==1,corner2_A,corner1_A)
    gen double corner_share_A_diff=corner_share_A_i-corner_share_A_j
    gen double mid_share_A_i=cond(person==1,mid1_A,mid2_A)
    gen double mid_share_A_j=cond(person==1,mid2_A,mid1_A)
    gen double mid_share_A_diff=mid_share_A_i-mid_share_A_j
    gen double corner_share_B_i=cond(person==1,corner1_B,corner2_B)
    gen double corner_share_B_j=cond(person==1,corner2_B,corner1_B)
    gen double corner_share_B_diff=corner_share_B_i-corner_share_B_j
    gen double mid_share_B_i=cond(person==1,mid1_B,mid2_B)
    gen double mid_share_B_j=cond(person==1,mid2_B,mid1_B)
    gen double mid_share_B_diff=mid_share_B_i-mid_share_B_j
    gen double RA_A_i=cond(person==1,ra1_A,ra2_A)
    gen double RA_A_j=cond(person==1,ra2_A,ra1_A)
    gen double RA_A_diff=RA_A_i-RA_A_j
    gen double RA_B_i=cond(person==1,ra1_B,ra2_B)
    gen double RA_B_j=cond(person==1,ra2_B,ra1_B)
    gen double RA_B_diff=RA_B_i-RA_B_j

    quietly summarize tie_A if person==1
    return scalar tieA=r(mean)
    quietly summarize tie_B if person==1
    return scalar tieB=r(mean)

    quietly reghdfe I_B High_A, absorb(class) vce(cluster class)
    return scalar AB1=_b[High_A]
    quietly reghdfe I_B High_A $d_group $d_friend $d_missing, ///
        absorb(class) vce(cluster class)
    return scalar AB2=_b[High_A]
    quietly reghdfe I_B High_A $d_group $d_friend $d_missing ///
        $d_ra_A $d_share_A, absorb(class) vce(cluster class)
    return scalar AB3=_b[High_A]
    quietly reghdfe I_B High_A $d_group_ng $d_friend $d_missing ///
        $d_ra_A $d_share_A, absorb(id_fe) vce(cluster class)
    return scalar AB4=_b[High_A]

    quietly reghdfe I_A High_B, absorb(class) vce(cluster class)
    return scalar BA1=_b[High_B]
    quietly reghdfe I_A High_B $d_group $d_friend $d_missing, ///
        absorb(class) vce(cluster class)
    return scalar BA2=_b[High_B]
    quietly reghdfe I_A High_B $d_group $d_friend $d_missing ///
        $d_ra_B $d_share_B, absorb(class) vce(cluster class)
    return scalar BA3=_b[High_B]
    quietly reghdfe I_A High_B $d_group_ng $d_friend $d_missing ///
        $d_ra_B $d_share_B, absorb(id_fe) vce(cluster class)
    return scalar BA4=_b[High_B]

    keep if person==1
    gen double dccei_A=abs(ccei1_A-ccei2_A)
    gen double dI_B=cond(ccei1_A>ccei2_A,I1_B-I2_B, ///
        cond(ccei2_A>ccei1_A,I2_B-I1_B,0))
    gen double dccei_B=abs(ccei1_B-ccei2_B)
    gen double dI_A=cond(ccei1_B>ccei2_B,I1_A-I2_A, ///
        cond(ccei2_B>ccei1_B,I2_A-I1_A,0))
    quietly regress dI_B dccei_A, vce(cluster class)
    return scalar diffAB=_b[dccei_A]
    quietly regress dI_A dccei_B, vce(cluster class)
    return scalar diffBA=_b[dccei_B]
end

simulate AB1=r(AB1) AB2=r(AB2) AB3=r(AB3) AB4=r(AB4) ///
    BA1=r(BA1) BA2=r(BA2) BA3=r(BA3) BA4=r(BA4) ///
    diffAB=r(diffAB) diffBA=r(diffBA) tieA=r(tieA) tieB=r(tieB), ///
    reps(`split_reps') seed(`split_seed') nodots: disjoint_repetition

gen int repetition=_n
order repetition
save `"`results_dir'/disjoint_repetitions_`split_reps'.dta"', replace
export delimited using ///
    `"`results_dir'/disjoint_repetitions_`split_reps'.csv"', replace

* Save results.
tempfile repeated_results
save `repeated_results'
postfile summary str12 statistic double median lower upper share_negative ///
    long n_estimates ///
    using `"`results_dir'/disjoint_repetitions_summary.dta"', replace

forvalues s=1/4 {
    preserve
        keep AB`s' BA`s'
        stack AB`s' BA`s', into(coefficient) clear
        drop if missing(coefficient)
        quietly summarize coefficient, detail
        local med=r(p50)
        quietly _pctile coefficient, percentiles(2.5 97.5)
        local lo=r(r1)
        local hi=r(r2)
        quietly count if coefficient<0
        local neg=r(N)
        quietly count
        local den=r(N)
    restore
    post summary ("spec`s'") (`med') (`lo') (`hi') (`neg'/`den') (`den')
}

foreach v in diffAB diffBA {
    quietly summarize `v', detail
    local med=r(p50)
    quietly _pctile `v', percentiles(2.5 97.5)
    local lo=r(r1)
    local hi=r(r2)
    quietly count if `v'<0 & !missing(`v')
    local neg=r(N)
    quietly count if !missing(`v')
    local den=r(N)
    post summary ("`v'") (`med') (`lo') (`hi') (`neg'/`den') (`den')
}
postclose summary
use `"`results_dir'/disjoint_repetitions_summary.dta"', clear
format median lower upper share_negative %9.3f
export delimited using ///
    `"`results_dir'/disjoint_repetitions_summary.csv"', replace
list, noobs clean

use `repeated_results', clear
quietly summarize tieA
local tieA_mean=r(mean)
quietly summarize tieB
local tieB_mean=r(mean)

macro drop D_ROLE_RAW D_REG_BASE D_DEFINITION
di as result "Repeated disjoint-data test completed: `split_reps' splits."
di as result "Mean tie shares: A = " %6.3f `tieA_mean' ///
    "; B = " %6.3f `tieB_mean'
di as result "Outputs: `results_dir'"
