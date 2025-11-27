
cls

clear
 
*--------------------------------------------------------------------------
* Start
*--------------------------------------------------------------------------

use Data_DG, replace 
    
keep if attrition == 0
drop if distance_t == .

***** school dummies
generate sch11 = 0
generate sch12 = 0
generate sch13 = 0
generate sch14 = 0
generate sch15 = 0
generate sch16 = 0

replace sch11 = 1 if school == 11
replace sch12 = 1 if school == 12
replace sch13 = 1 if school == 13
replace sch14 = 1 if school == 14
replace sch15 = 1 if school == 15
replace sch16 = 1 if school == 16

generate sch21 = 0
generate sch22 = 0
generate sch23 = 0
generate sch24 = 0
generate sch25 = 0
generate sch26 = 0

replace sch21 = 1 if school == 21
replace sch22 = 1 if school == 22
replace sch23 = 1 if school == 23
replace sch24 = 1 if school == 24
replace sch25 = 1 if school == 25
replace sch26 = 1 if school == 26

***** Panel A: 남녀합반만 
preserve

keep if schooltype == 1

* non-linear treatment = 0 & t = 0  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 0 & t == 0 ///
, cl(id) initial(g0 -1)

matrix define a00 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 0 & t = 1  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 0 & t == 1 ///
, cl(id) initial(g0 -1)
matrix define a01 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 1 & t = 0  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 1 & t == 0 ///
, cl(id) initial(g0 -1)
matrix define a10 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 1 & t = 1  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 1 & t == 1 ///
, cl(id) initial(g0 -1)
matrix define a11 = [e(b)[1,1], e(b)[1,2]]

***

matrix define a = [a00\ a01\ a10\ a11]

restore

***** Panel B: 남녀 분반만

preserve

keep if schooltype == 2

* non-linear treatment = 0 & t = 0  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 0 & t == 0 ///
, cl(id) initial(g0 -1)
matrix define b00 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 0 & t = 1  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 0 & t == 1 ///
, cl(id) initial(g0 -1)
matrix define b01 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 1 & t = 0  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 1 & t == 0 ///
, cl(id) initial(g0 -1)
matrix define b10 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 1 & t = 1  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 1 & t == 1 ///
, cl(id) initial(g0 -1)
matrix define b11 = [e(b)[1,1], e(b)[1,2]]

***

matrix define b = [b00\ b01\ b10\ b11]

restore

***** Panel C
preserve

keep if schooltype == 3

* non-linear treatment = 0 & t = 0  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 0 & t == 0 ///
, cl(id) initial(g0 -1)
matrix define c00 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 0 & t = 1  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 0 & t == 1 ///
, cl(id) initial(g0 -1)
matrix define c01 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 1 & t = 0  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 1 & t == 0 ///
, cl(id) initial(g0 -1)
matrix define c10 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 1 & t = 1  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 1 & t == 1 ///
, cl(id) initial(g0 -1)
matrix define c11 = [e(b)[1,1], e(b)[1,2]]

***

matrix define c = [c00\ c01\ c10\ c11]

restore

***** Panel D 모든 학교 
preserve

* non-linear treatment = 0 & t = 0  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 0 & t == 0 ///
, cl(id) initial(g0 -1)

matrix define d00 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 0 & t = 1  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 0 & t == 1 ///
, cl(id) initial(g0 -1)
matrix define d01 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 1 & t = 0  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 1 & t == 0 ///
, cl(id) initial(g0 -1)
matrix define d10 = [e(b)[1,1], e(b)[1,2]]

* non-linear treatment = 1 & t = 1  
nl ( ///
dt_share = ({d0})*distance_t^({g0}) ///
 + ( {a0}  ///
 + {b_female}*female + {b_height}*height + {b_weight}*weight + {b_motherage}*motherage + {b_fatherage}*fatherage ///
 + {b_motheredu}*motheredu + {b_fatheredu}*fatheredu + {b_firstborn}*firstborn  ///
 + {b_mathscore}*mathscore + {b_selfesteem}*selfesteem + {b_outgoing}*outgoing + {b_agreeable}*agreeable ///
 + {b_conscientious}*conscientious + {b_stable}*stable + {b_opened}*opened  ///
 + {b_same_height}*same_height + {b_same_weight}*same_weight + {b_same_class}*same_class + {b_same_sex}*same_sex ///
 + {b_sch11}*sch11 + {b_sch12}*sch12 + {b_sch13}*sch13 + {b_sch14}*sch14 + {b_sch15}*sch15 + {b_sch16}*sch16 ///
 + {b_sch21}*sch21 + {b_sch22}*sch22 + {b_sch23}*sch23 + {b_sch24}*sch24 + {b_sch25}*sch25 + {b_sch26}*sch26 ) ///
) ///
if treatment == 1 & t == 1 ///
, cl(id) initial(g0 -1)
matrix define d11 = [e(b)[1,1], e(b)[1,2]]

***

matrix define d = [d00\ d01\ d10\ d11]

restore

***

matrix define all = [a\b\c\d]
putexcel set Result_Simulation_NLcoefficients.csv, sheet("temp") replace
putexcel A1 = matrix(all)

* Then save convert the output file into nlcoefficients.mat for simulation
