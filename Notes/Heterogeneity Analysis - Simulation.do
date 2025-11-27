* Step 1: Generate 500 pairs, 2 individuals per pair
clear
set obs 500
gen pair_id = _n
expand 2
gen id = _n
bysort pair_id: gen mover = mod(_n, 2) 

* Step 2: Expand each individual to two time periods (pre, post)
expand 2
bysort id (pair_id): gen t = _n - 1  // t = 0 (pre), t = 1 (post)
gen post = t

* Create a unique identifier for each pair-time combination
egen pair_time = group(pair_id t)

* Randomly assign HighCCEI to one person in each pair-time
gen HighCCEI = .
gen rand = runiform()
bysort pair_time (rand): gen HighCCEI_flag = (_n == 1)
replace HighCCEI = HighCCEI_flag
drop rand HighCCEI_flag

* Enforce y_it + y_jt = 1
gen base_prob = 0.4 + 0.1*HighCCEI + 0.1*HighCCEI*post ///
	+ 0.1*mover+ 0.1*mover*HighCCEI + 0.1*mover*post + 0.1*mover*HighCCEI*post
bysort pair_id t (id): gen y = .
gen noise = rnormal(0, 0.05)
bysort pair_id t (id): replace y = base_prob + noise if _n == 1
bysort pair_id t (id): replace y = 1 - y[_n-1] if _n == 2

* Clip y to [0,1]
replace y = max(0, min(1, y))

* Create interaction terms
gen mover_HighCCEI = mover * HighCCEI
gen mover_post = mover * post
gen mover_HighCCEI_post = mover * HighCCEI * post
gen HighCCEI_post = HighCCEI * post

* Regression
eststo clear
eststo: reg y HighCCEI post HighCCEI_post mover mover_HighCCEI mover_post mover_HighCCEI_post, r cl(pair_id)
eststo: reg y HighCCEI post HighCCEI_post mover mover_HighCCEI mover_post mover_HighCCEI_post, r
eststo: reg y HighCCEI HighCCEI_post mover mover_HighCCEI mover_post mover_HighCCEI_post, r cl(pair_id)
eststo: reg y HighCCEI post HighCCEI_post mover mover_HighCCEI mover_post mover_HighCCEI_post, r cl(pair_id) nocons
eststo: reg y HighCCEI post HighCCEI_post mover mover_HighCCEI mover_post mover_HighCCEI_post if id<500, r 
esttab, label se

eststo clear
eststo: reg y HighCCEI HighCCEI_post , r cl(pair_id)
eststo: reg y HighCCEI post HighCCEI_post, r cl(pair_id) nocons
eststo: reg y HighCCEI post HighCCEI_post if id<500, r 
esttab, label se

sort pair_id t id

