clear all
set more off

do "programs/placebo_normalized_regressions_one_measure.do" ccei
do "programs/placebo_normalized_regressions_one_measure.do" hm
do "programs/placebo_normalized_regressions_one_measure.do" maxmpi

* Current manuscript Table 4 uses Table 3's six specifications without RA controls.
do "99_22_table4_placebo_normalization.do"

di as result "CCEI, HM, and MaxMPI placebo-normalized regressions completed."
