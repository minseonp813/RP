clear all
set more off

do "programs/placebo_normalized_regressions_one_measure.do" ccei
do "programs/placebo_normalized_regressions_one_measure.do" hm
do "programs/placebo_normalized_regressions_one_measure.do" maxmpi

di as result "CCEI, HM, and MaxMPI placebo-normalized regressions completed."
