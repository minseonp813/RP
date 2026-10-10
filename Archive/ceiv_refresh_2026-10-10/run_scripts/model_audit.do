clear all
set more off
foreach version in old new {
    local file "results/new_indices/collective_rationality_summary/figure6.ster"
    if "`version'"=="old" local file "../Archive/ceiv_refresh_2026-10-10/Code/results/new_indices/collective_rationality_summary/figure6.ster"
    estimates use "`file'"
    di "MODEL: `version'"
    ereturn list
    matrix audit_`version'_b = e(b)
    matrix audit_`version'_V = e(V)
}
mata:
    oldb=st_matrix("audit_old_b")
    newb=st_matrix("audit_new_b")
    oldV=st_matrix("audit_old_V")
    newV=st_matrix("audit_new_V")
    printf("maximum coefficient change: %g\n",max(abs(oldb-newb)))
    printf("maximum covariance change: %g\n",max(abs(oldV-newV)))
end
