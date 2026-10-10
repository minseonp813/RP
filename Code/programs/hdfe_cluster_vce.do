* Run after reghdfe with keepmata; pass the class-cluster variable.
* Form the clustered sandwich as a Gram matrix of cluster influences. This
* avoids cancellation in reghdfe's D*M*D when paired outcomes sum exactly to one.
include "reghdfe.mata", adopath
capture mata: mata drop hdfe_cluster_gram()
mata:
void hdfe_cluster_gram(real matrix data, real rowvector status,
    real rowvector stdevs, real rowvector means, real colvector residual,
    real colvector cluster, real scalar q, string scalar covariance_name)
{
    real scalar k, j
    real matrix X, bread, influence, V
    real rowvector sx, mx, score, positions, active, remaining
    real colvector levels, idx
    positions = selectindex(status[2..cols(status)]:==0)
    active = selectindex((status[2..cols(status)-1]:==0):|(status[2..cols(status)-1]:==3))
    remaining = selectindex(status[active:+1]:==0)
    k = cols(positions)-1
    X = data[,remaining:+1]
    sx = stdevs[2..k+1]
    mx = means[2..k+1]:*sx
    bread = invsym(quadcross(X,X))
    levels = uniqrows(sort(cluster,1))
    influence = J(rows(levels),k+1,0)
    for (j=1; j<=rows(levels); j++) {
        idx = selectindex(cluster:==levels[j])
        score = quadcross(residual[idx],X[idx,])
        influence[j,1..k] = (score*bread):/sx
        influence[j,k+1] = quadsum(residual[idx])/rows(data)-influence[j,1..k]*mx'
    }
    V = J(cols(st_matrix(covariance_name)),cols(st_matrix(covariance_name)),0)
    V[positions,positions] = q*quadcross(influence,influence)
    st_matrix(covariance_name,(V+V')/2)
}
end
capture program drop hdfe_cluster_vce
program define hdfe_cluster_vce, eclass
    syntax varname [, ZERO(string)]
    tempvar cluster_id
    quietly egen long `cluster_id' = group(`varlist')
    tempname cluster_V
    matrix `cluster_V' = e(V)
    mata: cluster_q = (HDFE.solution.N-1)/(HDFE.solution.N-HDFE.solution.df_m-HDFE.df_a-(HDFE.df_a_nested>0))*HDFE.solution.N_clust/(HDFE.solution.N_clust-1)
    mata: hdfe_cluster_gram(HDFE.solution.data, HDFE.solution.indepvar_status, HDFE.solution.stdevs, HDFE.solution.means, HDFE.solution.resid, st_data(HDFE.sample,st_local("cluster_id")), cluster_q, st_local("cluster_V"))
    if "`zero'" != "" {
        * Exact pair symmetry can imply zero coefficient and cluster variance.
        tempname cluster_b
        matrix `cluster_b' = e(b)
        local position = colnumb(`cluster_b', "`zero'")
        assert abs(_b[`zero']) < 1e-10 & sqrt(`cluster_V'[`position', `position']) < 1e-10
        matrix `cluster_b'[1, `position'] = 0
        forvalues term = 1/`=colsof(`cluster_b')' {
            matrix `cluster_V'[`term', `position'] = 0
            matrix `cluster_V'[`position', `term'] = 0
        }
        ereturn repost b=`cluster_b' V=`cluster_V'
    }
    else ereturn repost V=`cluster_V'
end
