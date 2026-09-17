ccei_dacrra_pr = function (L1, L2, gamma, rho) {
        if (rho == 1) {
                u = NaN;
                v = NaN;
                u1 = gamma[, L1[1,] >= L1[2,]] * log (L1[, L1[1,] >= L1[2,]]);
                v1 = gamma[, L2[1,] >= L2[2,]] * log (L2[, L2[1,] >= L2[2,]]);
                if (is.null (dim (u1)[1]) == TRUE) {
                        u1 = sum (u1);
                } else {
                        u1 = colSums (u1);
                }
                if (is.null (dim (v1)[1]) == TRUE) {
                        v1 = sum (v1);
                } else {
                        v1 = colSums (v1);
                }
                u2 = gamma[2 : 1, L1[1,] < L1[2,]] * log (L1[, L1[1,] < L1[2,]]);
                v2 = gamma[2 : 1, L2[1,] < L2[2,]] * log (L2[, L2[1,] < L2[2,]]);
                if (is.null (dim (u2)[1]) == TRUE) {
                        u2 = sum (u2);
                } else {
                        u2 = colSums (u2);
                }
                if (is.null (dim (v2)[1]) == TRUE) {
                        v2 = sum (v2);
                } else {
                        v2 = colSums (v2);
                }
                u[L1[1,] >= L1[2,]] = u1;
                u[L1[1,] < L1[2,]] = u2;
                v[L2[1,] >= L2[2,]] = v1;
                v[L2[1,] < L2[2,]] = v2;
        } else {
                u = NaN;
                v = NaN;
                u1 = gamma[, L1[1,] >= L1[2,]] * L1[, L1[1,] >= L1[2,]] ^ (1 - rho) / (1 - rho);
                v1 = gamma[, L2[1,] >= L2[2,]] * L2[, L2[1,] >= L2[2,]] ^ (1 - rho) / (1 - rho);
                if (is.null (dim (u1)[1]) == TRUE) {
                        u1 = sum (u1);
                } else {
                        u1 = colSums (u1);
                }
                if (is.null (dim (v1)[1]) == TRUE) {
                        v1 = sum (v1);
                } else {
                        v1 = colSums (v1);
                }
                u2 = gamma[2 : 1, L1[1,] < L1[2,]] * L1[, L1[1,] < L1[2,]] ^ (1 - rho) / (1 - rho);
                v2 = gamma[2 : 1, L2[1,] < L2[2,]] * L2[, L2[1,] < L2[2,]] ^ (1 - rho) / (1 - rho);
                if (is.null (dim (u2)[1]) == TRUE) {
                        u2 = sum (u2);
                } else {
                        u2 = colSums (u2);
                }
                if (is.null (dim (v2)[1]) == TRUE) {
                        v2 = sum (v2);
                } else {
                        v2 = colSums (v2);
                }
                u[L1[1,] >= L1[2,]] = u1;
                u[L1[1,] < L1[2,]] = u2;
                v[L2[1,] >= L2[2,]] = v1;
                v[L2[1,] < L2[2,]] = v2;
        }
        result = (u > v);
        return (result);
}
