fgarp = function (p, x, pi, e) {
        y = rbind (x[2,], x[1,]);
        if (pi[1, 1] != pi[2, 1]) {
                if (pi[1, 1] < pi[2, 1]) {
                        y[, (x[1,] <= x[2,])] = x[, (x[1,] <= x[2,])];
                } else  {
                        y[, (x[2,] <= x[1,])] = x[, (x[2,] <= x[1,])];
                }
        }
        R0 = NaN * matrix (1, dim (p)[2], dim (p)[2]);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (p)[2]) {
                        R0[i, j] = (e * sum (p[, i] * x[, i]) >= sum (p[, i] * x[, j]) | e * sum (p[, i] * x[, i]) >= sum (p[, i] * y[, j]));
                }
        }
        R = warshall (R0);
        P0 = NaN * matrix (1, dim (p)[2], dim (p)[2]);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (p)[2]) {
                        if (pi[1, 1] == pi[2, 1]) {
                                P0[i, j] = (e * sum (p[, i] * x[, i]) > sum (p[, i] * x[, j]) | e * sum (p[, i] * x[, i]) > sum (p[, i] * y[, j]));
                        } else {
                                P0[i, j] = (e * sum (p[, i] * x[, i]) > sum (p[, i] * x[, j]) | (e * sum (p[, i] * x[, i]) >= sum (p[, i] * y[, j]) & sum (x[, j] == y[, j]) != dim (p)[1]));
                        }
                }
        }
        result = 1;
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (p)[2]) {
                        if (R[i, j] == 1 & P0[j, i] == 1) {
                                result = 0;
                                break;
                        }
                }
                if (result == 0) {
                        break;
                }
        }
        return (result);
}
