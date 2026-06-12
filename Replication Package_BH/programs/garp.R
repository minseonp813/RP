garp = function (p, x, e) {
        R0 = NaN * matrix (1, dim (p)[2], dim (p)[2]);
        P0 = NaN * matrix (1, dim (p)[2], dim (p)[2]);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (p)[2]) {
                        R0[i, j] = e * sum (p[, i] * x[, i]) >= sum (p[, i] * x[, j]);
                        P0[i, j] = e * sum (p[, i] * x[, i]) > sum (p[, i] * x[, j]);
                }
        }
        R = warshall (R0);
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
