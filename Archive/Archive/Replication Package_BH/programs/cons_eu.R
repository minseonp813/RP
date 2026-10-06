cons_eu = function (p, x, pi, e) {
        grid = expand.grid (sort (unique (c (0, x))), sort (unique (c (0, x))));
        A = matrix (0, dim (grid)[1] * dim (p)[2], length (unique (c (0, x))));
        A1 = matrix (0, dim (A)[1], dim (A)[2]);
        for (i in 1 : (dim (A)[2] * dim (p)[2])) {
                A1[((i - 1) * dim (A)[2] + 1) : (i * dim (A)[2]), 1 : dim (A)[2]] = diag (dim (A)[2]) * pi[1, 1];
        }
        A2 = matrix (0, dim (A)[1], dim (A)[2]);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (A)[2]) {
                        A2[((i - 1) * dim (grid)[1] + (j - 1) * dim (A)[2] + 1) : ((i - 1) * dim (grid)[1] + j * dim (A)[2]), j] = matrix (1, dim (A)[2], 1) * pi[2, 1];
                }
        }
        A3 = matrix (0, dim (A)[1], dim (A)[2]);
        for (i in 1 : dim (p)[2]) {
                A3[((i - 1) * dim (grid)[1] + 1) : (i * dim (grid)[1]), 1 : dim (A)[2]] = t (matrix (t (-((x[1, i] == sort (unique (c (0, x)))) * pi[1, 1] + (x[2, i] == sort (unique (c (0, x)))) * pi[2, 1])), dim (A)[2], dim (grid)[1]));
        }
        A = A1 + A2 + A3;
        b = matrix (0, dim (A)[1], 1);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid)[1]) {
                        b[(i - 1) * dim (grid)[1] + j, 1] = (sum (p[, i] * c (grid[j, 1], grid[j, 2])) < e * sum (p[, i] * x[, i])) * -1;
                }
        }
        cons = cbind (A, b);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid)[1]) {
                        cons[(i - 1) * dim (grid)[1] + j,] = (sum (p[, i] * c (grid[j, 1], grid[j, 2])) <= e * sum (p[, i] * x[, i])) * cons[(i - 1) * dim (grid)[1] + j,];
                }
        }
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid)[1]) {
                        cons[(i - 1) * dim (grid)[1] + j,] = (x[1, i] < grid[j, 1] | x[2, i] < grid[j, 2]) * cons[(i - 1) * dim (grid)[1] + j,]; 
                }
        }
        cons = cons[rowSums (cons != 0) != 0,];
        A4 = matrix (0, dim (A)[2] - 1, dim (A)[2]);
        A4[, 1 : (dim (A)[2] - 1)] = diag (dim (A)[2] - 1);
        A5 = matrix (0, dim (A)[2] - 1, dim (A)[2]);
        A5[, 2 : dim (A)[2]] = -diag (dim (A)[2] - 1);
        cons = rbind (cons, cbind (A4 + A5, matrix (-1, dim (A4)[1], 1)));
        result = cons;
        return (result);
}
