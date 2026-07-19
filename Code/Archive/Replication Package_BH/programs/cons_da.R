cons_da = function (p, x, pi, gamma, e) {
        gammatilde = (gamma[1, 1] * (1 - pi[1, 1]) ^ 2) / (gamma[1, 1] - 2 * gamma[1, 1] * pi[1, 1] + pi[1, 1] ^ 2);
        gammatilde = matrix (c (gammatilde, 1 - gammatilde), 2, 1);
        grid = expand.grid (sort (unique (c (0, x))), sort (unique (c (0, x))));
        grid_below = grid[grid[, 1] >= grid[, 2],];
        A_below = matrix (0, dim (grid_below)[1] * dim (p)[2], length (unique (c (0, x))));
        A1_below = matrix (0, dim (A_below)[1], dim (A_below)[2]);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid_below)[1]) {
                        A1_below[(i - 1) * dim (grid_below)[1] + j,] = (sort (unique (c (0, x))) == grid_below[j, 1]) * gamma[1, 1];
                }
        }
        A2_below = matrix (0, dim (A_below)[1], dim (A_below)[2]);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid_below)[1]) {
                        A2_below[(i - 1) * dim (grid_below)[1] + j,] = (sort (unique (c (0, x))) == grid_below[j, 2]) * gamma[2, 1];
                }
        }
        A3_below = matrix (0, dim (A_below)[1], dim (A_below)[2]);
        for (i in 1 : dim (p)[2]) {
                if (x[1, i] >= x[2, i]) {
                        A3_below[((i - 1) * dim (grid_below)[1] + 1) : (i * dim (grid_below)[1]), 1 : dim (A3_below)[2]] = t (matrix (t (-((x[1, i] == sort (unique (c (0, x)))) * gamma[1, 1] + (x[2, i] == sort (unique (c (0, x)))) * gamma[2, 1])), dim (A3_below)[2], dim (grid_below)[1]));
                } else {
                        A3_below[((i - 1) * dim (grid_below)[1] + 1) : (i * dim (grid_below)[1]), 1 : dim (A3_below)[2]] = t (matrix (t (-((x[2, i] == sort (unique (c (0, x)))) * gammatilde[1, 1] + (x[1, i] == sort (unique (c (0, x)))) * gammatilde[2, 1])), dim (A3_below)[2], dim (grid_below)[1]));
                }
        }
        A_below = A1_below + A2_below + A3_below;
        b_below = matrix (0, dim (A_below)[1], 1);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid_below)[1]) {
                        b_below[(i - 1) * dim (grid_below)[1] + j, 1] = (sum (p[, i] * c (grid_below[j, 1], grid_below[j, 2])) < e * sum (p[, i] * x[, i])) * -1;
                }
        }
        cons_below = cbind (A_below, b_below);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid_below)[1]) {
                        cons_below[(i - 1) * dim (grid_below)[1] + j,] = (sum (p[, i] * c (grid_below[j, 1], grid_below[j, 2])) <= e * sum (p[, i] * x[, i])) * cons_below[(i - 1) * dim (grid_below)[1] + j,];
                }
        }
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid_below)[1]) {
                        cons_below[(i - 1) * dim (grid_below)[1] + j,] = (x[1, i] < grid_below[j, 1] | x[2, i] < grid_below[j, 2]) * cons_below[(i - 1) * dim (grid_below)[1] + j,];
                }
        }
        cons_below = cons_below[rowSums (cons_below != 0) != 0,];
        grid_above = grid[grid[, 1] < grid[, 2],];
        A_above = matrix (0, dim (grid_above)[1] * dim (p)[2], length (unique (c (0, x))));
        A1_above = matrix (0, dim (A_above)[1], dim (A_above)[2]);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid_above)[1]) {
                        A1_above[(i - 1) * dim (grid_above)[1] + j,] = (sort (unique (c (0, x))) == grid_above[j, 1]) * gammatilde[2, 1];
                }
        }
        A2_above = matrix (0, dim (A_above)[1], dim (A_above)[2]);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid_above)[1]) {
                        A2_above[(i - 1) * dim (grid_above)[1] + j,] = (sort (unique (c (0, x))) == grid_above[j, 2]) * gammatilde[1, 1];
                }
        }
        A3_above = matrix (0, dim (A_above)[1], dim (A_above)[2]);
        for (i in 1 : dim (p)[2]) {
                if (x[1, i] >= x[2, i]) {
                        A3_above[((i - 1) * dim (grid_above)[1] + 1) : (i * dim (grid_above)[1]), 1 : dim (A3_above)[2]] = t (matrix (t (-((x[1, i] == sort (unique (c (0, x)))) * gamma[1, 1] + (x[2, i] == sort (unique (c (0, x)))) * gamma[2, 1])), dim (A3_above)[2], dim (grid_above)[1]));
                } else {
                        A3_above[((i - 1) * dim (grid_above)[1] + 1) : (i * dim (grid_above)[1]), 1 : dim (A3_above)[2]] = t (matrix (t (-((x[2, i] == sort (unique (c (0, x)))) * gammatilde[1, 1] + (x[1, i] == sort (unique (c (0, x)))) * gammatilde[2, 1])), dim (A3_above)[2], dim (grid_above)[1]));
                }
        }
        A_above = A1_above + A2_above + A3_above;
        b_above = matrix (0, dim (A_above)[1], 1);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid_above)[1]) {
                        b_above[(i - 1) * dim (grid_above)[1] + j, 1] = (sum (p[, i] * c (grid_above[j, 1], grid_above[j, 2])) < e * sum (p[, i] * x[, i])) * -1;
                }
        }
        cons_above = cbind (A_above, b_above);
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid_above)[1]) {
                        cons_above[(i - 1) * dim (grid_above)[1] + j,] = (sum (p[, i] * c (grid_above[j, 1], grid_above[j, 2])) <= e * sum (p[, i] * x[, i])) * cons_above[(i - 1) * dim (grid_above)[1] + j,];
                }
        }
        for (i in 1 : dim (p)[2]) {
                for (j in 1 : dim (grid_above)[1]) {
                        cons_above[(i - 1) * dim (grid_above)[1] + j,] = (x[1, i] < grid_above[j, 1] | x[2, i] < grid_above[j, 2]) * cons_above[(i - 1) * dim (grid_above)[1] + j,]; 
                }
        }
        cons_above = cons_above[rowSums (cons_above != 0) != 0,];
        cons = rbind (cons_below, cons_above);
        A4 = matrix (0, dim (A_below)[2] - 1, dim (A_below)[2]);
        A4[, 1 : (dim (A_below)[2] - 1)] = diag (dim (A_below)[2] - 1);
        A5 = matrix (0, dim (A_below)[2] - 1, dim (A_below)[2]);
        A5[, 2 : dim (A_below)[2]] = -diag (dim (A_below)[2] - 1);
        cons = rbind (cons, cbind (A4 + A5, matrix (-1, dim (A4)[1], 1)));
        result = cons;
        return (result);
}
