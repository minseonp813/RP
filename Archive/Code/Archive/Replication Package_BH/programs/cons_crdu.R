cons_crdu = function (p, x, gamma, theta, e) {
        theta = matrix (theta[2 : 1], 2, 1);
        n.states = dim (p)[1];
        n.obs = dim (p)[2];
        x.max = max (ceiling (matrix (apply (p * x, 2, sum), n.states, n.obs, byrow = TRUE) / p)) + 1;
        x.unique = sort (unique (c (0, x, x.max)));
        n.x.unique = length (x.unique);
        grid = as.matrix (expand.grid (rep (list (x.unique), n.states - 1), KEEP.OUT.ATTRS = FALSE));
        n.grid.rows = dim (grid)[1];
        grid.intersect = NaN * matrix (1, n.states * n.grid.rows, n.states);
        for (i in 1 : n.states) {
                grid.intersect[((i - 1) * n.grid.rows + 1) : (i * n.grid.rows), -i] = grid;
        }
        A.1 = NaN * matrix (1, n.states * n.obs * n.grid.rows, n.x.unique);
        for (i in 1 : n.obs) {
                p.i = p[, i];
                x.i = x[, i];
                p.x.i = sum (p.i * x.i);
                e.p.x.i = e * p.x.i;
                grid.intersect.i = grid.intersect;
                p.grid.intersect.i = matrix (p.i, n.states * n.grid.rows, n.states, byrow = TRUE) * grid.intersect.i;
                p.grid.intersect.i = apply (p.grid.intersect.i, 1, sum, na.rm = TRUE);
                remainder.i = e.p.x.i - p.grid.intersect.i;
                remainder.i = remainder.i / c (matrix (p.i, n.grid.rows, n.states, byrow = TRUE));
                grid.intersect.i[is.nan(grid.intersect.i)] = remainder.i;
                keep.TF.i = apply (grid.intersect.i >= 0, 1, sum) == n.states;
                n.keep.i = sum (keep.TF.i);
                grid.intersect.i = grid.intersect.i[keep.TF.i,];
                lb.i = apply (grid.intersect.i, c (1, 2), function (z) {x.unique[max (which (x.unique <= z))]});
                ub.i = apply (grid.intersect.i, c (1, 2), function (z) {x.unique[min (which (x.unique >= z))]});
                lambda.i = (ub.i - grid.intersect.i) / (ub.i - lb.i);
                lambda.i[is.nan (lambda.i)] = 0;
                map.lb.i = apply (lb.i, c (1, 2), function (z) {which (x.unique %in% z)});
                map.ub.i = apply (ub.i, c (1, 2), function (z) {which (x.unique %in% z)});
                A.1.1.i = matrix (0, n.keep.i, n.x.unique);
                for (j in 1 : n.keep.i) {
                        if (grid.intersect.i[j, 1] >= grid.intersect.i[j, 2]) {
                                for (k in 1 : n.states) {
                                        A.1.1.i[j, map.lb.i[j, k]] = A.1.1.i[j, map.lb.i[j, k]] + lambda.i[j, k] * gamma[k, 1];
                                        A.1.1.i[j, map.ub.i[j, k]] = A.1.1.i[j, map.ub.i[j, k]] + (1 - lambda.i[j, k]) * gamma[k, 1];
                                }
                        } else {
                                for (k in 1 : n.states) {
                                        A.1.1.i[j, map.lb.i[j, k]] = A.1.1.i[j, map.lb.i[j, k]] + lambda.i[j, k] * theta[k, 1];
                                        A.1.1.i[j, map.ub.i[j, k]] = A.1.1.i[j, map.ub.i[j, k]] + (1 - lambda.i[j, k]) * theta[k, 1];
                                }
                        }
                }
                A.1.2.i = rep (0, n.x.unique);
                if (x.i[1] >= x.i[2]) {
                        for (j in 1 : n.states) {
                                A.1.2.i[which (x.unique %in% x.i[j])] = A.1.2.i[which (x.unique %in% x.i[j])] + gamma[j, 1];
                        }
                } else {
                        for (j in 1 : n.states) {
                                A.1.2.i[which (x.unique %in% x.i[j])] = A.1.2.i[which (x.unique %in% x.i[j])] + theta[j, 1];
                        }
                }
                A.1.2.i = matrix (A.1.2.i, n.keep.i, n.x.unique, byrow = TRUE);
                A.1.i = NaN * matrix (1, n.states * n.grid.rows, n.x.unique);
                A.1.i[keep.TF.i,] = A.1.1.i - A.1.2.i;
                A.1[((i - 1) * n.states * n.grid.rows + 1) : (i * n.states * n.grid.rows),] = A.1.i;
        }
        A.1 = A.1[apply (is.nan (A.1), 1, sum) == 0,];
        n.A.1.rows = dim (A.1)[1];
        b.1 = rep (0, n.A.1.rows);
        A.45 = NaN * matrix (1, n.obs, n.x.unique);
        for (i in 1 : n.obs) {
                p.i = p[, i];
                x.i = x[, i];
                p.x.i = sum (p.i * x.i);
                e.p.x.i = e * p.x.i;
                x.45.i = e.p.x.i / sum (p.i);
                x.45.lb.i = c (apply (matrix (x.45.i), c (1, 2), function (z) {x.unique[max (which (x.unique <= z))]}));
                x.45.ub.i = c (apply (matrix (x.45.i), c (1, 2), function (z) {x.unique[min (which (x.unique >= z))]}));
                x.45.lambda.i = (x.45.ub.i - x.45.i) / (x.45.ub.i - x.45.lb.i);
                x.45.lambda.i[is.nan (x.45.lambda.i)] = 0;
                map.x.45.lb.i = c (apply (matrix (x.45.lb.i), c (1, 2), function (z) {which (x.unique %in% z)}));
                map.x.45.ub.i = c (apply (matrix (x.45.ub.i), c (1, 2), function (z) {which (x.unique %in% z)}));
                A.45.i = rep (0, n.x.unique);
                if (x.i[1] >= x.i[2]) {
                        for (j in 1 : n.states) {
                                A.45.i[which (x.unique %in% x.i[j])] = A.45.i[which (x.unique %in% x.i[j])] + gamma[j, 1];
                        }
                } else {
                        for (j in 1 : n.states) {
                                A.45.i[which (x.unique %in% x.i[j])] = A.45.i[which (x.unique %in% x.i[j])] + theta[j, 1];
                        }
                }
                A.45.i = -A.45.i;
                A.45.i[map.x.45.lb.i] = A.45.i[map.x.45.lb.i] + x.45.lambda.i;
                A.45.i[map.x.45.ub.i] = A.45.i[map.x.45.ub.i] + (1 - x.45.lambda.i);
                A.45[i,] = A.45.i;
        }
        b.45 = rep (0, n.obs);
        diag.x.unique = diag (n.x.unique - 2);
        rhs.du = cbind (0, diag.x.unique, 0) + cbind (-diag.x.unique, 0, 0);
        rhs.dx = x.unique[-n.x.unique];
        rhs.dx = rhs.dx[-1] - rhs.dx[-length(rhs.dx)];
        rhs = rhs.du / rhs.dx;
        lhs.du = cbind (0, 0, diag.x.unique) + cbind (0, -diag.x.unique, 0);
        lhs.dx = x.unique[-1];
        lhs.dx = lhs.dx[-1] - lhs.dx[-length(lhs.dx)];
        lhs = lhs.du / lhs.dx;
        A.2 = lhs - rhs;
        n.A.2.rows = dim (A.2)[1];
        b.2 = rep (0, n.A.2.rows);
        diag.x.unique = diag (n.x.unique - 1);
        A.3.1 = matrix (0, n.x.unique - 1, n.x.unique);
        A.3.1[, -n.x.unique] = diag.x.unique;
        A.3.2 = matrix (0, n.x.unique - 1, n.x.unique);
        A.3.2[, -1] = -diag.x.unique;
        A.3 = A.3.1 + A.3.2;
        n.A.3.rows = dim (A.3)[1];
        b.3 = rep (-1, n.A.3.rows);
        A = rbind (A.1, A.2, A.3, A.45);
        b = c (b.1, b.2, b.3, b.45);
        cons = cbind (A, b);
        result = cons;
        return (result);
}
