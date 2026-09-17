cons_ceu = function (p, x, pi, e) {
        n.states = dim (p)[1];
        n.obs = dim (p)[2];
        max.x = max (matrix (apply (p * x, 2, sum), n.states, n.obs, byrow = TRUE) / p);
        unique.x = sort (unique (c (0, x, max.x)));
        n.unique.x = length (unique.x);
        grid = as.matrix (expand.grid (rep (list (unique.x), n.states - 1), KEEP.OUT.ATTRS = FALSE));
        n.grid.rows = dim (grid)[1];
        grid.intersect = NaN * matrix (1, n.states * n.grid.rows, n.states);
        for (i in 1 : n.states) {
                grid.intersect[((i - 1) * n.grid.rows + 1) : (i * n.grid.rows), -i] = grid;
        }
        A.1 = NaN * matrix (1, n.states * n.obs * n.grid.rows, n.unique.x);
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
                lb.i = apply (grid.intersect.i, c (1, 2), function (z) {unique.x[max (which (unique.x <= z))]});
                ub.i = apply (grid.intersect.i, c (1, 2), function (z) {unique.x[min (which (unique.x >= z))]});
                lambda.i = (ub.i - grid.intersect.i) / (ub.i - lb.i);
                lambda.i[is.nan (lambda.i)] = 0;
                map.lb.i = apply (lb.i, c (1, 2), function (z) {which (unique.x %in% z)});
                map.ub.i = apply (ub.i, c (1, 2), function (z) {which (unique.x %in% z)});
                A.1.1.i = matrix (0, n.keep.i, n.unique.x);
                for (j in 1 : n.keep.i) {
                        for (k in 1 : n.states) {
                                A.1.1.i[j, map.lb.i[j, k]] = A.1.1.i[j, map.lb.i[j, k]] + lambda.i[j, k] * pi[k, 1];
                                A.1.1.i[j, map.ub.i[j, k]] = A.1.1.i[j, map.ub.i[j, k]] + (1 - lambda.i[j, k]) * pi[k, 1];
                        }
                }
                A.1.2.i = rep (0, n.unique.x);
                for (j in 1 : n.states) {
                        A.1.2.i[which (unique.x %in% x.i[j])] = A.1.2.i[which (unique.x %in% x.i[j])] + pi[j, 1];
                }
                A.1.2.i = matrix (A.1.2.i, n.keep.i, n.unique.x, byrow = TRUE);
                A.1.i = NaN * matrix (1, n.states * n.grid.rows, n.unique.x);
                A.1.i[keep.TF.i,] = A.1.1.i - A.1.2.i;
                A.1[((i - 1) * n.states * n.grid.rows + 1) : (i * n.states * n.grid.rows),] = A.1.i;
        }
        A.1 = A.1[apply (is.nan (A.1), 1, sum) == 0,];
        n.A.1.rows = dim (A.1)[1];
        b.1 = rep (0, n.A.1.rows);
        diag.unique.x = diag (n.unique.x - 2);
        rhs.du = cbind (0, diag.unique.x, 0) + cbind (-diag.unique.x, 0, 0);
        rhs.dx = unique.x[-n.unique.x];
        rhs.dx = rhs.dx[-1] - rhs.dx[-length(rhs.dx)];
        rhs = rhs.du / rhs.dx;
        lhs.du = cbind (0, 0, diag.unique.x) + cbind (0, -diag.unique.x, 0);
        lhs.dx = unique.x[-1];
        lhs.dx = lhs.dx[-1] - lhs.dx[-length(lhs.dx)];
        lhs = lhs.du / lhs.dx;
        A.2 = lhs - rhs;
        n.A.2.rows = dim (A.2)[1];
        b.2 = rep (0, n.A.2.rows);
        diag.unique.x = diag (n.unique.x - 1);
        A.3.1 = matrix (0, n.unique.x - 1, n.unique.x);
        A.3.1[, -n.unique.x] = diag.unique.x;
        A.3.2 = matrix (0, n.unique.x - 1, n.unique.x);
        A.3.2[, -1] = -diag.unique.x;
        A.3 = A.3.1 + A.3.2;
        n.A.3.rows = dim (A.3)[1];
        b.3 = rep (-1, n.A.3.rows);
        A = rbind (A.1, A.2, A.3);
        b = c (b.1, b.2, b.3);
        cons = cbind (A, b);
        result = cons;
        return (result);
}
