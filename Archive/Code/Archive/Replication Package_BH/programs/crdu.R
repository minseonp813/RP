crdu = function (p, x, gamma, theta, e) {
        cons = cons_crdu (p, x, gamma, theta, e);
        A = cons[, 1 : dim (cons)[2] - 1];
        b = cons[, dim (cons)[2]];
        lp = make.lp (dim (A)[1], dim (A)[2]);
        for (i in 1 : dim (A)[2]) {
                set.column (lp, i, A[, i]);
        }
        set.constr.type (lp, rep ("<=", dim (A)[1]));
        set.bounds (lp, lower = rep (1, dim (A)[2]));
        set.rhs (lp, b);
        exitflag = solve (lp);
        if (exitflag == 0) {
                result = 1;
        } else {
                result = 0;
        }
        return (result);
}
