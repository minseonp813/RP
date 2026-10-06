ccei_dacrra = function (p, x, gamma, rho) {
        estar = 1;
        if (rho == 1) {
                u = rep (NaN, dim (p)[2]);
                u1 = gamma[, x[1,] >= x[2,]] * log (x[, x[1,] >= x[2,]]);
                if (is.null (dim (u1)[1]) == TRUE) {
                        u1 = sum (u1);
                } else {
                        u1 = colSums (u1);
                }
                u2 = gamma[2 : 1, x[1,] < x[2,]] * log (x[, x[1,] < x[2,]]);
                if (is.null (dim (u2)[1]) == TRUE) {
                        u2 = sum (u2);
                } else {
                        u2 = colSums (u2);
                }
                u[x[1,] >= x[2,]] = u1;
                u[x[1,] < x[2,]] = u2;
        } else {
                u = rep (NaN, dim (p)[2]);
                u1 = gamma[, x[1,] >= x[2,]] * x[, x[1,] >= x[2,]] ^ (1 - rho) / (1 - rho);
                if (is.null (dim (u1)[1]) == TRUE) {
                        u1 = sum (u1);
                } else {
                        u1 = colSums (u1);
                }
                u2 = gamma[2 : 1, x[1,] < x[2,]] * x[, x[1,] < x[2,]] ^ (1 - rho) / (1 - rho);
                if (is.null (dim (u2)[1]) == TRUE) {
                        u2 = sum (u2);
                } else {
                        u2 = colSums (u2);
                }
                u[x[1,] >= x[2,]] = u1;
                u[x[1,] < x[2,]] = u2;
        }
        if (rho == 1) {
                if (gamma[1, 1] >= gamma[2, 1]) {
                        v1 = gamma[1,] * log (estar * colSums (p * x) / (p[1,] + p[2,] * (p[1,] / gamma[1,]) / (p[2,] / gamma[2,]))) + gamma[2,] * log (estar * colSums (p * x) / (p[2,] + p[1,] * (p[2,] / gamma[2,]) / (p[1,] / gamma[1,])));
                        v2 = gamma[2,] * log (estar * colSums (p * x) / (p[1,] + p[2,] * (p[1,] / gamma[2,]) / (p[2,] / gamma[1,]))) + gamma[1,] * log (estar * colSums (p * x) / (p[2,] + p[1,] * (p[2,] / gamma[1,]) / (p[1,] / gamma[2,])));
                        v = apply (rbind (v1, v2), 2, max);
                } else {
                        v1 = gamma[1,] * log (estar * colSums (p * x) / (p[1,] + p[2,] * (p[1,] / gamma[1,]) / (p[2,] / gamma[2,]))) + gamma[2,] * log (estar * colSums (p * x) / (p[2,] + p[1,] * (p[2,] / gamma[2,]) / (p[1,] / gamma[1,])));
                        v2 = gamma[2,] * log (estar * colSums (p * x) / (p[1,] + p[2,] * (p[1,] / gamma[2,]) / (p[2,] / gamma[1,]))) + gamma[1,] * log (estar * colSums (p * x) / (p[2,] + p[1,] * (p[2,] / gamma[1,]) / (p[1,] / gamma[2,])));
                        v3 = log (estar * colSums (p * x) / colSums (p));
                        v = rep (NaN, dim (p)[2]);
                        v[p[1,] / p[2,] < gamma[1,] / gamma[2,]] = v1[p[1,] / p[2,] < gamma[1,] / gamma[2,]];
                        v[p[1,] / p[2,] > gamma[2,] / gamma[1,]] = v2[p[1,] / p[2,] > gamma[2,] / gamma[1,]];
                        v[is.nan (v)] = v3[is.nan (v)];
                }
        } else {
                if (gamma[1, 1] >= gamma[2, 1]) {
                        v1 = gamma[1,] * (estar * colSums (p * x) / (p[1,] + p[2,] * ((p[1,] / gamma[1,]) / (p[2,] / gamma[2,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho) + gamma[2,] * (estar * colSums (p * x) / (p[2,] + p[1,] * ((p[2,] / gamma[2,]) / (p[1,] / gamma[1,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho);
                        v2 = gamma[2,] * (estar * colSums (p * x) / (p[1,] + p[2,] * ((p[1,] / gamma[2,]) / (p[2,] / gamma[1,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho) + gamma[1,] * (estar * colSums (p * x) / (p[2,] + p[1,] * ((p[2,] / gamma[1,]) / (p[1,] / gamma[2,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho);
                        v = apply (rbind (v1, v2), 2, max);
                } else {
                        v1 = gamma[1,] * (estar * colSums (p * x) / (p[1,] + p[2,] * ((p[1,] / gamma[1,]) / (p[2,] / gamma[2,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho) + gamma[2,] * (estar * colSums (p * x) / (p[2,] + p[1,] * ((p[2,] / gamma[2,]) / (p[1,] / gamma[1,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho);
                        v2 = gamma[2,] * (estar * colSums (p * x) / (p[1,] + p[2,] * ((p[1,] / gamma[2,]) / (p[2,] / gamma[1,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho) + gamma[1,] * (estar * colSums (p * x) / (p[2,] + p[1,] * ((p[2,] / gamma[1,]) / (p[1,] / gamma[2,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho);
                        v3 = (estar * colSums (p * x) / colSums (p)) ^ (1 - rho) / (1 - rho);
                        v = rep (NaN, dim (p)[2]);
                        v[p[1,] / p[2,] < gamma[1,] / gamma[2,]] = v1[p[1,] / p[2,] < gamma[1,] / gamma[2,]];
                        v[p[1,] / p[2,] > gamma[2,] / gamma[1,]] = v2[p[1,] / p[2,] > gamma[2,] / gamma[1,]];
                        v[is.nan (v)] = v3[is.nan (v)];
                }
        }
        if (sum (v > u) > 0) {
                eL = 0;
                eH = 1;
                while (eH - eL > 1e-6) {
                        e = (eL + eH) / 2;
                        if (rho == 1) {
                                if (gamma[1, 1] >= gamma[2, 1]) {
                                        v1 = gamma[1,] * log (e * colSums (p * x) / (p[1,] + p[2,] * (p[1,] / gamma[1,]) / (p[2,] / gamma[2,]))) + gamma[2,] * log (e * colSums (p * x) / (p[2,] + p[1,] * (p[2,] / gamma[2,]) / (p[1,] / gamma[1,])));
                                        v2 = gamma[2,] * log (e * colSums (p * x) / (p[1,] + p[2,] * (p[1,] / gamma[2,]) / (p[2,] / gamma[1,]))) + gamma[1,] * log (e * colSums (p * x) / (p[2,] + p[1,] * (p[2,] / gamma[1,]) / (p[1,] / gamma[2,])));
                                        v = apply (rbind (v1, v2), 2, max);
                                } else {
                                        v1 = gamma[1,] * log (e * colSums (p * x) / (p[1,] + p[2,] * (p[1,] / gamma[1,]) / (p[2,] / gamma[2,]))) + gamma[2,] * log (e * colSums (p * x) / (p[2,] + p[1,] * (p[2,] / gamma[2,]) / (p[1,] / gamma[1,])));
                                        v2 = gamma[2,] * log (e * colSums (p * x) / (p[1,] + p[2,] * (p[1,] / gamma[2,]) / (p[2,] / gamma[1,]))) + gamma[1,] * log (e * colSums (p * x) / (p[2,] + p[1,] * (p[2,] / gamma[1,]) / (p[1,] / gamma[2,])));
                                        v3 = log (e * colSums (p * x) / colSums (p));
                                        v = rep (NaN, dim (p)[2]);
                                        v[p[1,] / p[2,] < gamma[1,] / gamma[2,]] = v1[p[1,] / p[2,] < gamma[1,] / gamma[2,]];
                                        v[p[1,] / p[2,] > gamma[2,] / gamma[1,]] = v2[p[1,] / p[2,] > gamma[2,] / gamma[1,]];
                                        v[is.nan (v)] = v3[is.nan (v)];
                                }
                        } else {
                                if (gamma[1, 1] >= gamma[2, 1]) {
                                        v1 = gamma[1,] * (e * colSums (p * x) / (p[1,] + p[2,] * ((p[1,] / gamma[1,]) / (p[2,] / gamma[2,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho) + gamma[2,] * (e * colSums (p * x) / (p[2,] + p[1,] * ((p[2,] / gamma[2,]) / (p[1,] / gamma[1,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho);
                                        v2 = gamma[2,] * (e * colSums (p * x) / (p[1,] + p[2,] * ((p[1,] / gamma[2,]) / (p[2,] / gamma[1,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho) + gamma[1,] * (e * colSums (p * x) / (p[2,] + p[1,] * ((p[2,] / gamma[1,]) / (p[1,] / gamma[2,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho);
                                        v = apply (rbind (v1, v2), 2, max);
                                } else {
                                        v1 = gamma[1,] * (e * colSums (p * x) / (p[1,] + p[2,] * ((p[1,] / gamma[1,]) / (p[2,] / gamma[2,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho) + gamma[2,] * (e * colSums (p * x) / (p[2,] + p[1,] * ((p[2,] / gamma[2,]) / (p[1,] / gamma[1,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho);
                                        v2 = gamma[2,] * (e * colSums (p * x) / (p[1,] + p[2,] * ((p[1,] / gamma[2,]) / (p[2,] / gamma[1,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho) + gamma[1,] * (e * colSums (p * x) / (p[2,] + p[1,] * ((p[2,] / gamma[1,]) / (p[1,] / gamma[2,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho);
                                        v3 = (e * colSums (p * x) / colSums (p)) ^ (1 - rho) / (1 - rho);
                                        v = rep (NaN, dim (p)[2]);
                                        v[p[1,] / p[2,] < gamma[1,] / gamma[2,]] = v1[p[1,] / p[2,] < gamma[1,] / gamma[2,]];
                                        v[p[1,] / p[2,] > gamma[2,] / gamma[1,]] = v2[p[1,] / p[2,] > gamma[2,] / gamma[1,]];
                                        v[is.nan (v)] = v3[is.nan (v)];
                                }
                        }
                        if (sum (v > u) == 0) {
                                estar = e;
                                eL = e;
                        } else {
                                estar = eL;
                                eH = e;
                        }
                }
        }
        result = estar;
        return (result);
}
