ccei_eucrra = function (p, x, pi, rho) {
        estar = 1;
        if (rho == 1) {
                u = colSums (pi * log (x));
        } else {
                u = colSums (pi * x ^ (1 - rho) / (1 - rho));
        }
        if (rho == 1) {
                v = pi[1,] * log (estar * colSums (p * x) / (p[1,] + p[2,] * (p[1,] / pi[1,]) / (p[2,] / pi[2,]))) + pi[2,] * log (estar * colSums (p * x) / (p[2,] + p[1,] * (p[2,] / pi[2,]) / (p[1,] / pi[1,])));
        } else {
                v = pi[1,] * (estar * colSums (p * x) / (p[1,] + p[2,] * ((p[1,] / pi[1,]) / (p[2,] / pi[2,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho) + pi[2,] * (estar * colSums (p * x) / (p[2,] + p[1,] * ((p[2,] / pi[2,]) / (p[1,] / pi[1,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho);
        }
        if (sum (v > u) > 0) {
                eL = 0;
                eH = 1;
                while (eH - eL > 1e-6) {
                        e = (eL + eH) / 2;
                        if (rho == 1) {
                                v = pi[1,] * log (e * colSums (p * x) / (p[1,] + p[2,] * (p[1,] / pi[1,]) / (p[2,] / pi[2,]))) + pi[2,] * log (e * colSums (p * x) / (p[2,] + p[1,] * (p[2,] / pi[2,]) / (p[1,] / pi[1,])));
                        } else {
                                v = pi[1,] * (e * colSums (p * x) / (p[1,] + p[2,] * ((p[1,] / pi[1,]) / (p[2,] / pi[2,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho) + pi[2,] * (e * colSums (p * x) / (p[2,] + p[1,] * ((p[2,] / pi[2,]) / (p[1,] / pi[1,])) ^ (1 / rho))) ^ (1 - rho) / (1 - rho);
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
