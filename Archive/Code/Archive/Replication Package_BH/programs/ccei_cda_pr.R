ccei_cda_pr = function (p, x, L1, L2, pi, gamma) {
        estar = 1;
        if (cda_pr (p, x, L1, L2, pi, gamma, estar) == 0) {
                eL = 0;
                eH = 1;
                while (eH - eL > 1e-6) {
                        e = (eL + eH) / 2;
                        if (cda_pr (p, x, L1, L2, pi, gamma, e) == 1) {
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
