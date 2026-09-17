ccei_ceu = function (p, x, pi) {
        estar = 1;
        if (ceu (p, x, pi, estar) == 0) {
                eL = 0;
                eH = 1;
                while (eH - eL > 1e-6) {
                        e = (eL + eH) / 2;
                        if (ceu (p, x, pi, e) == 1) {
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
