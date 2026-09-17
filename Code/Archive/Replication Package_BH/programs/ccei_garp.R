ccei_garp = function (p, x) {
        estar = 1;
        if (garp (p, x, estar) == 0) {
                eL = 0;
                eH = 1;
                while (eH - eL > 1e-6) {
                        e = (eL + eH) / 2;
                        if (garp (p, x, e) == 1) {
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
