ccei_rdu = function (p, x, gamma, theta) {
        estar = 1;
        if (rdu (p, x, gamma, theta, estar) == 0) {
                eL = 0;
                eH = 1;
                while (eH - eL > 1e-6) {
                        e = (eL + eH) / 2;
                        if (rdu (p, x, gamma, theta, e) == 1) {
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
