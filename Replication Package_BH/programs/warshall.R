warshall = function (R0) {
        R = R0;
        for (k in 1 : dim (R)[1]) {
                for (i in (1 : dim (R)[1])[-k]) { 
                        if (R[i, k] == 1) {
                                for (j in (1 : dim (R)[1])[-k]) {
                                        if (R[i, j] == 0) {
                                                R[i, j] = R[k, j];
                                        }
                                }
                        }
                }
        }
        result = R;
        return (result);
}
