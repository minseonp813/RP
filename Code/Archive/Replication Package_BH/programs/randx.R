randx = function (p, x) {
        w = matrix (runif (dim (p)[2], 0, 1), 1, dim (p)[2]);
        w = rbind (w, 1 - w);
        x0 = w * t (matrix (colSums (p * x), dim (p)[2], dim (p)[1])) / p;
        result = x0;
        return (result);
}
