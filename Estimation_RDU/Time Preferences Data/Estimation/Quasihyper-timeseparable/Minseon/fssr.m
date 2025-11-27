function output = fssr(pm,cearly,clate,mearly,mlate,treatment)

beta = pm(1); 
delta = pm(2); 
A1 = pm(3);
A2 = pm(4);

dm = opdem(beta,delta,A1,A2,mearly,mlate,treatment);
    
op_rd = dm(1)./(dm(1)+dm(2));
rd = cearly./(cearly + clate);
out = (rd - op_rd).^2;
output = sum(out);

end


