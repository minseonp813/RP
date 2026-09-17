function output = fssr(pm,cearly,clate,mearly,mlate,treatment)

n = length(cearly);

beta = pm(1); 
delta = pm(2); 
A1 = pm(3);
A2 = pm(4);
out = 0;

for i=1:n
    
    dm = opdem(beta,delta,A1,A2,mearly(i),mlate(i),treatment(i));
    
    op_rd = dm(1)/(dm(1)+dm(2));
    
    rd = cearly(i)/(cearly(i) + clate(i));
    
    out = out + (rd - op_rd)^2;
end

output = out;

end


