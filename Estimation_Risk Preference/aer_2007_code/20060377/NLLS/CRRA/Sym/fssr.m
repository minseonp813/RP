function output = fssr(pm,ldr,lpr,w)

% Compute the sum of squared residuals with
% ldr : log(x/y) and lpr : log(Ymax/Xmax)
% w : a constant to make the range of demand ratios compact bounded away from 0 and inf

n = length(ldr);

da = exp(pm(1)); % log(alpha)

rho = exp(pm(2));

bd = log(w); 

h = 0.00000000000000001;

if (rho < h)

    rho = h;
end

out = 0;

for i = 1:n
   
   if (da - rho*bd <= lpr(i))
       
       fv = bd;    
       
   elseif ((da - rho*bd > lpr(i))&(da < lpr(i)))
       
       fv = da/rho - (1/rho)*lpr(i);
       
   elseif ((lpr(i) <= da)&(-da <= lpr(i)))
       
       fv = 0;
    
   elseif ((lpr(i) < -da)&(-da + rho*bd < lpr(i)))
       
       fv = - da/rho - (1/rho)*lpr(i);

   elseif (lpr(i) <= -da + rho*bd)
       
       fv = - bd;
       
   end
   
   out = out + (ldr(i) - fv)^2;
end

output = out;



