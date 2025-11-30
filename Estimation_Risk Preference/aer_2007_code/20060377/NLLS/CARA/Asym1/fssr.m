function output = fssr(pm,y,x,ymax,xmax)

% Compute the sum of squared residuals with (y,x,ymax,xmax)
% log(alpha) = exp(pm(1)), rho = exp(pm(2))

n = length(y);
da = exp(pm(1)); % log(alpha)
rho = exp(pm(2));

lpr = log(ymax./xmax);

h = 0.00000000000000001;

if (rho < h)

    rho = h;
end

out = 0;

for i = 1:n
   
   if (da + rho*ymax(i) <= lpr(i))
       
       fv = ymax(i);    
       
   elseif ((da + rho*ymax(i) > lpr(i))&(da < lpr(i)))
       
       fv = -da/rho + (1/rho)*lpr(i);
       
   elseif ((lpr(i) <= da)&(-da <= lpr(i)))
       
       fv = 0;
    
   elseif ((lpr(i) < -da)&(-da - rho*xmax(i) < lpr(i)))
       
       fv = da/rho + (1/rho)*lpr(i);

   elseif (lpr(i) <= -da - rho*xmax(i))
       
       fv = - xmax(i);
       
   end
   
   out = out + (y(i) - x(i) - fv)^2;
end

output = out;



