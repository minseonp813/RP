function output = likh(pm,y,x,ymax,xmax)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The contruction of likelihood function given the data (y,x,ymax,xmax)
% for the ML estimation with CARA specification. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logda = exp(pm(1)); cara = exp(pm(2)); sgm = exp(pm(3));

n = length(y); h = 0.00000000000000000001;

loglik = 0;

for i = 1:n
    
    if (x(i) <= 1)
        
        z = -cara*(ymax(i)) - logda + log(ymax(i)/xmax(i));
        lik = log(normcdf(z,0,sgm)+h);
        
    elseif ((x(i) >1)&(x(i) <= y(i) - 1))
        
        z = -cara*(y(i)-x(i)) - logda + log(ymax(i)/xmax(i));
        lik = log(normpdf(z,0,sgm)+h);
        
    elseif (y(i) <= 1)
        
        z = cara*(xmax(i)) + logda + log(ymax(i)/xmax(i));
        lik = log(1-normcdf(z,0,sgm)+h);
        
    elseif ((y(i) >1)&(y(i) < x(i) - 1))
        
        z = -cara*(y(i)-x(i)) + logda + log(ymax(i)/xmax(i));
        lik = log(normpdf(z,0,sgm)+h);
        
    elseif ((y(i)-1 <= x(i))&(x(i) <= y(i)+1))
        
        z1 = logda + log(ymax(i)/xmax(i)) - cara*(y(i)-x(i));
        z2 = - logda + log(ymax(i)/xmax(i)) - cara*(y(i)-x(i));
        lik = log(normcdf(z1,0,sgm)-normcdf(z2,0,sgm)+h);
        
    end
    
    loglik = loglik + lik;
    
end

% The output of this function is the negative loglikelihood given the data
% (y,x,ymax,xmax)

output = - loglik;







