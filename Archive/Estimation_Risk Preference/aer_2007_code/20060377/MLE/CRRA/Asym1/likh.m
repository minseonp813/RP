function output = likh(pm,y,x,ymax,xmax)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The contruction of likelihood function given the data (y,x,ymax,xmax)
% for the ML Estimation with CRRA specification. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logda = exp(pm(1)); crra = exp(pm(2)); sgm = exp(pm(3));

n = length(y); h = 0.00000000000000000001; w = 0.01;

loglik = 0;

for i = 1:n
    
% When constructing the likelihood function at the diagonal point, we use one-token neighborhood.    
    
    if (x(i) <= w*y(i))
        
        z = -crra*(log((1/w))) - logda + log(ymax(i)/xmax(i));
        lik = log(normcdf(z,0,sgm)+h);
        
    elseif ((x(i) > w*y(i))&(x(i) <= y(i) - 1))
        
        z = -crra*(log(y(i)/x(i))) - logda + log(ymax(i)/xmax(i));
        lik = log(normpdf(z,0,sgm)+h);
        
    elseif (y(i) <= w*x(i))
        
        z = -crra*(log(w)) + logda + log(ymax(i)/xmax(i));
        lik = log(1-normcdf(z,0,sgm)+h);
        
    elseif ((y(i) > w*x(i))&(y(i) < x(i) - 1))
        
        z = -crra*(log(y(i)/x(i))) + logda + log(ymax(i)/xmax(i));
        lik = log(normpdf(z,0,sgm)+h);
        
    elseif ((y(i)-1 <= x(i))&(x(i) <= y(i)+1))
        
        z1 = logda + log(ymax(i)/xmax(i)) - crra*(log(y(i)/x(i)));
        z2 = -logda + log(ymax(i)/xmax(i)) - crra*(log(y(i)/x(i)));
        lik = log(normcdf(z1,0,sgm)-normcdf(z2,0,sgm)+h);
        
    end
    
    loglik = loglik + lik;
    
end

% The output of this function is the negative loglikelihood given the data (y,x,ymax,xmax)

output = - loglik;







