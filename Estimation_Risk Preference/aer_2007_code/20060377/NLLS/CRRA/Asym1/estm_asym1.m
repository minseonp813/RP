%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Nonlinear Least Squares (NLLS) Estimation in Choi, Fisman, Gale and Kariv, 
% "Consistency and Heterogeneity of Individual Behavior under Uncertainty", 
% American Economic Review, December 2007.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NLLS Estimation with CRRA specification using the data from the asymmetric treatment
% where the probability of state 1 being selected is 1/3.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clc

% Load the data file
% data_sym has six column vectors. The first column is about subject ID and
% the second is about the number of decision rounds.
% The third to the six column contain the decisions and the parameter in the budget set,
% (y, x, ymax, xmax). 

load data_asym1; dt = data_asym1; 

n = length(dt)/50; % n is the number of subjects in this asymmetric treatment

w = 0.001; % Set the minimum demand ratio parameter in the specification

results = [];

for i = 1:n
    m1 = 50*(i-1)+1; m2 = 50*i; id = dt(m1,1)

% Call the demands in x and y accounts and the x and y intercepts in the budget sets
    
    y = dt(m1:m2,3); x = dt(m1:m2,4); ymax = dt(m1:m2,5); xmax = dt(m1:m2,6);

% The boundary observations are adjusted to be consisent with the CRRA utility function 
% with the mininum demand ratio parameter, w = 0.001. 

    my = []; mx = [];
   
    for j = 1:50
        if ((x(j) < w*y(j))&(y(j) >= w*x(j)))            
            mx = [mx;w*ymax(j)]; my = [my;y(j)];
        elseif ((x(j) >= w*y(j))&(y(j) < w*x(j))) 
            mx = [mx;x(j)]; my = [my;w*xmax(j)];
        elseif ((x(j) < w*y(j))&(y(j) < w*x(j))) 
            mx = [mx;w*ymax(j)]; my = [my;w*xmax(j)];
        else
            mx = [mx;x(j)]; my = [my;y(j)];            
        end
    end
    
% Define the log of demand ratios and the log of price ratios    
    
    ldr = log(mx./my); lpr = log(ymax./xmax);   

% Alpha is restricted to be >= 1 and Rho to be >= 0. We reparameterize log(alpha) = exp(pm(1)) 
% and log(rho) = pm(2) where -inf < pm(1), pm(2) < inf. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%% The NLLS estimation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    pm0 = [-3; -2];

    [pm,ssr] = fminsearch(@fssr,pm0,optimset('Display','final'),ldr,lpr,w);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% Estimation Results for Subject id
% Alpha and Rho, their standard errors (using a boostrap method) and r-square
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    alpha = exp(exp(pm(1))); rho = exp(pm(2));  
    
    sst = sum((ldr).^2); r2 = 1-ssr/sst;
    
    se = bstrap(ldr,lpr,w,500);
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

    out = [id,alpha,se(1),rho,se(2),r2];
    
    results = [results;out];
    
end

save results results

