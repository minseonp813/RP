%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Nonlinear Least Squares (NLLS) Estimation in Choi, Fisman, Gale and Kariv, 
% "Consistency and Heterogeneity of Individual Behavior under Uncertainty", 
% American Economic Review, December 2007.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NLLS Estimation with CARA specification using the data from the asymmetric treatment
% where the probability of state 1 being selected is 2/3.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clc

% Load the data file
% data_sym has six column vectors. The first column is about subject ID and
% the second is about the number of decision rounds.
% The third to the six column contain the decisions and the parameter in the budget set,
% (y, x, ymax, xmax). 

load data_asym2; dt = data_asym2; 

n = length(dt)/50; % n is the number of subjects in this asymmetric treatment

results = [];

for i = 1:n
    
    m1 = 50*(i-1)+1; m2 = 50*i; id = dt(m1,1)

% Call the demands in x and y accounts and the x and y intercepts in the budget sets
    
    y = dt(m1:m2,3); x = dt(m1:m2,4); ymax = dt(m1:m2,5); xmax = dt(m1:m2,6);
            
%%%%%%%%%%%%%%%%%%%%%%%%%%%% The NLLS estimation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    pm0 = [-3; -2];

    [pm,ssr] = fminsearch(@fssr,pm0,optimset('Display','final'),y,x,ymax,xmax);
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% Estimation Results for Subject id
% Alpha and Rho, their standard errors (using a boostrap method) and r-square
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    alpha = exp(exp(pm(1))); rho = exp(pm(2));  
    
    sst = sum((y-x).^2); r2 = 1-ssr/sst;
    
    se = bstrap(y,x,ymax,xmax,500);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

    out = [id,alpha,se(1),rho,se(2),r2];
    
    results = [results;out];
    
end

save results results




