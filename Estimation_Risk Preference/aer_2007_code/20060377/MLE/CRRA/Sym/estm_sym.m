%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Maximum Likelihood Estimation (MLE) in Choi, Fisman, Gale and Kariv, 
% "Consistency and Heterogeneity of Individual Behavior under Uncertainty", 
% American Economic Review, December 2007.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ML Estimation with CRRA specification using the data from the symmetric treatment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clc

% Load the data file
% data_sym has six column vectors. The first column is about subject ID and
% the second is about the number of decision rounds.
% The third to the six column contain the decisions and the parameter in the budget set,
% (y, x, ymax, xmax). 

load data_sym; dt = data_sym; 

n = length(dt)/50; % n is the number of subjects in the symmetric treatment

results = [];

for i = 1:n
    
    m1 = 50*(i-1)+1; m2 = 50*i; id = dt(m1,1)

% Call the demands in x and y accounts and the x and y intercepts in the budget sets
    
    y = dt(m1:m2,3); x = dt(m1:m2,4); ymax = dt(m1:m2,5); xmax = dt(m1:m2,6);

    if ((id == 205)|(id == 218)|(id == 320))

% Three subjects (id 205, 218, 320) in most of decision rounds secure 10 tokens in a selected budget 
% set and thus we normalize such behavior to be consistent with risk neutrality.      
        
        y = max(y - 10,0); x = max(x - 10,0); ymax = ymax - 10; xmax = xmax - 10;
        
    end
        
% Alpha is restricted to be >= 1 and Rho to be >= 0. We reparameterize log(alpha) = exp(pm(1)) 
% and log(rho) = pm(2) where -inf < pm(1), pm(2) < inf. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%% The ML estimation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    pm0 = [-3; -1; -2];
    [pm,neg_loglik] = fminsearch(@likh,pm0,optimset('Display','final'),y,x,ymax,xmax);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% Estimation Results for Subject id
% Alpha and Rho, their standard errors (using a boostrap method) and loglikelihood at the maximum
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    alpha = exp(exp(pm(1))); rho = exp(pm(2)); sgm = exp(pm(3));
    
    se = bstrap(y,x,ymax,xmax,100);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    
    out = [id,alpha,se(1),rho,se(2),sgm,-neg_loglik];
    
    results = [results;out];
    
end

save results results


