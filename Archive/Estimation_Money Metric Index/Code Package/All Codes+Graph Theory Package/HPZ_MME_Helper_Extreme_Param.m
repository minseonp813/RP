function [aggregate] = HPZ_MME_Helper_Extreme_Param(single_param,endowment,observations,treatment,function_flag,metric,pref_class)

% The function calculates the MME criterion per subject. Given a
% specific functional form and prices, we look for the lowest expenditure 
% level that yields at least the same level of utility as does the observed
% choices. We aggregate these differences in a few ways. By metric we
% choose one of those.
% param is a vector of two parameters - beta (the disappointment aversion
% parameter) and the utility function parameter (rho for CRRA and A for
% CARA).
% endowment is a column vector. The number of elements is the number 
% of observations. Each row is a number.
% observations is a matrix with obs_num rows and 4 columns.
% each row is one choice of the subject.
% The first column is the quantity of good 1 chosen by the subject.
% The second column is the quantity of good 2 chosen by the subject.
% The third column is the price of good 1. 
% The fourth column is the price of good 2. 
% treatmentis the number of treatment in CFGK (2007).
% function_flag is the type of utility function chosen for the prizes:
% 1 - CRRA.
% 2 - CARA.
% metric is the type of aggregate we are interested in:
% 1 - max waste.
% 2 - average waste.
% 3 - sum of square of wastes.
% The function returns an aggregate of waste.

%% "True" value of this boolean flag restricts beta to be equal to zero
global beta_zero_flag

%% "True" value of this boolean flag restricts rho to be equal to zero
global rho_zero_flag

if beta_zero_flag == true
    % if beta is restricted to be 0, then:
    
    % initialize the parameter vector
    param = zeros(1,2);
    
    % set beta to be equal to 0, and rho as it is
    param(2)=single_param;
    
elseif rho_zero_flag == true
    % if rho is restricted to be 0, then:
    
    % initialize the parameter vector
    param = zeros(1,2);
    
    % set rho to be equal to 0, and beta as it is
    param(1)=single_param;
    
end

% compute the aggregated waste for the given parameter set, using MME
[max_waste,average_waste,sum_of_squares_waste] = HPZ_MME_Criterion(param,endowment,observations,treatment,function_flag,pref_class);

if metric==1
    
    aggregate=max_waste;
    
elseif metric==2
    
    aggregate=average_waste;
    
else
    
    aggregate=sum_of_squares_waste;
    
end