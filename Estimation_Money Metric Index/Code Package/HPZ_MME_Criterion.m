function [max_waste,average_waste,avg_sum_of_squares_waste] = HPZ_MME_Criterion(param,endowment,observations,treatment,function_flag,pref_class)
%% using numeric approach
global numeric_flag

% The function calculates the MME criterion (=lowest expenditure) per subject. Given a
% specific functional form and prices, we look for the lowest expenditure 
% level that yields at least the same level of utility as does the observed
% choices. We aggregate these differences in a few ways.
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
% flag is the type of utility function chosen for the prizes:
% 1 - CRRA.
% 2 - CARA.
% The function returns three aggregates of waste (one minus the minimal 
% expenditure) - max, mean and sum of squares

warning ('on','all');

if numeric_flag == true
    criterion = HPZ_MME(param,endowment,observations,treatment,function_flag,pref_class);
else
    criterion = HPZ_MME_Analytical(observations,param,function_flag,pref_class);
end

waste=endowment-criterion;

max_waste=max(waste);

average_waste=mean(waste);

if average_waste < -0.005
   
    warning ('average waste is negative');
    
end

avg_sum_of_squares_waste=sqrt(meansqr(waste));

