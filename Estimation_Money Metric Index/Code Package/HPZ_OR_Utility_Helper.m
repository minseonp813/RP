function [minus_utility] = HPZ_OR_Utility_Helper(x,param1,param2,flag, varargin)

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
% flag is the type of utility function chosen for the prizes:
% 1 - CRRA.
% 2 - CARA.
% metric is the type of aggregate we are interested in:
% 1 - max waste.
% 2 - average waste.
% 3 - sum of square of wastes.
% The function returns an aggregate of waste.

if (nargin == 1)
    alpha_multiplier = varargin(1);
else
    alpha_multiplier = 1;
end

minus_utility = alpha_multiplier * (0-HPZ_OR_Utility(x,param1,param2,flag));