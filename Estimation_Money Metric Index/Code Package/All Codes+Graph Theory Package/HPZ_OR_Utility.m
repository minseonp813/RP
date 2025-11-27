function [utility] = HPZ_OR_Utility (x, alpha, rho, flag)

% The function calculates the utility function in CFGK (2007) for a given 
% bundle.
% prob_x is the probability of the account x.
% x is a vector of quantities - x(1) is the quantity of x in the given 
% lottery while x(2) is the quantity of y in the given lottery.
% beta is the disappointment aversion parametr (Gul (1991)).
% rho is the parameter of the CRRA function.
% A is the parameter of the CARA function.
% flag is the parameter that specifies v(x) - in CFGK (2007) in is either 
% CRRA (flag=1) or CARA (flag=2) 

% By Gul (1991), in the case of two prizes, alpha is the probability of the 
% higher prize.
%beta

%rho

%x

% First, let us calculate v(max(x,y)) and v(min(x,y)) 
if flag==1
    self = alpha*(x(1,1)^rho);
    other = (1-alpha)*(x(2,1)^rho);
    utility = (self+other)^(1/rho);
end

% The optimization algorithms sometimes collapse due to infinite values. Therefore 
% we replace it with a very big number (-4.5036e+015). 
if isinf(utility)
    if utility < 0
        utility=-(1/eps);
    else
        utility=(1/eps);
    end
end

