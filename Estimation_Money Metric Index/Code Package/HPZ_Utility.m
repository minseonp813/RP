function [utility] = HPZ_Utility (prob_x, x, beta, rho, A, flag)

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

if x(1)>x(2)
    alpha=prob_x;
else
    alpha=1-prob_x;
end

% By Gul (1991), the weights are calculated using gamma(alpha)
gamma=alpha/(1+((1-alpha)*beta));

% By Gul (1991), the disappointment aversion utility function for the case
% of two prizes is the weighted sum: 
% gamma*v(max(x,y))+(1-gamma)*v(min(x,y))

% First, let us calculate v(max(x,y)) and v(min(x,y)) 
if flag==1
    v_max=CRRA(max(x),rho);
    v_min=CRRA(min(x),rho);
elseif flag==2
    v_max=CARA(max(x),A);
    v_min=CARA(min(x),A);
end

utility = (gamma*v_max)+((1-gamma)*v_min);

% if x is zero and rho is greater than 1, in CRRA we get -inf. The
% optimization algorithms sometimes collapse due to this value. Therefore 
% we replace it with a very big number (-4.5036e+015). 
if isinf(utility)
    if utility < 0
        utility=-(1/eps);
    else
        utility=(1/eps);
    end
end

