function [c,ceq] = HPZ_OR_Utility_Constraint (x, alpha, rho, flag, utility)

% The function generates the inequality constraint for the mincon function
% in the MME procedure.
% prob_x is the probability of the account x.
% x is a vector of quantities - x(1) is the quantity of x in the given 
% lottery while x(2) is the quantity of y in the given lottery.
% beta is the disappointment aversion parametr (Gul (1991)).
% rho is the parameter of the CRRA function.
% A is the parameter of the CARA function.
% flag is the parameter that specifies v(x) - in CFGK (2007) in is either 
% CRRA (flag=1) or CARA (flag=2) 
% utility is the level of utility in the observation.

% c=real(utility-HPZ_Utility (prob_x, x, beta, rho, A, flag));
% 
% if (isinf(c)||isnan(c))
%     disp('oi-HPZ_Utility_Constraint');
% end
% 
% ceq=0;

%% Equality constraint!

ceq=real(utility-HPZ_OR_Utility (x, alpha, rho, flag));

if (isinf(ceq)||isnan(ceq))
    disp('oi-HPZ_Utility_Constraint');
end

c=[]; % set nonequality constraint to null