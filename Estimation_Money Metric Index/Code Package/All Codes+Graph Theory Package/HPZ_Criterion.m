function [criterion] = HPZ_Criterion(param,observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class)
%% using numeric approach
global numeric_flag

% The function calculates the NLLS criterion (similar to CFGK (2007)) for 
% a given specification of a utility function and a set of choices. 
% param is a vector of two parameters - beta (the disappointment aversion
% parameter) and the utility function parameter (rho for CRRA and A for
% CARA).
% observations is a matrix with obs_num rows and 4 columns.
% each row is one choice of the subject.
% The first column is the quantity of good 1 chosen by the subject.
% The second column is the quantity of good 2 chosen by the subject.
% The third column is the price of good 1. 
% The fourth column is the price of good 2. 
% treatment is the number of treatment in CFGK (2007).

% The function returns the value of the criterion for the specified
% functional form and the given data. 

obs_num = length(observations(:,1));

% calculate endowments
Choices(:,1:2)=observations(1:obs_num,1:2);
    
Choices(:,3:4)=observations(1:obs_num,3:4);

expenditure = (Choices(:,1)*Choices(:,3)' + Choices(:,2)*Choices(:,4)')';

endowments = diag(expenditure);

if numeric_flag == true
    optimal_choice_zeros=HPZ_Choices(observations(:,3:4),endowments,treatment,function_flag,asymmetric_flag,param,pref_class);
else
    optimal_choice_zeros=HPZ_Choices_Analytical(observations,param,function_flag,pref_class);
end

if (zeros_flag==2)

    optimal_choice = HPZ_No_Corners (optimal_choice_zeros,obs_num,1);

else
    
    optimal_choice = optimal_choice_zeros;
    
end
    
if (metric_flag==1)
    
   criterion = HPZ_Euclid_Criterion(observations,optimal_choice);
   
else
    
   criterion = HPZ_ldr_Criterion(observations,optimal_choice);
   
end