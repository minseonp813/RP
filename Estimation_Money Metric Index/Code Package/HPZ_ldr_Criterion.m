function [ldr_criterion] = HPZ_ldr_Criterion(observations,optimal)

% The function calculates the NLLS criterion (similar to CFGK (2007)) for 
% a given set of observations and predictions.
% The function returns the value of the criterion for the specified
% functional form and the given data. 

   ldr_chosen = log(observations(:,2)./observations(:,1));
       
   ldr_predicted = log(optimal(:,2)./optimal(:,1));
       
   ldr_criterion = (ldr_chosen - ldr_predicted)'*(ldr_chosen - ldr_predicted);
   
end
   
    
