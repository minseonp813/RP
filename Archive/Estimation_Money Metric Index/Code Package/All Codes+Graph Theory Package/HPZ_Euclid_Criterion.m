function [euclid_criterion] = HPZ_Euclid_Criterion(observations,optimal)

% The function calculates the Euclidean NLLS criterion for 
% a given set of observations and predictions.
% The function returns the value of the criterion for the given data. 

euclid_criterion =  sum(sqrt( sum((observations(:,1:2) - optimal).^2, 2)) );
   
    
