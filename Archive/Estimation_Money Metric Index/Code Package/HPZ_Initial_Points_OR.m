function [initial_points_report] = HPZ_Initial_Points_OR ()
% The function constructs a matrix of initial values for the parameters 
% optimization
% The function returns a matrix with initial_points_num rows and two columns: 
% The first column is the initial value for beta.
% The second column is the initial value for the additional parameter 
% (rho if CRRA, A if CARA).


% global starting_points_estimation
global max_starting_points

% Initializa the matrix
initial_points_report = zeros(max_starting_points,2);

% The first initial point is the choice of CFGK (2007)
% initial_points_report(1,1)=exp(exp(-3))-1;
% initial_points_report(1,2)=exp(-2);

% if zero_rho_initial == true
%     % The second initial point [ 0 0 ]
%     initial_points_report(2,:)=[0 0];
% else
%     initial_points_report(2,:)=rand(1,2);
% end

% A matrix of random matrix in (0,1)
temp=rand(max_starting_points,2);

% for alpha (between 0 and 1)
initial_points_report(:,1) = temp(:,1);

% for rho (between -2 and 1)
initial_points_report(:,2) = 3*temp(:,1)-2;

end


