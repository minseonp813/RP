function [initial_points_report] = HPZ_Initial_Points (beta_flag, rho_flag, zero_rho_initial)
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
initial_points_report(1,1)=exp(exp(-3))-1;
initial_points_report(1,2)=exp(-2);

if zero_rho_initial == true
    % The second initial point [ 0 0 ]
    initial_points_report(2,:)=[0 0];
else
    initial_points_report(2,:)=rand(1,2);
end

% A matrix of random matrix in (0,1)
temp=rand(max_starting_points-2,2);

if rho_flag == true
    % for the additional parameter the value is taken from (0,1)
    % if subject has corner solutions and the CRRA is choses as the
    % utility (not implemented)
    initial_points_report(3:max_starting_points,2)=temp(:,2);
else
    % for the additional parameter the value is taken from (0,2), otherwise
    initial_points_report(3:max_starting_points,2)=2*temp(:,2);
end

for i=3:max_starting_points

    if beta_flag==2
        % if beta is constrained to be non-negative,the value is taken
        % from (0,2)
        initial_points_report(i,1)=2*temp(i-2,1);
    else
        % if beta is not constrained to be non-negative,the value is taken
        % from (-1,2)
        
        % if we have corners with CRRA and Rho is close to 0, Beta should
        % be non negative (not implemented)
        if rho_flag == true && initial_points_report(i,2) >= 0 && initial_points_report(i,2) < 5e-2
            initial_points_report(i,1)=abs(3*temp(i-2,1)-1);
        else
            initial_points_report(i,1)=3*temp(i-2,1)-1;
        end
    end        
end

end


