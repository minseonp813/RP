function [param_1,param_2,final_output] = HPZ_NLLS (data,obs_num,treatment,function_flag,beta_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class)

% this function estimates the parameters of the utility function
% according to the choices made by the subject.
% it does all its calculations for a single subject.
% in order to deal with all the subjects in the database, 
% a loop is in place in HPZ_Interface, that passes each 
% time the data of a single subject to this function.
 
% data is a matrix with six columns (We assume that the endowment is fixed at 1):
% The first column is the subject ID.
% The second column is the observation number. 
% The third column is the quantity of good 1 chosen by the subject.
% The fourth column is the quantity of good 2 chosen by the subject.
% The fifth column is the price of good 1. 
% The sixth column is the price of good 2. 
% obs_num is the number of observations.
% treatment is the number of treatment in CFGK (2007).
% function_flag is the type of utility function chosen for the prizes:
% 1 - CRRA.
% 2 - CARA.
 
% beta_flag indicates what restrictions should be put on beta and rho:
% 1 – rho >= 0 , beta > -1
% 2 – rho >= 0 , beta >= 0 (as in choi et al(2007)).
% zeros_flag indicates whether correction for corner choices should be held:
% 1 – No
% 2 – Yes (as in choi et al(2007)).
% metric flag indicates the aggregator for the differences between the
% observed and predicted bundles.
% 1 - Euclidean norm.
% 2 - Geometric mean (as in choi et al(2007)). 
% asymmetric_flag should equal to 0 for treatments that 
% involve equal probability of the two states.
% in case, the probability is not 50-50 
% (as in treatments 2 and 3 of CFGK),
% this flag should be 1 (to reconstruct CFGK results use 0).
% pref_class indicates the type of preferences being examined:
% 1 – risk preferences
% 2 – other regarding (OR) preferences
 
%% This file keeps track of all results produced by the fminsearchbnd
% global fid_NLLS_final_report
% global output_flag_vec
%% "True" value of this boolean flag restricts beta to be equal to zero
global beta_zero_flag
%% "True" value of this boolean flag restricts rho to be equal to zero
% global rho_zero_flag
%% The max number of convergence points NLLS
global NLLS_min_counter
% Max number of initial points for the fminsearchbnd optimization procedure
global max_starting_points
% numeric calculation or analytic calculation
% numeric – TRUE , analytic - FALSE
global numeric_flag
% whether to print all the results of the 
% matrix of the best points found
% (stored in optimal_parameters_matrix),
% or only the one best point.
% 1 – only the best point ,
% otherwise – all the points in the best points matrix
global write_all_flag

if (numeric_flag)
    % numeric
    % now we only use max 20 iterations as our upper bound
    max_starting_points = 20;
else
    % analytical
    % now we only use max 100 iterations as our upper bound    
    max_starting_points = 100;
end

% Time (minutes) that user want to spend on running the code on his machine
global NLLS_max_time_estimation

if NLLS_max_time_estimation == Inf
    NLLS_time = NLLS_max_time_estimation;
else
    % the entire time that user wants to invest on estimation
    NLLS_time = NLLS_max_time_estimation * 60; 
end

% keeps track of the time it takes for the entire estimation process
% t_total = tic;    
 
% choose proper algorithm for the fminsearchbnd optimization procedure
% the algorithm is for interior points
% not displaying error messages
options = optimset('Algorithm','interior-point','Display','off');

% subject_data is a matrix with obs_num rows and 4 columns.
% each row is one choice of the subject.
% The first column is the quantity of good 1 chosen by the subject.
% The second column is the quantity of good 2 chosen by the subject.
% The third column is the price of good 1. 
% The fourth column is the price of good 2. 
subject_data = data(1:obs_num,3:6);

% calculate endowments
Choices(:,1:2)=data(1:obs_num,3:4);
    
Choices(:,3:4)=data(1:obs_num,5:6);

% a matrix of the size obs_numXobs_num
% in the i,j cell we have the expenditure on the bundle
% that was chosen in the i'th observation given the
% prices of the j'th observation.
 
expenditure = (Choices(:,1)*Choices(:,3)' + Choices(:,2)*Choices(:,4)')';
 
% the vector of length obs_num which indicates the 
% endowment in each observation

endowments = diag(expenditure);

if zeros_flag==2 % Choi et al. (2007) correction
    
    % Choi et al. (2007) correction is applied for corner choices
    observations = HPZ_No_Corners (subject_data,obs_num,1);
    
else
    
    observations = subject_data;

end

% in risk preferences, when the functional form is 
% DA with CRRA\CARA we wish to have [0,0] as one of 
% the starting points for the optimization routine.
% zero_rho_initial:
% true - [0,0] is one of the initial points.
% false - [0,0] is not one of the initial points.

zero_rho_initial = false;
if ((pref_class == 1) && (function_flag == 1 || function_flag == 2))
    zero_rho_initial = true; 
end

%% Setting initial point for the fminsearchbnd optimization procedure
% The first two initial points are the point chosen by CFGK (2007) and the
% rest are points chosen randomly.

% a flag that is not used - enables the user to 
% choose adequate initial points in case 
% rho is restricted (in the CRRA) to be in [0,1].
restricted_rho = false;

if pref_class==1 
    % initial points for risk preferences 
    initial_points = HPZ_Initial_Points (beta_flag,restricted_rho,zero_rho_initial);
elseif pref_class==2
    % initial points for other regarding (OR) preferences
    initial_points = HPZ_Initial_Points_OR ();
end

%% Setting initial points for the fminsearchbnd optimization procedure
% Initializing the structures that hold the estimation results
 
% (this matrix keeps track of all estimations of parameters)
results=zeros(max_starting_points, 2);
   
% (keeps track of all function values)
criterion=zeros(max_starting_points, 1);
 
% a 2-D matrix with a size of: 3 x min_fvals 
% (min_fvals is the max number of convergence points)
% the 3 columns represent, respectively: 1.Beta 2.Rho 3.Fval
optimal_parameter_matrix = zeros(NLLS_min_counter, 3);

if pref_class == 1 % risk preferences
    
    % calculating the function value for the initial point (0,0)    
 
    [criterion_EU] = HPZ_Criterion([0 0],observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class);
 
    % entering the result to all the relevant variables and matrices
    
    optimal_parameter_matrix(1,1:2) = [0 0];
 
    optimal_parameter_matrix(1,3) = criterion_EU;
 
    fval_temp_min = criterion_EU;
    
elseif pref_class == 2 % other regarding preferences
 
    % calculating the function value for the initial point (0.5,0)
 
    [criterion_FCB] = HPZ_Criterion([0.5 0],observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class);
 
    % entering the result to all the relevant variables and matrices
 
    optimal_parameter_matrix(1,1:2) = [0.5 0];
 
    optimal_parameter_matrix(1,3) = criterion_FCB;
 
    fval_temp_min = criterion_FCB;
    
end
      
% an arbitrary threshold.
% minimal values of function value will be
% considered equal if they are less than this 
% threshold from one another.

fval_threshold = 5e-2;

% number of consecutive equal (+-fval_threshold) 
% function values.
% process will stop once equal_fval_counter 
% will be equal to NLLS_min_counter.

equal_fval_counter = 1;

% define the waitbar
h_wb = waitbar(0,strcat('Recovering Subject-',num2str(data(1,1)), ' Preferences. Please wait...'), 'name', 'Estimation is running...');
new_bar_val = 0;

% This array keeps track of time that is spent on each covergence point
% time_optim_conv_pnt = zeros(1, NLLS_min_counter);

% the next 2 time-related variables are only
% required if NLLS_time ~= Inf
 
% keeps track of accumulative time for all initial points
time_init_accum = 0;
% time for a certain initial point
time_init_pt = 0;

% termination_flag = false;

%epsilon_lb = 10 ^(-5);

for j=1:max_starting_points

    %% Estimation withought time limit
    if NLLS_time == Inf
        % if TRUE, continue processing.
        % else, do nothing (the loop continues till
        % j==max_starting_points, but does nothing
        if (equal_fval_counter < NLLS_min_counter)

            %% Updating the waitbar
            if equal_fval_counter > 0 
                % update the value of the progress bar
                new_bar_val = equal_fval_counter / NLLS_min_counter;
            end
            waitbar((j / max_starting_points) + new_bar_val);

            % calculate estimation time for each point
            % t_optim = tic;
            
            %% computations take place here
            
            if pref_class == 2 % other regarding preferences

                [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_Criterion(param,observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class),initial_points(j,:),[0 -inf], [1 1], options);                                            

            % if it is risk preferences, there are
            % three possible restrictions on beta:
                
            elseif beta_zero_flag == true
                % set beta to be 0, as we restricted it to be so.
                results(j,1) = 0;
                % the optimization process only searches for the optimal value
                % of the rho, given beta is equal to 0.
                % Note: in this case values that can be taken by rho are all
                % positive values within the range of (0,inf).
                [results(j,2),criterion(j)]=fminsearchbnd(@(param) HPZ_Criterion_Extreme_Param(param,observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag),initial_points(j,2), 0, [], options);

            elseif beta_flag==2
                % Rho >= 0 and Beta >= 0
                [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_Criterion(param,observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class),initial_points(j,:),[0 0], [], options);                            
                
            elseif beta_flag==1
                % Rho >= 0 and Beta > -1 
                %[results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_Criterion(param,observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag),initial_points(j,:),[-1+epsilon_lb 0], [], options);
                [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_Criterion(param,observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class),initial_points(j,:),[-1 0], [], options);   
            end

            % check if the resulting criterion is equal 
            % (+- fval_threshold) to computed minimum criterion 
            if ( abs(fval_temp_min - criterion(j)) <= fval_threshold )
                % update the convergence counter
                equal_fval_counter = equal_fval_counter + 1;

                % update the estimation running time for the convergence point
                % time_optim_conv_pnt(equal_fval_counter) = toc(t_optim);

                % store new minimum in the matrix
                optimal_parameter_matrix(equal_fval_counter, 1:2) = results(j,:);
                optimal_parameter_matrix(equal_fval_counter,   3) =  criterion(j);

                % update the minimum when the the new function value is
                % smaller than the current minimum
                if(criterion(j) < fval_temp_min)
                    % update the temporary minimum of fvals
                    fval_temp_min = criterion(j);

                    % check the previous results to see if they are within the
                    % new acceptance range for criterion

                    % number of violations
                    mismatch_counter = 0;

                    % create a copy of the current number of convergence points
                    temp_equal_fval_counter = equal_fval_counter;

                    % among all current convergence points:
                    % check if they all are within new acceptance range
                    for j_ind = 1 : equal_fval_counter
                        % if the point is not within the range:
                        if (optimal_parameter_matrix(j_ind,   3) > (fval_temp_min + fval_threshold))
                            % replace that element with inf
                            optimal_parameter_matrix(j_ind,   :) = Inf;
                            % update the value of temporary counter for
                            % convergence points
                            temp_equal_fval_counter = temp_equal_fval_counter - 1;
                            % update the number of violations
                            mismatch_counter = mismatch_counter + 1;
                        end
                        %% if the point is within the range - do nothing
                    end
                    
                    % re-arrange the results vector:
                    % start index
                    start_ind = 1;
                    % end index
                    end_ind = equal_fval_counter;
                    % a temporary point value for swaping values
                    % temp_val = zeros(1,3);
 
                    % Go through the result vector as long as there is still
                    % a violation
                    while mismatch_counter > 0 && end_ind > 0 && start_ind < equal_fval_counter
                        % push Infs to the end of the array
                        if (optimal_parameter_matrix(start_ind,   3) == Inf)
                            % swap the Inf element with the last element
                            temp_val = optimal_parameter_matrix(start_ind,   :);
                            optimal_parameter_matrix(start_ind,   :) = optimal_parameter_matrix(end_ind,   :);
                            optimal_parameter_matrix(end_ind,   :) = temp_val;
                            % update mismatch counter value
                            mismatch_counter = mismatch_counter - 1;
                            % update pointers
                            end_ind  = end_ind - 1;
                        else
                            % if the value is not Inf, just move the pointer forward
                            start_ind = start_ind + 1;
                        end
                    end

                    % update equal_fval_counter
                    equal_fval_counter = temp_equal_fval_counter;

                end

            % checks if we found a new minimum 
            % (that is: criterion(j) < fval_temp_min)
            % AND the old minimum is not within the threshold
            % of the new minimum
            elseif (criterion(j) < fval_temp_min && abs(fval_temp_min - criterion(j)) > fval_threshold )
                % update minimum
                fval_temp_min = criterion(j);
                % update counter
                equal_fval_counter = 1;
                % update result matrix:
                
                % first reset the matrix:
                optimal_parameter_matrix = zeros(NLLS_min_counter, 3);
 
                % then store new minimum in the matrix
                optimal_parameter_matrix(equal_fval_counter, 1:2) = results(j,:);
                optimal_parameter_matrix(equal_fval_counter,   3) =  criterion(j);
 
            end % if not a new minimum and not within the threshold, do nothing.

            % if optimization is terminated
            if equal_fval_counter == NLLS_min_counter || j == max_starting_points
                % store the total number of used initial points
                total_init_points = j;
            end

        end
        
    else % NLLS_time ~= Inf
        %% Estimation with time limit
        
        %% Updating the waitbar
        waitbar(time_init_pt/NLLS_time);
        
        % calculate estimation time for each convergence point
        % t_optim = tic;
        
        % calculate estimation time for each initial point
        t_init = tic;

        %% computations take place here
        
        % if NLLS_time is greater than time passed so far, 
        % continue processing.
        % else, do nothing (the loop continues till
        % j==max_starting_points, but does nothing
        
        if NLLS_time > (time_init_accum + time_init_pt)

            if pref_class == 2 % other regarding preferences

                [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_Criterion(param,observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class),initial_points(j,:),[0 -inf], [1 1], options);                                            

            % if it is risk preferences, then there
            % are 3 options regarding beta:
            % beta == 0, beta >= 0, beta > -1
                
            elseif beta_zero_flag == true
                % if beta is restricted to be equal to 0, and there is no
                % constraint on the rho's value, then:

                % set beta to be 0, as we restricted it to be so.
                results(j,1) = 0;
                % the optimization process only searches for the optimal value
                % of the rho, given beta is equal to 0.
                % Note: in this case values that can be taken by rho are all
                % positive values within the range of (0,inf).
                [results(j,2),criterion(j)]=fminsearchbnd(@(param) HPZ_Criterion_Extreme_Param(param,observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag),initial_points(j,2), 0, [], options);

            elseif beta_flag==2
                % Rho >= 0 and Beta >= 0
                [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_Criterion(param,observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class),initial_points(j,:),[0 0], [], options);                            

            elseif beta_flag==1
                % Rho >= 0 and Beta > -1
                % [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_Criterion(param,observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag),initial_points(j,:),[-1+epsilon_lb 0], [], options);
                [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_Criterion(param,observations,treatment,function_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class),initial_points(j,:),[-1 0], [], options);    
            end

            % update the estimation running time for the convergence point
            % time_optim_conv_pnt(j) = toc(t_optim);
            
            if j < NLLS_min_counter
 
                % if the optimal_parameter_matrix is not yet full,
                % just add the new value to the matrix
 
                % j+1 and not j, since we had one value
                % before we entered the loop
                
                optimal_parameter_matrix(j+1, 1:2) = results(j,:);
                
                optimal_parameter_matrix(j+1,   3) = criterion(j);
                
            else

                % if the optimal_parameter_matrix is already full,
                % then:
                % if the maximum value currently in the matrix is
                % bigger than the new value –
                % then replace the maximum with the new value
                % if the new value is bigger than all values
                % currently in the matrix – do nothing
                
                [worst_c,worst_i] = max(optimal_parameter_matrix(:,   3));
                
                if criterion(j) < worst_c
                   
                   optimal_parameter_matrix(worst_i, 1:2) = results(j,:); 
                   
                   optimal_parameter_matrix(worst_i,   3) = criterion(j);
                    
                end
                
            end
            
            % measure the time for each initial point
            time_init_pt = toc(t_init);
            % measure the accumulative time time for all initial points so far
            time_init_accum = time_init_accum + time_init_pt;
            % store the total number of used initial points
            total_init_points = j;

        end % end of if (NLLs_time > time passed)
                
    end % end of if (NLLS_time == Inf), else
 
end % end of loop
 
% update the waitbar
remainder_multplier = 0.20;
new_bar_val = 1 - (remainder_multplier * 1);
waitbar(new_bar_val);

if NLLS_time ==Inf

    % sorting the matrix of the best points discovered
    optimal_parameter_matrix = sortrows(optimal_parameter_matrix(1:equal_fval_counter,:),3);
    
    if write_all_flag == 1 % print only the best solution
        
        max_index = 1;
        
    else % print all the solutions in optimal_parameter_matrix
        
        max_index = equal_fval_counter; 
        
    end
    
    final_output = zeros(max_index, 8);    
    
    % for each point we store in final_output the values 
    % that were already calculated (point coordinates and 
    % NLLS Criterion using Euclidean metric), and three 
    % other criterions (one of them (MME) requires three 
    % slots (rows 5-7))    
    for i = 1 : max_index
        % [~,best_index]=min(criterion);
        final_output(i,1)=optimal_parameter_matrix(i, 1);
        final_output(i,2)=optimal_parameter_matrix(i, 2);
        
        % NLLS Criterion using Euclidean metric
        final_output(i,3)=optimal_parameter_matrix(i, 3);
        
        % The endowments are normalized to 1
        % endowment=ones(length(observations(:,1)),1);

        % NLLS Criterion using Choi et al. (2007) metric
        final_output(i,4)=HPZ_Second_Criterion(final_output(i,1:2),observations,treatment,function_flag,zeros_flag,mod(metric_flag+1,2),asymmetric_flag,pref_class);

        % MME Criterion (Max Waste, Mean Waste, Sum of Squares Wastes)
        [final_output(i,5),final_output(i,6),final_output(i,7)]=HPZ_MME_Criterion(final_output(i,1:2),endowments,observations,treatment,function_flag,pref_class);
        
        % BI Criterion
        [final_output(i,8)] = HPZ_BI_Criterion(final_output(i,1:2),endowments,observations,treatment,function_flag,pref_class);
        
        %% finalize the output file for distinct results
        %action_flag = 2; %% NLLS
        %[fid_NLLS_final_report] = HPZ_Write_Result_File_Finalizer(action_flag, i , fid_NLLS_final_report, final_output, num2str(data(1,1)), output_flag_vec);
        % update the waitbar
        waitbar(new_bar_val + ((i/NLLS_min_counter)*remainder_multplier));
    end
    
else
    %% time limited estimation
    num_of_points = min(total_init_points,NLLS_min_counter);
    
    optimal_parameter_matrix = sortrows(optimal_parameter_matrix(1:num_of_points,:),3);
    
    if write_all_flag == 1
        
        max_index = 1;
        
    else
        
        max_index = num_of_points;
        
    end
    
    final_output = zeros(max_index, 8);
    
    for i = 1 : max_index    
        % [~,best_index]=min(criterion);
        final_output(i,1)=optimal_parameter_matrix(i, 1);
        final_output(i,2)=optimal_parameter_matrix(i, 2);
        % NLLS Criterion using Euclidean metric
        final_output(i,3)=optimal_parameter_matrix(i, 3);

        % The endowments are normalized to 1
        % endowment=ones(length(observations(:,1)),1);
        
        % NLLS Criterion using Choi et al. (2007) metric
        final_output(i,4)=HPZ_Second_Criterion(final_output(i,1:2),observations,treatment,function_flag,zeros_flag,mod(metric_flag+1,2),asymmetric_flag,pref_class);

        % MME Criterion (Max Waste, Mean Waste, Sum of Squares Wastes)
        [final_output(i,5),final_output(i,6),final_output(i,7)]=HPZ_MME_Criterion(final_output(i,1:2),endowments,observations,treatment,function_flag,pref_class);

        % BI Criterion
        [final_output(i,8)] = HPZ_BI_Criterion(final_output(i,1:2),endowments,observations,treatment,function_flag,pref_class);

        %% finalize the output file for distinct results
        %action_flag = 2; %% NLLS
        %[fid_NLLS_final_report] = HPZ_Write_Result_File_Finalizer(action_flag, i , fid_NLLS_final_report, final_output, num2str(data(1,1)), output_flag_vec);

        % update the waitbar
        waitbar(new_bar_val + ((i/total_init_points)*remainder_multplier));
    end
end

%[~,best_params_index] = min (final_output(:,3));

% results: the estimation of rho (param_1) and beta (param_2)

param_1 = optimal_parameter_matrix(1,1);

param_2 = optimal_parameter_matrix(1,2);

% keep track of the entire estimation process
% time_total = toc(t_total);

% close the waitbar
close(h_wb);

end