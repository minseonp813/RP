function [param_1,param_2,final_output] = HPZ_MME_Estimation (data,obs_num,treatment,function_flag,beta_flag,zeros_flag,aggregation,pref_class)
% The function calculates the utility functions of the GARP satisfying 
% subjects in CFGK (2007) for a given bundle using NLLS.
% data is a matrix with six columns (We assume that the endowment is fixed at 1):
% The first column is the subject ID.
% The second column is the observation number - 50 observations per subject
% The third column is the quantity of good 1 chosen by the subject.
% The fourth column is the quantity of good 2 chosen by the subject.
% The fifth column is the price of good 1. 
% The sixth column is the price of good 2. 
% subjects is a vector that lists the subjects IDs.
% subject_num is the number of subjects.
% obs_num is the number of observations per subject.
% treatmentis the number of treatment in CFGK (2007).
% function_flag is the type of utility function chosen for the prizes:
% 1 - CRRA.
% 2 - CARA.
% The function returns a matrix with subject_num rows and nine columns: 
% The first column is the estimated beta using the max measure.
% The second column is the estimated additional parameter (rho if CRRA, 
% A if CARA) using the max measure.
% The third column is the value of the function at the optimum using the max measure.
% The fourth column is the estimated beta using the average measure.
% The fifth column is the estimated additional parameter (rho if CRRA, 
% A if CARA) using the average measure.
% The sixth column is the value of the function at the optimum using the average measure.
% The seventh column is the estimated beta using the sum of squares measure.
% The eighth column is the estimated additional parameter (rho if CRRA, 
% A if CARA) using the sum of squares measure.
% The ninth column is the value of the function at the optimum using the sum of squares measure.

%% This file keep track of all results produced by the fminsearchbnd
%global fid_MME_final_report
%% This set the file configrations
%global output_flag_vec
%% "True" value of this boolean flag restricts beta to be equal to zero
global beta_zero_flag
%% The max number of convergance points MME
global MME_min_counter;
% Max number of initial points for the fminsearchbnd optimization procedure
global max_starting_points
global numeric_flag
global write_all_flag

if (numeric_flag)
    % now we only use max 100 iterations (with different initial points) 
    % as our upper bound numeric
    % (On different iteration number) Analyticial approach needs less time 
    % per iteration while is more sucpetible to initial parameter value (Minseon)
  
    max_starting_points = 20;
else
    % analytical
    max_starting_points = 100;
end

% Time (minutes) that user want to spend on running the code on his machine
global MME_max_time_estimation

if MME_max_time_estimation == Inf
    MME_time = MME_max_time_estimation;
else
    % the entire time that user wants to invest on estimation
    MME_time = MME_max_time_estimation * 60; 
end

% keep track of the entire estimation process
%t_total = tic;

% choose proper algorithm for the fminsearchbnd optimization procedure
options = optimset('Algorithm','interior-point','Display','off');

% subject_data is a matrix with obs_num rows and 4 columns.
% each row is one choice of the subject.
% The first column is the quantity of good 1 chosen by the subject.
% The second column is the quantity of good 2 chosen by the subject.
% The third column is the price of good 1. 
% The fourth column is the price of good 2. 
observations = data(1:obs_num,3:6);

if zeros_flag==2 % Choi et al. (2007) correction
    
    % Choi et al. (2007) correction is applied for corner choices
    subject_data = HPZ_No_Corners (observations,obs_num,1);
    
else
    
    subject_data = observations;

end

%% Handle the corner solutions when CRRA is chosen as the utility function 
restricted_rho = false;
% a flag that restricts rho to be in the range of [0,1] for subjects with
% cornor solutions where CRRA is chosen as the utility function
if ((any(observations(:,1) == 0) > 0 || any(observations(:,2) == 0) > 0) && function_flag == 1 && pref_class == 1)
    % check whether subject has any corner solution(s) (meaning x1 or x2 = 0)
    % among their 50 observations, where the chosen utility function is CRRA
    restricted_rho = true; % if not, u(0)=-inf 
end

% (0,0) case included within the initial parameter settings
zero_rho_initial = false;
if ((pref_class == 1) && (function_flag == 1 || function_flag == 2))
    zero_rho_initial = true; 
end

%% Setting initial point for the fminsearchbnd optimization procedure
% The first two initial points are the point chosen by CFGK (2007) and the
% rest are choosen points randomly.
% initial_points = HPZ_Initial_Points (1,restricted_rho);
if pref_class==1 
    initial_points = HPZ_Initial_Points (1,restricted_rho,zero_rho_initial);
elseif pref_class==2
    initial_points = HPZ_Initial_Points_OR ();
end

% The endowments are normalized to 1
endowment=ones(obs_num,1);

% Initializing the structures that hold the estimation results
% (this matrix keep track of all estimations of parameters)
results=zeros(max_starting_points,2);
   
% (keep track of all fvals)
criterion=zeros(max_starting_points,1);

% Beta Rho Fval : 3xmin_fvals
optimal_parameter_matrix = zeros(MME_min_counter, 3);

if pref_class == 1
    
    [criterion_EU] = HPZ_MME_Helper([0 0],endowment,subject_data,treatment,function_flag,aggregation,pref_class);

    optimal_parameter_matrix(1,1:2) = [0 0];

    optimal_parameter_matrix(1,3) = criterion_EU;

    fval_temp_min = criterion_EU;
    
elseif pref_class == 2

    [criterion_FCB] = HPZ_MME_Helper([0.5 0],endowment,subject_data,treatment,function_flag,aggregation,pref_class);

    optimal_parameter_matrix(1,1:2) = [0.5 0];

    optimal_parameter_matrix(1,3) = criterion_FCB;

    fval_temp_min = criterion_FCB;
    
end

%HPZ_MME_Helper([0.680230833599344 0.000001374739790],endowment,subject_data,treatment,function_flag,aggregation);

% initialization of threshold on the aggregated criterion value
fval_threshold = 1e-5;

% number of consecutive equal fvals
equal_fval_counter = 1;

for j=1:max_starting_points
    
    if MME_time == Inf
        %% Estimation withought time limit
    
        if (equal_fval_counter < MME_min_counter)
             
            if pref_class == 2
                
                [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_MME_Helper(param,endowment,subject_data,treatment,function_flag,aggregation,pref_class),initial_points(j,:),[0 -inf], [1 1], options);
                
            %% EU case:
            elseif beta_zero_flag == true && restricted_rho == false
                % if beta restricted to be equal to 0, and there is no
                % constraint on the rho's value, then:

                % set beta to be 0, as we restricted it to be so.
                results(j,1) = 0;
                % the optimization process only search for the optimal value
                % of the rho, given beta is equal to 0.
                % Note: in this case values that can be taken by rho are all
                % positive values within the range of (0,inf).
                [results(j,2),criterion(j)]=fminsearchbnd(@(param) HPZ_MME_Helper_Extreme_Param(param,endowment,subject_data,treatment,function_flag,aggregation,pref_class),initial_points(j,2), 0, [], options);
                
            elseif beta_zero_flag == true && restricted_rho == true
                % if beta restricted to be equal to 0, and there is a
                % constraint on the rho's value, then:

                % set beta to be 0, as we restricted it to be so.
                results(j,1) = 0;
                % the optimization process only search for the optimal value
                % of the rho, given beta is equal to 0.
                % Note: in this case values that can be taken by rho are all
                % positive values within the range of (0,1).
                [results(j,2),criterion(j)]=fminsearchbnd(@(param) HPZ_MME_Helper_Extreme_Param(param,endowment,subject_data,treatment,function_flag,aggregation,pref_class),initial_points(j,2), 0, 1, options);

            elseif beta_flag==2 && restricted_rho == false
                [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_MME_Helper(param,endowment,subject_data,treatment,function_flag,aggregation,pref_class),initial_points(j,:),[0 0], [], options);
            
            elseif beta_flag==2 && restricted_rho == true
                % if the subject has a corner solution(s) where CRRA is chosen 
                % as the utility function, then restrict rho<1 and beta can be
                % any number positive number [0, inf]
                [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_MME_Helper(param,endowment,subject_data,treatment,function_flag,aggregation,pref_class),initial_points(j,:),[0 0], [inf 1], options);

            elseif beta_flag==1 && restricted_rho == false
                [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_MME_Helper(param,endowment,subject_data,treatment,function_flag,aggregation,pref_class),initial_points(j,:),[-1 0], [], options);
                
            elseif beta_flag==1 && restricted_rho == true
                % if the subject has a corner solution(s) where CRRA is chosen 
                % as the utility function, then restrict rho<1 and beta can be
                % a negative number [-1, inf]
                [results(j,:),criterion(j)]=fminsearchbnd(@(param) HPZ_MME_Helper(param,endowment,subject_data,treatment,function_flag,aggregation,pref_class),initial_points(j,:),[-1 0], [inf 1], options);
            end

            % initialize the temporary minimum of fvals by using the objective 
            % value of the first initial point
%             if (j == 1)
%                 fval_temp_min = criterion(j);
%                 % initialize equal_fval_counter to 1
%                 equal_fval_counter = equal_fval_counter + 1;
%                 % update the estimation running time for the convergence point
%                 time_optim_conv_pnt(equal_fval_counter) = toc(t_optim);
% 
%                 optimal_parameter_matrix(equal_fval_counter, 1:2) = results(j,:);
%                 optimal_parameter_matrix(equal_fval_counter,   3) =  criterion(j);
%             end

            % check if the resulting criterion is equal to computed minimum
            % criterion or not AND number of minimal criterion are not greater 
            % than min_fvals (=3) AND j > 1
            if ( abs(fval_temp_min - criterion(j)) <= fval_threshold)
                % update the convergence counter
                equal_fval_counter = equal_fval_counter + 1;

                % store new minimum in the matrix
                optimal_parameter_matrix(equal_fval_counter, 1:2) = results(j,:);
                optimal_parameter_matrix(equal_fval_counter,   3) =  criterion(j);

                % update the minimum when the threshold is not achieved
                if(criterion(j) < fval_temp_min)
                    % update the temporary minimum of fvals
                    fval_temp_min = criterion(j);

                    % check the previous results to see if they are within the
                    % new acceptance range for criterion

                    % number of violations
                    mismatch_counter = 0;
                    % copy the current number of convergance points
                    temp_equal_fval_counter = equal_fval_counter;

                    % among all current convergance points:
                    for j_ind = 1 : equal_fval_counter
                        % check if they all are within new acceptance range
                        % if not:
                        if (optimal_parameter_matrix(j_ind,   3) > (fval_temp_min + fval_threshold))
                            % replace that element with inf
                            optimal_parameter_matrix(j_ind,   :) = Inf;
                            % update the value of temporary counter for
                            % convergance points
                            temp_equal_fval_counter = temp_equal_fval_counter - 1;
                            % update the number of violations
                            mismatch_counter = mismatch_counter + 1;
                        end
                        % Do nothing
                    end

                    % re-arrange the results vector:
                    % start index
                    start_ind = 1;
                    % end index
                    end_ind = equal_fval_counter;
                    % temporary value for swaping values
                    % temp_val = zeros(1,3);

                    % Go through the result vector as long as there is still
                    % violation
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
                            % move pointer forward
                            start_ind = start_ind + 1;

                        end

                    end

                    % update the equal_fval_counter
                    equal_fval_counter = temp_equal_fval_counter;

                end


            % check if we find a new minimum AND number of minimal criterion
            % are not greater than min_fvals (=3) AND j > 1 AND criterion(j) <
            % fval_temp_min 
            elseif (criterion(j) < fval_temp_min && abs(fval_temp_min - criterion(j)) > fval_threshold)
                % update minimum
                fval_temp_min = criterion(j);
                % update counter
                equal_fval_counter = 1;
                % update result matrix
                optimal_parameter_matrix = zeros(MME_min_counter, 3);

                % store new minimum in the matrix
                optimal_parameter_matrix(equal_fval_counter, 1:2) = results(j,:);
                optimal_parameter_matrix(equal_fval_counter,   3) =  criterion(j);

            end

            % if optimization is terminated
            if equal_fval_counter == MME_min_counter || j == max_starting_points
                % store the total number of used initial points
                total_init_points = j;
            end

        end
    
    end        
end

if MME_time == Inf

    optimal_parameter_matrix = sortrows(optimal_parameter_matrix(1:equal_fval_counter,:),3);
      
    if write_all_flag == 1
        
        max_index = 1;
        
    else
        
        max_index = equal_fval_counter;
        
    end
    
    final_output = zeros(max_index, 8);    
    
    for i = 1 : max_index  
         
        % [~,best_index]=min(criterion);
        final_output(i,1)=optimal_parameter_matrix(i, 1);
        final_output(i,2)=optimal_parameter_matrix(i, 2);

        % NLLS value using Euclidean metric
        final_output(i,3)=HPZ_Second_Criterion(final_output(i,1:2),observations,treatment,function_flag,zeros_flag,1,1,pref_class);
        % NLLS value using Choi et al. (2007) metric
        final_output(i,4)=HPZ_Second_Criterion(final_output(i,1:2),observations,treatment,function_flag,zeros_flag,2,1,pref_class);
        
        % MME values (Max Waste, Mean Waste, Sum of Squares Wastes)
        [final_output(i,5),final_output(i,6),final_output(i,7)]=HPZ_MME_Criterion(final_output(i,1:2),endowment,observations,treatment,function_flag,pref_class);

        [final_output(i,8)] = HPZ_BI_Criterion(final_output(i,1:2),endowment,observations,treatment,function_flag,pref_class);        

        %% finalize the output file for distinct results
        %action_flag = 3; %% MME
        %[fid_MME_final_report] = HPZ_Write_Result_File_Finalizer(action_flag, i , fid_MME_final_report, final_output, num2str(data(1,1)), output_flag_vec);
          
    end
end
% 
% [~,best_params_index] = min (final_output(:,3));
% 
% param_1 = final_output(best_params_index,1);
% 
% param_2 = final_output(best_params_index,2);

param_1 = optimal_parameter_matrix(1,1);

param_2 = optimal_parameter_matrix(1,2);

% keep track of the entire estimation process
%time_total = toc(t_total);

end