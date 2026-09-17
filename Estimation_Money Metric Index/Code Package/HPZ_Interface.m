function HPZ_Interface
% This file collects the information on the required estimation and reports
% the results to the user.

% "True" means that the estimation calculation is numeric and "False" means
% analytic estimation
global numeric_flag %#ok<*NUSED>
%
global output_flag_vec
%
global fid_BI_final_report
%
global fid_MME_final_report
%
global fid_NLLS_final_report
%
global fid_VI_final_report
% "True" value of this boolean flag restricts beta to be equal to zero
global beta_zero_flag
% The max number of convergance points MME
global MME_min_counter;
% The max number of convergance points NLLS
global NLLS_min_counter;
% The max number of convergance points BI
global BI_min_counter;
% Time (minutes) that user want to spend on running the code on his machine
global MME_max_time_estimation
%
global NLLS_max_time_estimation
%
global BI_max_time_estimation
%
global NLLS_str
%
global MME_str
%
global BI_str

global NLLS_residuals_str

global MME_residuals_str

global BI_residuals_str

global do_not_write

warning('off','MATLAB:xlswrite:AddSheet');

% Select a data set. This code accomodates choi et al. (2007) only. 
data_set=0;
   
while ~((data_set==1)||(data_set==2)||(data_set==3)||(data_set==4)||(data_set==5))

     data_set = listdlg('PromptString','Dataset Selection',...
                'SelectionMode','single', 'ListString',{'choi et al. (2007)',...
                'Kurtz et al. (2016)','Halevy et al. (2016)','daegu (2016)','daegu (2016)_sim'},...
                'Name','Dataset Selection','ListSize',[500 50],'uh',30,'fus',8,'ffs',8);
end

% Additional data sets can be added easily as long as the data is arranged 
% in an equivalent manner. 
% data_set = 1;

%% Set treatment to be symmetric
treatment = 1;

% Extracting the data of Choi et. al (2007)

if (data_set==1)
    
    data_matrix = HPZ_CFGK_2007_Format_Data (treatment);
    
    pref_class = 1;
    
elseif (data_set==2)
    
    [data_matrix,~] = HPZ_KLP_2016_Format_Data ();
    
    pref_class = 2;    
    
elseif (data_set==3)
    
    data_matrix = HPZ_HZ_2016_Format_Data ();
    
    pref_class = 1;    

elseif (data_set==4)

    data_matrix = Daegu_2016_Format_Data ();    

    pref_class = 1;
    
elseif (data_set==5)
    
    data_matrix = Daegu_2016_Format_Data_Sim ();    

    pref_class = 1;
    
end    

% At this point we assume that data_matrix has a specific structure:
% The matrix has six columns (We assume that the endowment is fixed at 1):
% The first column is the subject ID.
% The second column is the observation number - 50 observations per subject
% The third column is the quantity of good 1 chosen by the subject.
% The fourth column is the quantity of good 2 chosen by the subject.
% The fifth column is the price of good 1. 
% The sixth column is the price of good 2. 

% list of subjects' numbers in the required treatment

[rows,~] = size(data_matrix);

subjects = unique(data_matrix(:,1));

num_subjects = length(subjects);

first_obs_row = zeros(num_subjects,1);

counter_subjects = 1;

sub_temp = subjects(1) ;

min_obs = zeros(rows,1);

min_obs(1,1) = data_matrix(1,2);

for i=2:rows
    
    if data_matrix(i,1) == sub_temp
        
        min_obs(i) = min_obs(i-1);
   
    else
        min_obs(i) = data_matrix(i,2);
        
        sub_temp = data_matrix(i);
        
    end
end

for i=1:rows
   
    if data_matrix(i,2) == min_obs(i)
        
        first_obs_row (counter_subjects) = i;
        
        counter_subjects = counter_subjects + 1;
        
    end
           
end

subjects_str = num2str(subjects);

action=0;
   
while ~((action==1)||(action==2)||(action==3)||(action==4))

     action = listdlg('PromptString','Action Selection',...
                'SelectionMode','single', 'ListString',{'Consistency Tests and Inconsistency Indices',...
                'Nonlinear Least Squares',...
                'Money Metric Index Method',...
                'Binary Index Method'},...
                'Name','Action Selection','ListSize',[500 100],'uh',30,'fus',8,'ffs',8);
end

legal_assignment=0;
   
while ~(legal_assignment==1)
    
    legal_assignment=1;
    
    subjects_index = listdlg('PromptString','Subjects Selection (the same action applies to all)',...
                'SelectionMode','multiple', 'ListString',subjects_str,...
                'Name','Subjects','ListSize',[400 600],'uh',30,'fus',8,'ffs',8);
    
    chosen_subjects_num = length(subjects_index);
            
   % for i=1:chosen_subjects_num

      %  if isempty(find(subjects==data_matrix(first_obs_row(subjects_index(i)),1), 1))
            
       %    legal_assignment=0;
            
      %  end
            
   % end
end

%obs_num = length(unique(data_matrix(:,2)));

%% Setting the result files
if action == 1 % Index
    VI_str = strcat('Indices-Results-', date,'-' ,num2str(floor(now) + floor(rand*10^5)));
    fid_VI_final_report = fopen(strcat(VI_str, '.csv'), 'w');
    %% finalize the output file for distinct results
    fprintf(fid_VI_final_report, '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n', ...
        'WARP_Violations', '#Pairs', ...
        'GARP_Violations', '#Pairs', ...
        'SARP_Violations', '#Pairs', ...
        'AFRIAT_Index', ...
        'VARIAN_Index_Min', 'VARIAN_Index_Mean', 'VARIAN_Index_AVG(SSQ)', 'Is exact',...
        'Houtman-Maks-Index', 'Is exact',...
        'Subject');
end

% if estimation was chosen we need some more information
if ((action==2)||(action==3)||(action==4))    
%    pref_class = 1; % Risk Behavior
    if pref_class == 1
        % the following is specific information needed for the estimation 
        % of Choi et al. (2007) data set.
        [function_flag, beta_flag, zeros_flag] = HPZ_Interface_Functional_form_settings();
    elseif pref_class == 2
        % the following is specific information needed for the estimation 
        % of Kurtz et al. (2016) data set.
        beta_flag = 1; 
        zeros_flag = 1;
        [function_flag] = HPZ_Interface_Functional_form_settings_OR();        
    end
    
	% If estimation was chosen we need some more information
    % Aggregation_flag determines the objective function for the estimation of
    % parameters. Whatever the choice is, all other aggregation indcies 
    % can be reported in the output file if we choose to 
    % We can use parallel tool box only when taking numerical approach,
    % not the analytical one
    
	[aggregation_flag, metric_flag, parallel_flag] = HPZ_Interface_Optimization_settings(action);
      
    % set corners:
    if metric_flag == 2 % CFGK
        zeros_flag = 2;
    end
    
    [bootstrap_flag,file_val_str] = HPZ_output_file_format(aggregation_flag, metric_flag);
    
    if bootstrap_flag == 1
        
        if ((numeric_flag == false) && ((action == 2) || (action == 3)))

            number_of_samples = 1000;
            
        else

            number_of_samples = 100;
            
        end
        
    end
%     file_val_str = {'Value', 'Value', 'Value', 'Value', 'Value', 'Value'};
%     if metric_flag ~= 0
%         file_val_str{metric_flag} = 'Criterion';
%     elseif aggregation_flag ~= 0
%         file_val_str{aggregation_flag + 2} = 'Criterion';
%     else
%         file_val_str{6} = 'Criterion';
%     end

    [residual_flag,in_sample_flag,out_sample_flag] = HPZ_Interface_Residual_calc_settings;

    HPZ_Write_Result_File_Initializer(action, function_flag, file_val_str, pref_class,bootstrap_flag);
            
      %% Set matlabpool (Parallel Computing)
      if parallel_flag
          % if parallel flag is on
          
          % close all other session
          %matlabpool close force local;
          
          % use all resources
          %matlabpool;
          
          % parpool replaced matlabpool in the 2013 version
          
          % close all other session          
          poolobj = gcp('nocreate');
          delete(poolobj);

          % use all resources
          parpool;

          
      end
end


for i=1:chosen_subjects_num
    
    if subjects_index(i) < num_subjects
  
        data = data_matrix(first_obs_row(subjects_index(i)):((first_obs_row(subjects_index(i)+1))-1),:);
        
        obs_num = (first_obs_row(subjects_index(i)+1))-(first_obs_row(subjects_index(i)));
        
    else
        
        data = data_matrix(first_obs_row(subjects_index(i)):rows,:);
        
        obs_num = data_matrix(rows,2);
        
    end

  if action==1
  
    Choices(:,1:2)=data(1:obs_num,3:4);
    
    Choices(:,3:4)=data(1:obs_num,5:6);
    
    % the function can return the disaggregated matrices of violations, we
    % dont report it.
    [~,~,~,~,~,~,~,~,~,VIO_PAIRS,VIOLATIONS,AFRIAT,VARIAN,Var_exact,HM,HM_exact] = HPZ_Subject_Consistency (Choices);

	fprintf(fid_VI_final_report, '%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n', ...
                                      num2str(VIOLATIONS(1)), num2str(VIO_PAIRS(1)), ...
                                      num2str(VIOLATIONS(2)), num2str(VIO_PAIRS(2)), ....
                                      num2str(VIOLATIONS(3)), num2str(VIO_PAIRS(3)), ...
                                      num2str(AFRIAT), ...
                                      num2str(VARIAN(1)), num2str(VARIAN(2)), num2str(VARIAN(3)), num2str (Var_exact),...
                                      num2str(HM), num2str (HM_exact), ...
                                      num2str(data(1,1)));
                                  
     clear Choices;
                                  
  end
  
  if action==2
    % Perform the estimation NLLS
    do_not_write = 0; %#ok<NASGU>
    
    asymmetric_flag = 1; % As is
    
    [param_1,param_2,final_output] = HPZ_NLLS (data,obs_num,treatment,function_flag,beta_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class);

    [rows_out,~] = size(final_output);
    
    if bootstrap_flag == 1
        
       [me_param_1,me_param_2,se_param_1,se_param_2,lower_95_percent_param_1,lower_95_percent_param_2,upper_95_percent_param_1,upper_95_percent_param_2] = HPZ_Bootstrap_SE(data,treatment,number_of_samples,action,function_flag,beta_flag,zeros_flag,metric_flag,aggregation_flag,asymmetric_flag,pref_class);
 
       me_1 = me_param_1*ones(rows_out,1);

       me_2 = me_param_2*ones(rows_out,1);       
       
       se_1 = se_param_1*ones(rows_out,1);

       se_2 = se_param_2*ones(rows_out,1);       

       le_1 = lower_95_percent_param_1*ones(rows_out,1);

       le_2 = lower_95_percent_param_2*ones(rows_out,1);       

       ue_1 = upper_95_percent_param_1*ones(rows_out,1);

       ue_2 = upper_95_percent_param_2*ones(rows_out,1);       
       
       final_output_new = [final_output,me_1,me_2,se_1,se_2,le_1,le_2,ue_1,ue_2];
       
    else
        
       final_output_new = final_output;        
       
    end
    
    for j=1:rows_out
    
        [fid_NLLS_final_report] = HPZ_Write_Result_File_Finalizer(action, j , fid_NLLS_final_report, final_output_new, num2str(data(1,1)), output_flag_vec,bootstrap_flag);
        
    end
    
    do_not_write = 1;

    if residual_flag == 1
        
        Mat = HPZ_NLLS_residuals (data,obs_num,treatment,function_flag,beta_flag,zeros_flag,metric_flag,asymmetric_flag,in_sample_flag,out_sample_flag,param_1,param_2,pref_class);
        
        status = xlswrite(NLLS_residuals_str,Mat,i);

        if status == 0 
            
           msgbox('Problem in writing to the residuals file');
            
        end
        
    end
    
  end
  
  if action==3
      
    do_not_write = 0;   %#ok<NASGU>
    
    asymmetric_flag = 1; % As is
    
    % Perform the estimation MME
    [param_1,param_2,final_output] = HPZ_MME_Estimation (data,obs_num,treatment,function_flag,beta_flag,zeros_flag,aggregation_flag,pref_class);  

    [rows_out,~] = size(final_output);
    
    if bootstrap_flag == 1
        
       [me_param_1,me_param_2,se_param_1,se_param_2,lower_95_percent_param_1,lower_95_percent_param_2,upper_95_percent_param_1,upper_95_percent_param_2] = HPZ_Bootstrap_SE(data,treatment,number_of_samples,action,function_flag,beta_flag,zeros_flag,metric_flag,aggregation_flag,asymmetric_flag,pref_class);

       me_1 = me_param_1*ones(rows_out,1);

       me_2 = me_param_2*ones(rows_out,1);       
       
       se_1 = se_param_1*ones(rows_out,1);

       se_2 = se_param_2*ones(rows_out,1);       

       le_1 = lower_95_percent_param_1*ones(rows_out,1);

       le_2 = lower_95_percent_param_2*ones(rows_out,1);       

       ue_1 = upper_95_percent_param_1*ones(rows_out,1);

       ue_2 = upper_95_percent_param_2*ones(rows_out,1);       
       
       final_output_new = [final_output,me_1,me_2,se_1,se_2,le_1,le_2,ue_1,ue_2];
       
    else
        
       final_output_new = final_output;        
       
    end

    for j=1:rows_out
    
        [fid_MME_final_report] = HPZ_Write_Result_File_Finalizer(action, j , fid_MME_final_report, final_output_new, num2str(data(1,1)), output_flag_vec,bootstrap_flag);

    end
    
    do_not_write = 1;
   
     % residual means waste (endowment-criterion)
     % in-sample redidual utilizes parameters from all observations while
     % out-sample does from other observations excluding obs i itself
     
    if residual_flag == 1
        
        Mat = HPZ_MME_residuals (data,obs_num,treatment,function_flag,beta_flag,zeros_flag,in_sample_flag,out_sample_flag,param_1,param_2,aggregation_flag,pref_class);
        
        status = xlswrite(MME_residuals_str,Mat,i);
        
        if status == 0 
            
           msgbox('Problem in writing to the residuals file');
            
        end
        
    end

    
  end
  
    if action==4
      
    do_not_write = 0;   %#ok<NASGU>
    
    asymmetric_flag = 1; % As is
    
    % Perform the estimation MME
    [param_1,param_2,final_output] = HPZ_BI_Estimation (data,obs_num,treatment,function_flag,beta_flag,zeros_flag,pref_class);  

    [rows_out,~] = size(final_output);
    
    if bootstrap_flag == 1
        
       [me_param_1,me_param_2,se_param_1,se_param_2,lower_95_percent_param_1,lower_95_percent_param_2,upper_95_percent_param_1,upper_95_percent_param_2] = HPZ_Bootstrap_SE(data,treatment,number_of_samples,action,function_flag,beta_flag,zeros_flag,metric_flag,aggregation_flag,asymmetric_flag,pref_class);
    
       me_1 = me_param_1*ones(rows_out,1);

       me_2 = me_param_2*ones(rows_out,1);       
       
       se_1 = se_param_1*ones(rows_out,1);

       se_2 = se_param_2*ones(rows_out,1);       

       le_1 = lower_95_percent_param_1*ones(rows_out,1);

       le_2 = lower_95_percent_param_2*ones(rows_out,1);       

       ue_1 = upper_95_percent_param_1*ones(rows_out,1);

       ue_2 = upper_95_percent_param_2*ones(rows_out,1);       
       
       final_output_new = [final_output,me_1,me_2,se_1,se_2,le_1,le_2,ue_1,ue_2];
       
    else
        
       final_output_new = final_output;        
       
    end
    
    for j=1:rows_out
    
        [fid_BI_final_report] = HPZ_Write_Result_File_Finalizer(action, j , fid_BI_final_report, final_output_new, num2str(data(1,1)), output_flag_vec,bootstrap_flag);

    end    
    
    do_not_write = 1;
    
    if residual_flag == 1
        
        Mat = HPZ_BI_residuals (data,obs_num,treatment,function_flag,beta_flag,zeros_flag,in_sample_flag,out_sample_flag,param_1,param_2,pref_class);
        
        status = xlswrite(BI_residuals_str,Mat,i);
        
        if status == 0 
            
           msgbox('Problem in writing to the residuals file');
            
        end
        
    end
    
    end
  
disp([num2str(i),' obs done out of ',num2str(chosen_subjects_num)])
end

    if action==1 % VI
        % close the result file
        fclose(fid_VI_final_report);
        % Address the result file
        msgbox(strcat('The result file is saved as: ',VI_str,'.csv',' under the following path:  ', pwd), 'Output File Path','help');
    end

    if action==2 % NLLS
        % close the result file
        fclose(fid_NLLS_final_report);
        % Address the result file
        
        if residual_flag == 1
            msgbox(strcat('Both files are saved under the following path:  ', pwd), 'Output File Path','help');
        else 
            msgbox(strcat('The result file is saved as: ',NLLS_str,'.csv',' under the following path:  ', pwd), 'Output File Path','help');
        end
    end
    
    if action==3 % MME
        % close the result file
        fclose(fid_MME_final_report);
        % Address the result file
        if residual_flag == 1
            msgbox(strcat('Both files are saved under the following path:  ', pwd), 'Output File Path','help');
        else 
            msgbox(strcat('The result file is saved as: ',MME_str,'.csv',' under the following path:  ', pwd), 'Output File Path','help');
        end
    end
    
    if action==4 % MME
        % close the result file
        fclose(fid_BI_final_report);
        % Address the result file
        if residual_flag == 1
            msgbox(strcat('Both files are saved under the following path:  ', pwd), 'Output File Path','help');
        else 
            msgbox(strcat('The result file is saved as: ',BI_str,'.csv',' under the following path:  ', pwd), 'Output File Path','help');
        end
    end    
end