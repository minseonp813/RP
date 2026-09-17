function [me_param_1,me_param_2,se_param_1,se_param_2,lower_95_percent_param_1,lower_95_percent_param_2,upper_95_percent_param_1,upper_95_percent_param_2] = HPZ_Bootstrap_SE(data,treatment,bs_samples,method_flag,function_flag,beta_flag,zeros_flag,metric_flag,aggregation_flag,asymmetric_flag,pref_class)
                 
%data - n x 6 matrix formatting as [subject obs x y p_x p_y]

%bs_samples - (scalar) number of bootsrap samples

% method_flag - 1 for NLLS, 2 for MMI, 3 for BI

obs_num = size(data,1);

parameters = zeros(bs_samples,2);

% recover parameters from actual data

%re-sampling w/ replacement 

sample_indices = ceil(obs_num*rand(obs_num,bs_samples));

for i=1:bs_samples
    
   bs_sample = data(sample_indices(:,i),:);
   
   % recover and store parameters from bs_sample 
    
   if method_flag == 2

       [param_1,param_2,~] = HPZ_NLLS (bs_sample,obs_num,treatment,function_flag,beta_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class);

   elseif method_flag == 3

       [param_1,param_2,~] = HPZ_MME_Estimation (bs_sample,obs_num,treatment,function_flag,beta_flag,zeros_flag,aggregation_flag,pref_class);
                             
   elseif method_flag == 4

       [param_1,param_2,~] = HPZ_BI_Estimation (bs_sample,obs_num,treatment,function_flag,beta_flag,zeros_flag,pref_class);  
                             
    end
    
    parameters(i,:) = [param_1 param_2];   
    
end

me = mean(parameters);

se = std(parameters);

me_param_1 = me(1,1);

me_param_2 = me(1,2);

se_param_1 = se(1,1);

se_param_2 = se(1,2);

ordered_param_1 = sort (parameters(:,1));

ordered_param_2 = sort (parameters(:,2));

lower = ceil(0.05*bs_samples);

upper = ceil(0.95*bs_samples);

lower_95_percent_param_1 = ordered_param_1 (lower);

lower_95_percent_param_2 = ordered_param_2 (lower);

upper_95_percent_param_1 = ordered_param_1 (upper);

upper_95_percent_param_2 = ordered_param_2 (upper);
