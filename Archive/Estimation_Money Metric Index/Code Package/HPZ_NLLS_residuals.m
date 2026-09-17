function Mat = HPZ_NLLS_residuals (data,obs_num,treatment,function_flag,beta_flag,zeros_flag,metric_flag,asymmetric_flag,in_sample_flag,out_sample_flag,param_1,param_2,pref_class)
               
% data is a matrix with six columns (We assume that the endowment is fixed at 1):
% The first column is the subject ID.
% The second column is the observation number - 50 observations per subject
% The third column is the quantity of good 1 chosen by the subject.
% The fourth column is the quantity of good 2 chosen by the subject.
% The fifth column is the price of good 1. 
% The sixth column is the price of good 2. 
% obs_num is the number of observations per subject.
% treatment is the number of treatment in CFGK (2007).
% function_flag is the type of utility function chosen for the prizes:
% 1 - CRRA.
% 2 - CARA.
global numeric_flag

if zeros_flag==2 % Choi et al. (2007) correction
    
    % Choi et al. (2007) correction is applied for corner choices
    subject_data = HPZ_No_Corners (data,obs_num,1);
    
else
    
    subject_data = data;

end

if in_sample_flag == 1 && out_sample_flag == 1
    
    Mat = zeros (obs_num,8);

elseif in_sample_flag == 0 && out_sample_flag == 1
    
    Mat = zeros (obs_num,7);
    
elseif in_sample_flag == 1 && out_sample_flag == 0 
    
    Mat = zeros (obs_num,5);
    
end

Choices(:,1:2)=subject_data(1:obs_num,3:4);
    
Choices(:,3:4)=subject_data(1:obs_num,5:6);

expenditure = (Choices(:,1)*Choices(:,3)' + Choices(:,2)*Choices(:,4)')';

endowments = diag(expenditure);

param = [param_1 param_2]; 

if in_sample_flag == 1
    
    for i=1:obs_num
       
        Mat(i,1) = subject_data(1,1);
        
        Mat(i,2:3) = param;

        Mat(i,4) = i;
        
        observed_choices = Choices(i,1:2);
            
        if (numeric_flag)
            
            % numeric
            
            predicted_choices = HPZ_Choices (Choices(i,3:4),endowments(i),treatment,function_flag,asymmetric_flag,param,pref_class);

        else
            
            predicted_choices = HPZ_Choices_Analytical(Choices(i,1:4),param,function_flag,pref_class);

        end
        
        if (metric_flag==1)
    
            Mat (i,5) = HPZ_Euclid_Criterion(observed_choices,predicted_choices);
   
        else
    
            Mat (i,5) = HPZ_ldr_Criterion(observed_choices,predicted_choices);
                   
        end
            
    end
    
end
    
if out_sample_flag == 1
            
    for i=1:obs_num
        
        if in_sample_flag == 0 
       
            Mat(i,1) = subject_data(1,1);
        
            Mat(i,2:3) = param;

            Mat(i,4) = i;

        end
        
        if i < obs_num
           
            adjusted_data = [subject_data(1:(i-1),:) ; subject_data(i+1:obs_num,:)];
            
        elseif i == obs_num
            
            adjusted_data = subject_data(1:(obs_num-1),:);
            
        end
        
        [p_1,p_2] = HPZ_NLLS (adjusted_data,(obs_num-1),treatment,function_flag,beta_flag,zeros_flag,metric_flag,asymmetric_flag,pref_class);

        p_12 = [p_1 p_2];
        
        if in_sample_flag == 0 
       
            Mat(i,5) = p_1;
        
            Mat(i,6) = p_2;
            
        else
            
            Mat(i,6) = p_1;
        
            Mat(i,7) = p_2;
            
        end

        observed_choices = Choices(i,1:2);

        if (numeric_flag)
            
            % numeric
            
            predicted_choices = HPZ_Choices (Choices(i,3:4),endowments(i),treatment,function_flag,asymmetric_flag,p_12,pref_class);
            
        else
            
            predicted_choices = HPZ_Choices_Analytical(Choices(i,1:4),p_12,function_flag,pref_class);
            
        end
        
        if (metric_flag==1)
                
           if in_sample_flag == 0 
       
              Mat (i,7) = HPZ_Euclid_Criterion(observed_choices,predicted_choices);
               
           else
            
              Mat (i,8) = HPZ_Euclid_Criterion(observed_choices,predicted_choices);
            
          end      
                
       else
    
          if in_sample_flag == 0 
       
             Mat (i,7) = HPZ_ldr_Criterion(observed_choices,predicted_choices);
               
          else
            
             Mat (i,8) = HPZ_ldr_Criterion(observed_choices,predicted_choices);
            
          end                      
    
       end
            
    end
    
end

