function [choices,criterion] = HPZ_Grid_Search_Choices (prob_x,beta,rho,A,flag,max_x1,max_x2)

jump=0.01;

ratio=max_x2/max_x1;

inv_ratio=max_x1/max_x2;

Values_x1=zeros(floor(max_x1*100)+2,1);

Values_x2=zeros(floor(max_x2*100)+2,1);

num_cases_1 = length (Values_x1);

num_cases_2 = length (Values_x2);

grid=zeros(num_cases_1+num_cases_2-1,3);

for i=1:num_cases_1-1 
    
   grid(i,1) = 0+(i-1)*jump; 
   
   grid(i,2) = max_x2-ratio*grid(i,1);
   
   grid(i,3) = HPZ_Utility_Helper(prob_x,grid(i,1:2),beta,rho,A,flag);

end

grid(num_cases_1,1) = max_x1;

grid(num_cases_1,2) = 0;
   
grid(num_cases_1,3) = HPZ_Utility_Helper(prob_x,grid(num_cases_1,1:2),beta,rho,A,flag);

for j=1:num_cases_2-2 
    
   grid(j+num_cases_1,2) = 0+(j-1)*jump; 
   
   grid(j+num_cases_1,1) = max_x1-inv_ratio*grid(j+num_cases_1,2);
   
   grid(j+num_cases_1,3) = HPZ_Utility_Helper(prob_x,grid(j+num_cases_1,1:2),beta,rho,A,flag);

end

grid(num_cases_1+num_cases_2-1,1) = 0;

grid(num_cases_1+num_cases_2-1,2) = max_x2;
   
grid(num_cases_1+num_cases_2-1,3) = HPZ_Utility_Helper(prob_x,grid(num_cases_1+num_cases_2-1,1:2),beta,rho,A,flag);

[criterion,row] = min(grid(:,3));

if criterion==0
    disp('oops - HPZ_Grid_Search_Choices');
end
    
choices=grid(row,1:2);

end