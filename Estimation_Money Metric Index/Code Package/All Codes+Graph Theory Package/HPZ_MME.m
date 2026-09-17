function [criterion] = HPZ_MME(param,endowment,observations,treatment,flag,pref)

% The function calculates the MME criterion per observation. Given a
% specific functional form and prices, we look for the lowest expenditure 
% level that yields at least the same level of utility as does the observed
% choice.
% param is a vector of two parameters - beta (the disappointment aversion
% parameter) and the utility function parameter (rho for CRRA and A for
% CARA).
% endowment is a column vector. The number of elements is the number 
% of observations. Each row is a number.
% observations is a matrix with obs_num rows and 4 columns.
% each row is one choice of the subject.
% The first column is the quantity of good 1 chosen by the subject.
% The second column is the quantity of good 2 chosen by the subject.
% The third column is the price of good 1. 
% The fourth column is the price of good 2. 
% treatmentis the number of treatment in CFGK (2007).
% flag is the type of utility function chosen for the prizes:
% 1 - CRRA.
% 2 - CARA.
% The function returns the value of the level of expenditure for 
% each observaion, therefore criterion is a vector with length equal to the 
% number of observations.

[num_obs,~]=size(observations);

options = optimset('Algorithm','interior-point','Display','off');

prices= observations(:,3:4);

if pref == 1
    if ((treatment==1)||(treatment==4))
        prob_x=1/2;    
    elseif treatment==2
        prob_x=1/3;
    elseif treatment==3
        prob_x=2/3;
    end
end

criterion=zeros(num_obs,1);

if pref == 1

if flag==1 % CRRA

	parfor i=1:num_obs
        % compute the utility level
       utility = HPZ_Utility (prob_x, observations(i,1:2), param(1),param(2), 0, flag);
            
       if ((observations(i,1) == 0 || observations(i,2) == 0))
           
           if param(2) >= 1 % then u is -inf, so minimal expenditure for the comparable utility level is 0
           
               criterion(i,1) = 0;
               
           else
           
                % if the subject has a corner solution(s) among its observations
                % and the CRRA is chosen as the utility function, then:
                % use the grid search to find the minimal expenditure for that
                % corner solution:
                max_x1=1/observations(i,3); % compute the intersection point (1/p1)
                max_x2=1/observations(i,4); % compute the intersection point (1/p2)
                % compute minimal expenditure using the grid search
                criterion(i,1) = HPZ_Grid_Search_MME (prob_x,param(1),param(2), 0,flag,max_x1,max_x2,utility,treatment,pref); 
                
           end
           
       else
           % if other observations of the subject (non-corner solutions)
           % use the numeric approach in order to find the minimal
           % expenditure:
           
           init_grid = zeros(2,5);
           
           x_1_init = endowment(i)/prices(i,1);
           
           % first point
           init_grid(1,1) = (102/(1000+1))*x_1_init;
           init_grid(1,2) = (endowment(i)-(init_grid(1,1)*prices(i,1)))/prices(i,2);
           [init_grid(1,3:4),init_grid(1,5), eflag1] = fmincon (@(x) prices(i,1:2)*x',init_grid(1,1:2),[],[],[],[],[0 0],[],@(x) HPZ_Utility_Constraint(prob_x,x,param(1),param(2),0,flag,utility), options);
         
           % second point
           init_grid(2,1) = (898/(1000+1))*x_1_init;
           init_grid(2,2) = (endowment(i)-(init_grid(2,1)*prices(i,1)))/prices(i,2);
           [init_grid(2,3:4),init_grid(2,5), eflag2] = fmincon (@(x) prices(i,1:2)*x',init_grid(2,1:2),[],[],[],[],[0 0],[],@(x) HPZ_Utility_Constraint(prob_x,x,param(1),param(2),0,flag,utility), options);
           
           if eflag1 ==1 && eflag2 ==1 % if FOC condition nearly holds, no more search 
               % compute minimal expenditure
               criterion(i,1) = min(init_grid(:,5));
           else
               max_x1=1/observations(i,3); % compute the intersection point (1/p1)
               max_x2=1/observations(i,4); % compute the intersection point (1/p2)
               % compute minimal expenditure using the grid search
               criterion(i,1) = HPZ_Grid_Search_MME (prob_x,param(1),param(2), 0,flag,max_x1,max_x2,utility,treatment,pref);                
           end

       end

	end
    
else
    %% CARA
    parfor i=1:num_obs
       % compute the utility level
       utility = HPZ_Utility (prob_x, observations(i,1:2), param(1), 0, param(2), flag);
       % use the numeric approach in order to find the minimal expenditure:
       % for both corner and non-corner solutions
       init_grid = zeros(2,5);
       
       x_1_init = endowment(i)/prices(i,1);
       
       % first point
       init_grid(1,1) = (102/(1000+1))*x_1_init;
       init_grid(1,2) = (endowment(i)-(init_grid(1,1)*prices(i,1)))/prices(i,2);
       [init_grid(1,3:4),init_grid(1,5), eflag1] = fmincon (@(x) prices(i,1:2)*x',init_grid(1,1:2),[],[],[],[],[0 0],[],@(x) HPZ_Utility_Constraint(prob_x,x,param(1),0, param(2),flag,utility), options);
       
       % second point
       init_grid(2,1) = (898/(1000+1))*x_1_init;
       init_grid(2,2) = (endowment(i)-(init_grid(2,1)*prices(i,1)))/prices(i,2);
       [init_grid(2,3:4),init_grid(2,5), eflag2] = fmincon (@(x) prices(i,1:2)*x',init_grid(2,1:2),[],[],[],[],[0 0],[],@(x) HPZ_Utility_Constraint(prob_x,x,param(1),0, param(2),flag,utility), options);
       
       if eflag1 ==1 && eflag2 ==1
           % compute minimal expenditure
           criterion(i,1) = min(init_grid(:,5));
       else
           max_x1=1/observations(i,3); % compute the intersection point (1/p1)
           max_x2=1/observations(i,4); % compute the intersection point (1/p2)
           % compute minimal expenditure using the grid search
           criterion(i,1) = HPZ_Grid_Search_MME (prob_x,param(1), 0, param(2),flag,max_x1,max_x2,utility,treatment,pref);
       end

    end
    
end

elseif pref == 2
   
if flag==1 % CES

	parfor i=1:num_obs
        % compute the utility level
       utility = HPZ_OR_Utility (observations(i,1:2), param(1),param(2), flag);

       % use the numeric approach in order to find the minimal
       % expenditure:
           
       init_grid = zeros(2,5);
           
       x_1_init = endowment(i)/prices(i,1);
           
       % first point
       init_grid(1,1) = (102/(1000+1))*x_1_init;
       init_grid(1,2) = (endowment(i)-(init_grid(1,1)*prices(i,1)))/prices(i,2);
       [init_grid(1,3:4),init_grid(1,5), eflag1] = fmincon (@(x) prices(i,1:2)*x',init_grid(1,1:2),[],[],[],[],[0 0],[],@(x) HPZ_OR_Utility_Constraint(x,param(1),param(2),flag,utility), options);
         
       % second point
       init_grid(2,1) = (898/(1000+1))*x_1_init;
       init_grid(2,2) = (endowment(i)-(init_grid(2,1)*prices(i,1)))/prices(i,2);
       [init_grid(2,3:4),init_grid(2,5), eflag2] = fmincon (@(x) prices(i,1:2)*x',init_grid(2,1:2),[],[],[],[],[0 0],[],@(x) HPZ_OR_Utility_Constraint(x,param(1),param(2),flag,utility), options);
           
       if eflag1 ==1 && eflag2 ==1
           % compute minimal expenditure
           criterion(i,1) = min(init_grid(:,5));
       else
           max_x1=1/observations(i,3); % compute the intersection point (1/p1)
           max_x2=1/observations(i,4); % compute the intersection point (1/p2)
           % compute minimal expenditure using the grid search
           criterion(i,1) = HPZ_Grid_Search_MME (prob_x,param(1),param(2), 0,flag,max_x1,max_x2,utility,treatment,pref);
       end

    end
    
end

end