function [choice_matrix] = HPZ_Choices_Analytical(observations,param,function_flag,pref)

% number of observations
[num_obs,~]=size(observations);

% set intersection pints from observation matrix
max_vals = 1./ observations(:,3:4);

% set observed bundle (x1,x2) from observation matrix
observed_bundle = observations(:,1:2);

% the output matrix initialization
choice_matrix=zeros(num_obs,2);

% in case of risk preferences
if pref == 1
    
% in case of CRRA
if function_flag==1
    
    % cannot use parfor any more since x2* computation depends on x1*
    for i=1:num_obs 
          
       % p = Max_Y / Max_X
       p = max_vals(i, 2) / max_vals(i,1);
       
       %% if rho is equal to 0, beta should be positive
%        if param(2) == 0 || abs(param(2)) <= 10^(-6)
%            if param(1) < 0
%                param(1) = abs(param(1));
%            end
%        end
              
       %% if Rho = 0 and Beta >= 0 CASE #4
       if param(1) >= 0 && ( param(2) == 0 || (param(2) > 0 && param(2) <= 10^(-6)))

           % if (1+Beta) < p 
           if p > (1 + param(1))
               choice_matrix(i,:) = [0, max_vals(i,2)]; % (0, max_y)
           % if p < 1 / (1 + Beta)
           elseif p < 1 / (1 + param(1))
               choice_matrix(i,:) = [max_vals(i,1), 0]; % (max_x, 0)
           % if (1/(Beta + 1)) < p < (Bet + 1)
           elseif p > 1 / (1 + param(1)) && p < (1 + param(1))
               choice_matrix(i,:) = [max_vals(i,2) / (p + 1), max_vals(i,2) / (p + 1)]; % (max_y/(p+1), max_y/(p+1))
           % P = (1/(Beta + 1))
           elseif p == 1
               if ( param(1) == 0 || (param(1) > 0 && param(1) <= 10^(-6)))
                    choice_matrix(i,:) = observed_bundle(i,:); % Error is 0
               else
                   mid_point = [max_vals(i,2) / (p + 1), max_vals(i,2) / (p + 1)]; % (max_y/(p+1), max_y/(p+1));
                   choice_matrix(i,:) = mid_point; % Erros is distance from the mid point
               end
           elseif p == 1 / (1 + param(1))
               % if x1 >= x2
               if observed_bundle(i,1) >= observed_bundle(i,2)
                   choice_matrix(i,:) = observed_bundle(i,:); % Error is 0
               else
                   % measure the error
                   mid_point = [max_vals(i,2) / (p + 1), max_vals(i,2) / (p + 1)]; % (max_y/(p+1), max_y/(p+1));
                   choice_matrix(i,:) = mid_point; % Erros is distance from the mid point
               end
           % p = (Bet + 1)
           elseif p == (1 + param(1))
               % if x1 <= x2
               if observed_bundle(i,1) <= observed_bundle(i,2)
                   choice_matrix(i,:) = observed_bundle(i,:);
               else
                   % measure the error
                   mid_point = [max_vals(i,2) / (p + 1), max_vals(i,2) / (p + 1)]; % (max_y/(p+1), max_y/(p+1));
                   choice_matrix(i,:) = mid_point; % Erros is distance from the mid point
               end
           end
           
       %% if Rho = 0 and Beta < 0 CASE #3b    
       elseif param(1) < 0 && ( param(2) == 0 || (param(2) > 0 && param(2) <= 10^(-6)))
           
           if p < 1
               choice_matrix(i,:) = [max_vals(i,1), 0];
           elseif p == 1
               if observed_bundle(i, 1)  >= observed_bundle(i,2)
                   choice_matrix(i,:) = [max_vals(i,1), 0];
               else
                   choice_matrix(i,:) = [0, max_vals(i,2)];
               end
           elseif p > 1       
               choice_matrix(i,:) = [0, max_vals(i,2)];
           end
           
       %% if Rho > 0 and Beta >= 0 CASE #1
       elseif param(1) >= 0 && param(2) > 0
           
           % if p < 1 / (1 + Beta)
           if p < 1 / (1 + param(1))
               % x1* = Max_x / [1 + (p*(1+Beta)^(1/rho))/p]
               choice_matrix(i,1) = (max_vals(i,1) / ( 1 + ( ((1+param(1))*p)^(1/param(2)) ) / p ) );
               % x2* = x1* *((1+Beta)*p)^(1/Rho)
               choice_matrix(i,2) = choice_matrix(i,1)*((1+param(1))*p)^(1/param(2));

           % if 1 + Beta <= p <= 1 / (1 + Beta)
           elseif p >= 1 / (1 + param(1)) && p <= 1 + param(1)
               % x1* = (max_y)/(p+1)
               choice_matrix(i,1) = (max_vals(i,2) ) / ( p + 1 ) ;
               % x2* = (max_y)/(p+1)
               choice_matrix(i,2) = (max_vals(i,2) ) / ( p + 1 ) ;
               
           % if p > 1 / (1 + Beta)
           elseif p > 1 + param(1)
               % x1* = (max_x)/(1 + ((p/(1+Beta))^(1/Rho))/p )
               choice_matrix(i,1) =  (max_vals(i,1) ) / ( 1 + ( ( p/(1+param(1)) )^(1/param(2)) )/ p ) ;
               % x2* = x1* *(p/(1+Beta))^(1/Rho)
               choice_matrix(i,2) =  choice_matrix(i,1) * ( ( p/(1+param(1)) )^(1/param(2)) );
               
           end

       
       %% Beta = -1 and Rho > 0 CASE #3a
       elseif param(2) > 0 && ( param(1) == -1 || abs(-1 - param(1) ) <= 10^(-6) )
           if p < 1
               choice_matrix(i,:) = [max_vals(i,1), 0];
           elseif p == 1
               if observed_bundle(i, 1)  >= observed_bundle(i,2)
                   choice_matrix(i,:) = [max_vals(i,1), 0];
               else
                   choice_matrix(i,:) = [0, max_vals(i,2)];
               end
           elseif p > 1       
               choice_matrix(i,:) = [0, max_vals(i,2)];
           end
       
       %% if Rho > 0 and Beta < 0 CASE #2
       elseif param(2) > 0 && ( param(1) > -1 && param(1) < 0 )
           % if p < 1
           if p < 1
               % x1* = Max_x / [1 + (p*(1+Beta)^(1/rho))/p]
               choice_matrix(i,1) = (max_vals(i,1) / ( 1 + ( ((1+param(1))*p) ^(1/param(2)) ) / p ) );
               % x2* = x1* *((1+Beta)*p)^(1/Rho)
               choice_matrix(i,2) = choice_matrix(i,1)*((1+param(1))*p)^(1/param(2));

           % if p = 1
           elseif p == 1
               % if x1 >= x2 => x1* must be greater or equal than x2*
               if observed_bundle(i, 1) >= observed_bundle(i, 2)

                   choice_matrix(i,1) = max_vals(i,1) / (1 + ( (1 + param(1))^(1/param(2)) ) );                   
                   choice_matrix(i,2) = max_vals(i,1) / (1 + ( (1 + param(1))^(-1/param(2)) ) );                   

               else
                   choice_matrix(i,1) = max_vals(i,1) / (1 + ( (1 + param(1))^(-1/param(2)) ) );                   
                   choice_matrix(i,2) = max_vals(i,1) / (1 + ( (1 + param(1))^(1/param(2)) ) );                   
               end
               
           % if p > 1 
           elseif p > 1
               % x1* = (max_x)/(1 + ((p/(1+Beta))^(1/Rho))/p )
               choice_matrix(i,1) =  (max_vals(i,1) ) / ( 1 + ( ( p/(1+param(1)) )^(1/param(2)) )/ p ) ;
               % x2* = x1* *(p/(1+Beta))^(1/Rho)
               choice_matrix(i,2) =  choice_matrix(i,1) * ( ( p/(1+param(1)) )^(1/param(2)) );
           end
           
       end
       
       if (isnan(choice_matrix(i,1)) || isnan(choice_matrix(i,1)))
   
           warning('A Nan when Rho is %2.10f, Beta is %f, P is %f and i is %d.',param(2),param(1),p,i);

       end       

    end
    
%% CARA
else
	beta = param(1);
	A  = param(2);
    for i=1:num_obs
        % p = Max_Y / Max_X
        p = max_vals(i, 2) / max_vals(i,1);
        
%         % A is zero
%         if A == 0 
%             
%             choice_matrix(i,:) = observed_bundle(i, :);
%         
        %% beta non-negative
%         elseif beta >= 0
        if beta >= 0
            
            if p < (exp(-A*max_vals(i,1))/(1+beta))
                choice_matrix(i,:) = [max_vals(i,1), 0];
            
            elseif p >= (exp(-A*max_vals(i,1))/(1+beta)) && p < (1 / (1+beta))
                choice_matrix(i,1) = (max_vals(i,2) - (log(p*(1+beta))) / A )/(1+p); 
                choice_matrix(i,2) = (max_vals(i,2) + (p*log(p*(1+beta))) / A )/(1+p); 
            
            elseif p >= (1 / (1+beta)) && p <= (1+beta)
                choice_matrix(i,:) = [max_vals(i,2)/(p+1), max_vals(i,2)/(p+1)];

            elseif p > (1+beta) && p <= (1+beta)*exp(A*max_vals(i,2))            
                choice_matrix(i,1) = (max_vals(i,2) - (log(p/(1+beta))) / A )/(1+p); 
                choice_matrix(i,2) = (max_vals(i,2) + (p*log(p/(1+beta))) / A )/(1+p); 
            
            elseif p > (exp(A*max_vals(i,2))*(1+beta))
                choice_matrix(i,:) = [0,max_vals(i,2)];
            end
        
        %% negative beta
        elseif beta > -1 && beta < 0
            if p < (exp(-A*max_vals(i,1))/(1+beta))
                choice_matrix(i,:) = [max_vals(i,1), 0];
            
            elseif p >= (exp(-A*max_vals(i,1))/(1+beta)) && p < 1
                choice_matrix(i,1) = (max_vals(i,2) - (log(p*(1+beta))) / A )/(1+p); 
                choice_matrix(i,2) = (max_vals(i,2) + (p*log(p*(1+beta))) / A )/(1+p); 
            
            elseif p == 1
               % if x1 >= x2 => x1* must be greater or equal than x2*
               if observed_bundle(i, 1) >= observed_bundle(i, 2)                  
                   choice_matrix(i,1) = (max_vals(i,2) - log(1+beta) / A )/2; 
                   choice_matrix(i,2) = (max_vals(i,2) + log(1+beta) / A )/2; 
               else
                   choice_matrix(i,1) = (max_vals(i,2) + log(1+beta) / A )/2; 
                   choice_matrix(i,2) = (max_vals(i,2) - log(1+beta) / A )/2; 
               end

            elseif p > 1 && p <= (1+beta)*exp(A*max_vals(i,2))            
                choice_matrix(i,1) = (max_vals(i,2) - (log(p/(1+beta))) / A )/(1+p); 
                choice_matrix(i,2) = (max_vals(i,2) + (p*log(p/(1+beta))) / A )/(1+p); 
            
            elseif p > (exp(A*max_vals(i,2))*(1+beta))
                choice_matrix(i,:) = [0,max_vals(i,2)];
            end
        
        %% beta equal to -1
        elseif beta == -1
            if p < 1
                choice_matrix(i,:) = [max_vals(i,1), 0];
            elseif p == 1
                if observed_bundle(i, 1) >= observed_bundle(i, 2)
                    choice_matrix(i,:) = [max_vals(i,1), 0];
                else
                    choice_matrix(i,:) = [0, max_vals(i,2)];
                end
            elseif p > 1
                choice_matrix(i,:) = [0, max_vals(i,2)];
            end
            
        end %% end of conditions on beta

       if (isnan(choice_matrix(i,1)) || isnan(choice_matrix(i,1)))
   
           warning('A Nan when A is %f, Beta is %f, P is %f and i is %d.',param(2),param(1),p,i);

       end       
        
        
    end %% end of for
    
end %% end of CARA

% in case of other regarding preferences
elseif pref == 2
    
% in case of CES
if function_flag==1
    
    % cannot use parfor any more since x2* computation depends on x1*
    for i=1:num_obs 
          
       % p = Max_Y / Max_X
       p = max_vals(i, 2) / max_vals(i,1);
       
       % alpha / (1 - alpha)
       alpha_ratio = param(1)/(1-param(1));
       
       % Elasticity of substitution
       EOS = 1 / (1 - param(2));
       
        if param(1) <= 10^(-6)
           
           % Case 1 
           choice_matrix(i,:) = [0, max_vals(i,2)]; % (0, max_y)
           
        elseif param(1) >= (1-10^(-6))

           % Case 2 
           choice_matrix(i,:) = [max_vals(i,1), 0]; % (max_x, 0)
            
        elseif (abs(param(2)) <= 10^(-6))

           % Case 4 
           choice_matrix(i,:) = [param(1) * max_vals(i,1),  (1 - param(1)) * max_vals(i,2)]; 

        elseif ((param(2) <= (1 + 10^(-6))) && (param(2) >= (1 - 10^(-6))))

           % Case 5 
           if p > alpha_ratio
               
               choice_matrix(i,:) = [0, max_vals(i,2)];
                
           elseif p < alpha_ratio
               
               choice_matrix(i,:) = [max_vals(i,1), 0]; 
               
           else
               
               choice_matrix(i,:) = observed_bundle;
               
           end
           
        elseif param(2) < (1 - 10^(-6))

            base = (p * alpha_ratio) ^ EOS ; 

            % Case 6 
            choice_matrix(i,:) = [max_vals(i,2) \ (p + base), max_vals(i,2) \ (1 + (p / base))];  
            
        elseif param(2) > (1 + 10^(-6))
            
           % Case 7 
           if p > alpha_ratio
               
               choice_matrix(i,:) = [0, max_vals(i,2)];
                
           elseif p < alpha_ratio
               
               choice_matrix(i,:) = [max_vals(i,1), 0]; 
               
           elseif observed_bundle(i,1) > observed_bundle(i,2)
               
               choice_matrix(i,:) = [max_vals(i,1), 0];
               
           else
               
               choice_matrix(i,:) = [0, max_vals(i,2)];               
               
           end            
           
       end
              
       if (isnan(choice_matrix(i,1)) || isnan(choice_matrix(i,1)))
   
           warning('A Nan when Rho is %2.10f, Alpha is %f, P is %f and i is %d.',param(2),param(1),p,i);

       end       

    end

end %% end of pref_class

end % end of the Choices function