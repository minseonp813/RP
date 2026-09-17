function [criterion] = HPZ_MME_Analytical(observations,param,function_flag,pref_class)

% number of observations
[num_obs,~]=size(observations);

% set prices from observation matrix
prices= observations(:,3:4);

% set observed bundle (x1,x2) from observation matrix
observed_bundle = observations(:,1:2);

% initialization of criterion matrix
criterion=zeros(num_obs,1);

if pref_class == 1

    if function_flag==1 % CRRA

        for i=1:num_obs

            % p = Max_Y / Max_X => p = p1 / p2
            p = prices(i, 1) / prices(i,2);
       
            min_x  = min (observed_bundle(i, 1), observed_bundle(i, 2));
            max_x  = max (observed_bundle(i, 1), observed_bundle(i, 2));
       
            if param(2) < 0.008
                
                rho=0;
                param(2)=0;
       
            else
                
                rho  = param(2);
            
            end
            
            beta = param(1);
       
            %u_0 is (1-rho)*(2+beta)*u(x_0,y_0)
            u_0 = (max_x^(1-rho)) + (1+beta)*(min_x^(1-rho));
       
            %u is u(x_0,y_0)
            u = (1-rho)*(2+beta)*u_0;
       
            %% if Rho = 1 and Beta >=0,  CASE 7
            if (abs(param(2)-1)) < 10^(-6) && param(1) >= 0
           
                %% p < 1/1+Beta
                if  p < 1/(1+param(1))
                
                    % compute the optimal expenditure
                    criterion(i,1) = (2+beta)*((((prices(i,1)^(1/(1+beta)))*prices(i,2))/(1+beta))^((1+beta)/(2+beta)))*exp(u);
           
                end
           
                %% 1/1+Beta < p < 1+ Beta
                if p >= 1/(1+param(1)) && p <= (1+ param(1))
               
                    % compute the optimal expenditure
                    criterion(i,1) = (prices(i,1) + prices(i,2)) * exp(u);
           
                end
           
                %% p > 1 + Beta           
                if p > (1+ param(1))
                
                    % compute the optimal expenditure
                    criterion(i,1) = (2+beta)*((((prices(i,2)^(1/(1+beta)))*prices(i,1))/(1+beta))^((1+beta)/(2+beta)))*exp(u);
           
                end
           
            %% if Rho > 0 and Beta >=0, CASE 6
            elseif param(2) > 0 && param(1) >= 0
           
           
                %% p < 1/1+Beta
                if  p < 1/(1+param(1))
           
                    % x1* optimal:
                    x1_opt = ( u_0 / ( 1 + ( (1+beta)^(1/rho) ) * ( p^( (1-rho)/rho ) ) ) ) ^ (1/(1-rho));
                   
                    % x2* optimal:
                    % x1*[(1+Beta)*p]^(1/Rho)
                    x2_opt = x1_opt * ( (1 + beta) * p ) ^ (1 / rho);
                    % compute the optimal expenditure, using optimal bundle
                    % (x1*, x2*)
                    criterion(i,1) = (prices(i,1) * x1_opt) + (prices(i,2) * x2_opt);
                    
                end
                
                
                %% 1/1+Beta < p < 1+ Beta
                if p >= 1/(1+param(1)) && p <= (1+ param(1))
               
                    % x1* optimal:
                    x1_opt = ( u_0  / ( 2+beta ) ) ^ (1/(1-rho));
               
                    % x2* optimal:x1*
                    x2_opt = x1_opt;
               
                    % compute the optimal expenditure, using optimal bundle
                    % (x1*, x2*)
                    criterion(i,1) = (prices(i,1) * x1_opt) + (prices(i,2) * x2_opt);
           
                end
                
                
                %% p > 1 + Beta
                if p > (1+ param(1))
                    
               
                    % x1* optimal:
                    x1_opt = ( u_0 / ( (1+beta) + ( p / (1+beta) )^( (1-rho)/rho ) ) ) ^ (1/(1-rho));
               
                    % x2* optimal:
                    % x1*[p/(1+Beta)]^(1/Rho)
                    %% x2_opt = x1_opt * ( p / (1 + beta ) )  ^ (1 / rho);
                    x2_opt = ( u_0 / (1 + ( (1+beta) /( (p/(1+beta))^( (1-rho)/rho) ) ) ) )^(1/(1-rho));
               
                    % compute the optimal expenditure, using optimal bundle
                    % (x1*, x2*)
                    criterion(i,1) = (prices(i,1) * x1_opt) + (prices(i,2) * x2_opt);
                    
                end
                
                
            %% if Rho = 1 and -1<Beta<0,  CASE 9
            elseif (abs(param(2)-1)) < 10^(-6) && (param(1) > -1 && param(1) < 0)
           
           
                %% p < 1/1+Beta
                if  p < 1
                   
                    % compute the optimal expenditure
                    criterion(i,1) = (2+beta)*((((prices(i,1)^(1/(1+beta)))*prices(i,2))/(1+beta))^((1+beta)/(2+beta)))*exp(u);
                
                end
           
                %% 1/1+Beta < p < 1+ Beta
                if p == 1
                    
                    % compute the optimal expenditure
                    criterion(i,1) = (2+beta) * prices(i,1) * ((1 / (1 + beta)) ^ ((1+beta)/(2+beta))) * exp(u);
           
                end
           
                %% p > 1 + Beta           
                if p > 1
                
                    % compute the optimal expenditure
                    criterion(i,1) = (2+beta)*((((prices(i,2)^(1/(1+beta)))*prices(i,1))/(1+beta))^((1+beta)/(2+beta)))*exp(u);
           
                end
                      
            %% if Rho > 0 and -1 < Beta < 0 : CASE 8
            elseif param(2) > 0 && (param(1) > -1 && param(1) < 0)
           
                %% p < 1
                if  p < 1
               
                    % x1* optimal:
                    x1_opt = ( u_0 / ( 1 + ( (1+beta)^(1/rho) ) * ( p^( (1-rho)/rho ) ) ) ) ^ (1/(1-rho));
                    % x2* optimal:
                    % x1*[(1+Beta)*p]^(1/Rho)
                    x2_opt = x1_opt * ( (1 + beta) * p ) ^ (1 / rho);
                    % compute the optimal expenditure, using optimal bundle
                    % (x1*, x2*)
                    criterion(i,1) = (prices(i,1) * x1_opt) + (prices(i,2) * x2_opt);
           
                end
                
                
                %% p = 1
                if p == 1
               
                    % x1* optimal:
                    x1_opt = ( u_0 / ( 1 + ( (1+beta)^(1/rho) ) ) ) ^ (1/(1-rho));
                    % x2* optimal:
                    x2_opt = x1_opt * ( (1+beta)^(1/rho) );
                    % compute the optimal expenditure, using optimal bundle
                    % (x1*, x2*)
                    criterion(i,1) = (prices(i,1) * x1_opt) + (prices(i,2) * x2_opt);
           
                end
           
                %% p > 1          
                if p > 1
               
                    % x1* optimal:
                    x1_opt = ( u_0 / ( (1+beta) + ( p / (1+beta) )^( (1-rho)/rho ) ) ) ^ (1/(1-rho));
                    % x2* optimal:
                    % x1*[p / (1+Beta)]^(1/Rho)
                    x2_opt = x1_opt * ( p / (1 + beta ) )  ^ (1 / rho);
                    % compute the optimal expenditure, using optimal bundle
                    % (x1*, x2*)
                    criterion(i,1) = (prices(i,1) * x1_opt) + (prices(i,2) * x2_opt);
                    
                end
           
            %% if Rho = 1 and Beta = -1: CASE 11
            elseif (abs(param(2)-1)) < 10^(-6) && (abs(param(1)+1)<(10^(-6)))
           
                criterion(i,1) = min (prices(i, 1), prices(i, 2)) * exp(u);
        
            %% if Rho > 0 and Beta = -1: CASE 10
            elseif param(2) > 0 && (abs(param(1)+1)<(10^(-6)))
           
                criterion(i,1) = min (prices(i, 1), prices(i, 2)) * u_0 ^ (1/(1-rho));
           
            %% if Rho == 0 and Beta >= 0. CASE 12
            elseif param(2)  == 0 && param(1) >= 0
           
                %% if  p < (1/(1+Beta))
                if  p <= 1/(1+ beta)
               
                    x1_opt = max_x + (1+beta)*min_x;
                    x2_opt = 0;
                    % compute the optimal expenditure, using optimal bundle
                    % (x1*, x2*)
                    criterion(i,1) = (prices(i,1) * x1_opt) + (prices(i,2) * x2_opt);
                       
                %% if p > (1+Beta)
                elseif p >= (1+ param(1))
                    
                    x1_opt = 0;
                    x2_opt = max_x + (1+beta)*min_x;
                    % compute the optimal expenditure, using optimal bundle
                    % (x1*, x2*)
                    criterion(i,1) = (prices(i,1) * x1_opt) + (prices(i,2) * x2_opt);
                         
                %% if  (1/(1+Beta)) <= p <= (1+Beta)
                elseif ( (p >= 1/(1+ param(1)) ) && ( p <= (1+ param(1)) ) ) %&& ( observed_bundle(i,1) >= observed_bundle(i,2) )
               
                    criterion(i,1) = ( ( prices(i,1) + prices(i,2) ) * ( max_x + (1+beta)*min_x ) ) / (2+beta);
           
                end
           
            %% if Rho = 0 and 0 > Beta >= -1:
            elseif param(2) == 0 && (param(1) >= -1) && (param(1) < 0)
           
                criterion(i,1) = min (prices(i, 1), prices(i, 2)) * u_0 ;
           
            end
       
            if isnan(criterion(i,1))
   
                warning('A Nan when Rho is %f, Beta is %f, P is %f and i is %d.',rho,beta,p,i);

            end
       
        end % end of loop over observations
        
    else
        
        beta = param(1);
        A = param(2);
    
        for i = 1 : num_obs
        
            p = prices(i, 1) / prices(i,2);
        
            min_x  = min (observed_bundle(i, 1), observed_bundle(i, 2));
        
            max_x  = max (observed_bundle(i, 1), observed_bundle(i, 2));
        
            p_x = prices(i,1);
        
            p_y = prices(i,2);
        
            u_0 = -exp(-A*max_x) - (1+beta)*exp(-A*min_x);
        
            %% non-negative beta
        
            %CASE 29
            if  A == 0 
            
                criterion(i,1) = 0;
        
           % CASE 27
           elseif beta >= 0 && ((A > 0 && A < 10^(-5)) )
            
                % when a->0 the w(x)=ax-1. 
                u_0 = max_x + (1+beta)*min_x;
            
                if p <= 1/(1+beta)
                    
                    criterion(i,1) = p_x*u_0;
            
                elseif p <= 1+beta && p >= 1/(1+beta)
                
                    criterion(i,1) = ((p_x + p_y)/(2+beta))*u_0;
            
                elseif p >= 1+beta
                
                    criterion(i,1) = p_y*u_0;
                end
        
            % CASE 21    
            elseif beta >= 0 && -u_0 > 1+beta
            
                if p < - ((u_0 + (1+beta))/(1+beta)) %% Done
                
                    criterion(i,1) = - (p_x * log(-(u_0 + (1+beta))))/A;
            
                elseif p >= - ((u_0 + (1+beta))/(1+beta)) && p < 1 / (1+beta)
                
                    criterion(i,1) = p_x * ( log(-( (p+1)/(p*u_0) )) / A ) + ...
                                     p_y * ( log(- ( ((1+p)*(1+beta))/u_0 ) ) / A );
            
                elseif p >= 1/(1+beta) && p <= (1+beta) %% Done
                    
                    criterion(i,1) = (p_x + p_y) * (log(-(2+beta)/u_0) / A);
            
                elseif p > 1+beta && p <= - ((1+beta)/(u_0 + (1+beta))) %% Done
                    
                    criterion(i,1) = p_x * ( log(- ( ((1+p)*(1+beta))/(p*u_0) ) ) / A ) + ...
                                 p_y * ( log(- ( (p+1)/(u_0) )) / A );
            
                elseif p > - ((1+beta)/(u_0 + (1+beta))) %% Done
              
                    criterion(i,1) = - (p_y * log(-(u_0 + (1+beta))))/A;
            
                end
            
            % CASE 22    
            elseif beta >=0 && -u_0 <= 1+beta
            
                if p < 1/ (1+beta)
                
                    criterion(i,1) = p_x * ( log(-( (p+1)/(p*u_0) )) / A ) + ...
                                     p_y * ( log(- ( ((1+p)*(1+beta))/u_0 ) ) / A );
            
                elseif p >= 1/(1+beta) && p <= 1+beta
                    
                    criterion(i,1) = (p_x + p_y) * (log(-(2+beta)/u_0) / A);
            
                elseif p > 1 + beta
                    
                    criterion(i,1) = p_x * ( log(- ( ((1+p)*(1+beta))/(p*u_0) ) ) / A ) + ...
                                 p_y * ( log(- ( (p+1)/(u_0) )) / A );
            
                end
            
            %% negative beta  
	
            %% beta equal to -1, CASE 26 and the extreme of 28
            elseif beta == -1 || (abs(-1 - beta) <= 10^(-2))
            
                if p <= 1
                
                    criterion(i,1) = p_x * max_x;
            
                else
                    
                    criterion(i,1) = p_y * max_x;
            
                end
            
            %% CASE 28            
            elseif beta >= -1 && beta < 0 && ( A == 0 || (A > 0 && A < 10^(-5)) )
            
                 u_0 = max_x + (1+beta)*min_x;
            
                 criterion(i,1) = min (p_x, p_y) * u_0;

        
            elseif beta > -1 && beta < 0  && -u_0 > 1+beta %% Done
            
            
                %CASE 23
                if - ((u_0 + (1+beta))/(1+beta)) > 1
                
                    if p <= 1 % p_x <= p_y
                        
                        criterion(i,1) = - (p_x * log(-(u_0 + (1+beta)) ))/A;
                
                    else % p_x > p_y
                        
                        criterion(i,1) = - (p_y * log(-(u_0 + (1+beta)) ))/A;
                
                    end
            
                %CASE 24    
                else
                    
                    if p < - ((u_0 + (1+beta))/(1+beta)) % - ((u_0 + (1+beta))/(1+beta)) <= 1
                
                        criterion(i,1) = - (p_x * log(-(u_0 + (1+beta)) ))/A;
            
                    elseif p >= - ((u_0 + (1+beta))/(1+beta)) && p <= 1 %% Done
                
                        criterion(i,1) = p_x * ( log(-( (p+1)/(p*u_0) )) / A ) + ...
                                         p_y * ( log(- ( ((1+p)*(1+beta))/u_0 ) ) / A );
            
                    elseif p > 1 && p <= - ((1+beta)/(u_0 + (1+beta))) %% Done
                
                        criterion(i,1) = p_x * ( log(- ( ((1+p)*(1+beta))/(p*u_0) ) ) / A ) + ...
                                         p_y * ( log(- ( (p+1)/(u_0) )) / A );
            
                    elseif p > - ((1+beta)/(u_0 + (1+beta)))
                    
                        criterion(i,1) = - (p_y * log(-(u_0 + (1+beta))))/A;

                    end
                    
                end
                    
%             elseif - ((1+beta)/(u_0 + (1+beta))) < 1
%                 if p <= 1 % p_x <= p_y
%                     criterion(i,1) = - (p_x * log(-(u_0 + (1+beta))))/A;
%                 else
%                     criterion(i,1) = - (p_y * log(-(u_0 + (1+beta))))/A;
%                 end

            %CASE 25    
            elseif beta > -1 && beta < 0  && -u_0 <= 1+beta %% Done
            
                if p <= 1
                
                    criterion(i,1) = p_x * ( log(-( (p+1)/(p*u_0) )) / A ) + ...
                                     p_y * ( log(- ( ((1+p)*(1+beta))/u_0 ) ) / A );
            
                else
                    
                    criterion(i,1) = p_x * ( log(- ( ((1+p)*(1+beta))/(p*u_0) ) ) / A ) + ...
                                     p_y * ( log(- ( (p+1)/(u_0) )) / A );
            
                end
                
            end
        
            if isnan(criterion(i,1))
   
                warning('A Nan when A is %f, Beta is %f, P is %f and i is %d.',A,beta,p,i);

            end
        
        end %% end of for
    
    end % end of CRRA and CARA

elseif pref_class == 2
    
    if function_flag == 1

        Alpha = param(1);

        Rho = param(2);

        Rat_alpha = Alpha / (1-Alpha);

        for i = 1 : num_obs
        
            p_x = prices(i,1);
            
            p_y = prices(i,2);
            
            p = prices(i, 1) / prices(i,2);
            
            x_0 = observed_bundle(i, 1);
            
            y_0 = observed_bundle(i, 2);
            
            %CASE 8
            if Alpha < (10^(-3))
            
                criterion(i,1) = p_y * y_0 ;
            
            %CASE 9    
            elseif (1 - Alpha) < (10^(-3))
                
                criterion(i,1) = p_x * x_0 ;
           
            %CASE 10    
            elseif Rho < -250
                
                criterion(i,1) = (p_x + p_y) * min(x_0,y_0) ;                
                
            %CASE11    
            elseif abs(Rho) < 0.005
                
                criterion(i,1) = (((p_x * x_0)/Alpha)^Alpha) * (((p_y * y_0)/(1-Alpha))^(1-Alpha));                

            %CASE12    
            elseif abs(Rho-1) < 0.005
                
                if Rat_alpha < p
                
                    criterion(i,1) = p_y * (y_0 + (Rat_alpha * x_0));                
                    
                elseif Rat_alpha == p
                    
                    criterion(i,1) = (p_x * x_0) + (p_y * y_0);                                    
                    
                elseif Rat_alpha > p

                    criterion(i,1) = p_x * (x_0 + ((1 / Rat_alpha) * y_0));                

                end
                
            %CASE13    
            elseif (Rho < 1) && (Alpha > 0) && (Alpha < 1)
                
                u = ((Alpha * (x_0 ^ Rho)) + ((1 - Alpha) * (y_0 ^ Rho))) ^ (1 / Rho);
                
                Rat_Rho = Rho / (Rho - 1);
                
                den_Part_1 = (Alpha + ((Alpha^Rat_Rho)/(((1-Alpha)^(1/(Rho-1)))*(p^Rat_Rho))))^(1/Rho);
                
                den_Part_2 = ((1-Alpha) + ((((1-Alpha)^Rat_Rho)*(p^Rat_Rho))/(Alpha^(1/(Rho-1)))))^(1/Rho);
                
                criterion(i,1) = u * ((p_x/den_Part_1)+(p_y/den_Part_2));
                
                if (criterion(i,1)- 1) > 0.025
   
                    warning ('criterion is greater than 1');
                    
                end                 

            %CASE14    
            elseif (Rho > 1) && (Alpha > 0) && (Alpha < 1)
                
                u = ((Alpha * (x_0 ^ Rho)) + ((1 - Alpha) * (y_0 ^ Rho))) ^ (1 / Rho);
                
                if Rat_alpha <= p
                    
                    criterion(i,1) = u * (p_y/(1-Alpha));
                    
                    if (criterion(i,1)- 1) > 0.025
   
                        warning ('criterion is greater than 1');
    
                    end                    
                    
                else
                   
                    criterion(i,1) = u * (p_x/Alpha);                

                    if (criterion(i,1)- 1) > 0.025
   
                        warning ('criterion is greater than 1');
    
                    end                    
                    
                end
                
            end
            
            if (criterion(i,1)- 1) > 0.025
   
                 warning ('criterion is greater than 1');
    
            end
            
        end
        
    end
    
end % end of the MME function