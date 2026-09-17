function output = opdemand(alpha,gamma,omega,p1,p2)

% 0 < alpha < 1; alpha = 1/2 EUT; 
% alpha > 1/2 pessimism (or disappointment aversion in Gul's model); alpha < 1/2 optimism (or elation loving in Gul's model)
% gamma: CRRA coefficient
% omega: a parameter of demand shifter, small enough, set 0.01(?)

eps = 0.0000001;



ln_p12 = log(p1/p2); ln_p21 = log(p2/p1);


if gamma > eps
    
    if alpha < eps
        
        if p1 >= p2
            
            x1 = 0; x2 = 1/p2;
            
        else
            
            x1 = 1/p1; x2 = 0;
        
        end
        
    elseif alpha > 1 - eps
        
        x1 = 1/(p1+p2); x2 = 1/(p1+p2);
                       
    else
        
        if p1 >= p2
            
            K = ((p2/p1)*(alpha/(1-alpha)))^(1/gamma);
            
        else
            
            K = ((p1/p2)*(alpha/(1-alpha)))^(1/gamma);
            
        end
        

        if (ln_p12 < log(alpha/(1-alpha)))&&(ln_p12 > - log(alpha/(1-alpha)))
    
            x1 = 1/(p1+p2); x2 = 1/(p1+p2);
        
        else
    
            if p1 >= p2
            
                if (ln_p12 < (gamma*(log((1+omega*p2)/(omega*p2))) + log(alpha/(1-alpha))))
                                        
                    x1 = K*(1+(1-K)*omega*p1)/(K*p1 + p2) - (1-K)*omega;
    
                    x2 = (1+(1-K)*omega*p1)/(K*p1 + p2);
                
                else
                
                    x1 = 0; x2 = 1/p2;
                                
                end
            

            else
            
                if (ln_p21 < (gamma*(log((1+omega*p1)/(omega*p1))) + log(alpha/(1-alpha))))
                                        
                    x2 = K*(1+(1-K)*omega*p2)/(K*p2 + p1) - (1-K)*omega;
    
                    x1 = (1+(1-K)*omega*p2)/(K*p2 + p1);
                
                else
                
                    x2 = 0; x1 = 1/p1;
                                
                end
            

            end
        end
    end

else
    
    if alpha < eps
        if p1 >= p2
            
            x1 = 0; x2 = 1/p2;
            
        else
            
            x1 = 1/p1; x2 = 0;
        
        end
        
    elseif alpha > 1 - eps
        
        x1 = 1/(p1+p2); x2 = 1/(p1+p2);
        
    else
    
        if (ln_p12 < log(alpha/(1-alpha)))&&(ln_p12 > - log(alpha/(1-alpha)))
    
            x1 = 1/(p1+p2); x2 = 1/(p1+p2);
        
        else
        
            if p1 >= p2
            
                x1 = 0; x2 = 1/p2;
            
            
            else
            
                x1 = 1/p1; x2 = 0;
    
            end
    
        end
    
    end
end

    output = [x1,x2];
    
end


