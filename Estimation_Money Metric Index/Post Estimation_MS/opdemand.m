function output = opdemand(alpha,rho,p1,p2)

% 0 < alpha < 1; alpha = 1/2 EUT; 
% alpha > 1/2 pessimism (or disappointment aversion in Gul's model); alpha < 1/2 optimism (or elation loving in Gul's model)
% rho: CARA coefficient

eps = 0.0000001;

ln_p12 = log(p1/p2); ln_p21 = log(p2/p1);

if rho > eps
    
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
            
                if ((rho/p2) >= ln_p12 - log(alpha/(1-alpha)))
                    
                    x1 = 1/(p1+p2) - (p2/(rho*(p1+p2)))*(ln_p12 - log(alpha/(1-alpha)));
    
                    x2 = 1/(p1+p2) + (p1/(rho*(p1+p2)))*(ln_p12 - log(alpha/(1-alpha)));
                
                else
                
                    x1 = 0; x2 = 1/p2;
                                
                end
            

            else
            
                if ((rho/p1) >= ln_p21 - log(alpha/(1-alpha)))
                
                    x2 = 1/(p1+p2) - (p1/(rho*(p1+p2)))*(ln_p21 - log(alpha/(1-alpha)));
    
                    x1 = 1/(p1+p2) + (p2/(rho*(p1+p2)))*(ln_p21 - log(alpha/(1-alpha)));
                
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

