function output = opdem(beta,delta,A1,A2,eMax,lMax,treatment)

if treatment == 1
    
    dfactor = beta*delta; curv = A1;
    
elseif treatment == 2
    
    dfactor = delta; curv = A2;
    
end

ep = 0.000001;

if curv >= ep
    term = (1/curv)*log(dfactor*(lMax/eMax));
else
    term = (1/ep)*log(dfactor*(lMax/eMax));
end

if ((term >= -eMax)&&(term <= lMax))
        
        dm1 = (1/(1+(lMax/eMax)))*(lMax - term); 
        dm2 = ((lMax/eMax)/(1+(lMax/eMax)))*(eMax + term);
        
elseif (term < -eMax)
        
        dm1 = eMax;
        dm2 = 0;
        
elseif (term > lMax)
        
        dm1 = 0;
        dm2 = lMax;
        
end   

    
output = [dm1,dm2];

end
