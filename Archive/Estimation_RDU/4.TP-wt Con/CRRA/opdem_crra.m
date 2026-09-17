function output = opdem_crra(beta,delta,rho1,rho2,omega,eMax,lMax,treatment)

if treatment == 1
    
    dfactor = beta*delta;  rho=rho1;
    
elseif treatment == 2
    
    dfactor = delta; rho=rho2;
    
end

ep = 0.000001;

if rho >= ep
    term = (dfactor*(lMax/eMax))^(1/rho);
else
    term = (dfactor*(lMax/eMax))^(1/ep); 
end

if (lMax/eMax)<(omega^(rho))/(dfactor*((eMax+omega)^(rho)))
        
    dm1=eMax;
    dm2=0;
        
elseif (lMax/eMax)>((lMax+omega)^(rho))/(dfactor*omega^(rho))
        
        dm1 = 0;
        dm2 = lMax;
        
else  
        dm1 = lMax/(term+(lMax/eMax))+omega*(1-term)/(term+(lMax/eMax)) ;
        dm2 = (term*(lMax/eMax)/(term+(lMax/eMax)))*(eMax+(1-1/term)*omega);
        
end   

    
output = [dm1,dm2];

end
