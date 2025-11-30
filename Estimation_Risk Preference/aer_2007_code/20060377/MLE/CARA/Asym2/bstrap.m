function output = bstrap(y,x,ymax,xmax,nr)

% Compute the standard errors of alpha and rho using the bootstrap method
% nr : the number of replication in boostrapping

unif = 50*rand(nr,50);

alpha = []; rho = [];

for i = 1:nr   
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Bootstrap sampling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    rsam = unif(i,:);    
    dy = []; dx = []; dymax = []; dxmax = [];
    
    for j = 1:50        
        for k = 1:50
            if (((k-1) <= rsam(j))& (rsam(j)<k))  
                dy = [dy;y(k)]; dx = [dx;x(k)];
                dymax = [dymax;ymax(k)]; dxmax = [dxmax;xmax(k)];
            end            
        end 
    end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    pm0 = [-2; -1; -2];
    [pm,neg_loglik] = fminsearch(@likh,pm0,optimset('Display','off'),dy,dx,dymax,dxmax);
    
    alpha = [alpha;exp(exp(pm(1)))]; rho = [rho;exp(pm(2))]; 
    
end

output = [std(alpha),std(rho)];
