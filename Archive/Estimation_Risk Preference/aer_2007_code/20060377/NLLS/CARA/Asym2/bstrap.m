function output = bstrap(y,x,ymax,xmax,nr)

% Compute the standard errors of alpha and rho using the bootstrap method
% nr : the number of replication in boostrapping

unif = 50*rand(nr,50);

alpha = []; rho = [];

for i = 1:nr    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Bootstrap sampling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    rsam = unif(i,:);    
    yy = []; xx = []; yymax = []; xxmax = [];
    
    for j = 1:50        
        for k = 1:50
            if (((k-1) <= rsam(j))& (rsam(j)<k))  
                yy = [yy;y(k)]; xx = [xx;x(k)];
                yymax = [yymax;ymax(k)]; xxmax = [xxmax;xmax(k)];
            end            
        end 
    end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    pm0 = [-3; -2];
    [pm,ssr] = fminsearch(@fssr,pm0,optimset('Display','off'),yy,xx,yymax,xxmax);
    
    alpha = [alpha;exp(exp(pm(1)))]; rho = [rho;exp(pm(2))]; 
    
end

output = [std(alpha),std(rho)];



