function output = bstrap(pm0,y,x,ymax,xmax,treatment,nr)

% Compute the standard errors of alpha and rho using the bootstrap method
% nr : the number of replication in boostrapping

unif1 = 50*rand(nr,50); unif2 = 50*rand(nr,50);

tbeta = []; tdelta = []; tA = [];

LB = [0;0;0]; UB = [5;1; 2];

options = optimset('Algorithm','interior-point','Display','off','MaxFunEvals',1.0E+5);


for i = 1:nr    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Bootstrap sampling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    rsam1 = unif1(i,:); rsam2 = unif2(i,:);   
    yy1 = []; xx1 = []; yymax1 = []; xxmax1 = [];
    yy2 = []; xx2 = []; yymax2 = []; xxmax2 = [];
    
    for j = 1:50        
        for k = 1:50
            if (((k-1) <= rsam1(j))&& (rsam1(j)<k))  
                yy1 = [yy1;y(k)]; xx1 = [xx1;x(k)];
                yymax1 = [yymax1;ymax(k)]; xxmax1 = [xxmax1;xmax(k)];
            end 
            if (((k-1) <= rsam2(j))&& (rsam2(j)<k))  
                yy2 = [yy2;y(k+50)]; xx2 = [xx2;x(k+50)];
                yymax2 = [yymax2;ymax(k+50)]; xxmax2 = [xxmax2;xmax(k+50)];
            end             
        end
    end
        
    yy = [yy1;yy2]; xx = [xx1;xx2]; yymax = [yymax1;yymax2]; xxmax = [xxmax1;xxmax2];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    pm01 = pm0;
    [pm1,tssr1] = fmincon(@fssr,pm01,[],[],[],[],LB,UB,[],options,yy,xx,yymax,xxmax,treatment);

    pm02 = [1; 0.9; 0.05];
    [pm2,tssr2] = fmincon(@fssr,pm02,[],[],[],[],LB,UB,[],options,yy,xx,yymax,xxmax,treatment);
    
    pm03 = [0.9; 0.95; 0.1];
    [pm3,tssr3] = fmincon(@fssr,pm03,[],[],[],[],LB,UB,[],options,yy,xx,yymax,xxmax,treatment);
    
    tssrmin = min([tssr1 tssr2 tssr3]);
    
    if (tssr1 <= tssrmin)
        
        pm = pm1;
        
    elseif (tssr2 <= tssrmin)
        
        pm = pm2;
        
    else
        
        pm = pm3;
        
    end
    
    beta = pm(1); delta = pm(2); 
    
    ep = 0.000001;
    
    if pm(3)>= ep 
        A = pm(3);        
    else
        A = 0;
    end
    
    tbeta = [tbeta; beta]; tdelta = [tdelta;delta]; tA = [tA;A];
    
end

output = [tbeta, tdelta, tA];





