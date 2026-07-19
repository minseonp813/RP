
clc

load Data-Xlab-TPE.mat; 
dt = data; 

n = length(dt)/100; 

results = []; btsample_all = []; Pred = [];

nboot = 2;

for i = 1:1
    
    m1 = 100*(i-1)+1; m2 = 100*i; 
    id = dt(m1,2)
    session = dt(m1,1);
    
    if session ~= 4
    
    treatment = dt(m1:m2,3);
    
    mlate = dt(m1:m2,8); mearly = dt(m1:m2,7); 
    clate = dt(m1:m2,6); cearly = dt(m1:m2,5);
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%% The NLLS estimation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    LB = [0;0;0]; UB = [2;1; 2];

    options = optimset('Algorithm','interior-point','Display','iter','MaxFunEvals',1.0E+5);
    
    pm01 = [1; 0.9; 0.05];
    
    [pm1,tssr1] = fmincon(@fssr,pm01,[],[],[],[],LB,UB,[],options,cearly,clate,mearly,mlate,treatment);

    pm02 = [1; 0.8; 0.1];
    
    [pm2,tssr2] = fmincon(@fssr,pm02,[],[],[],[],LB,UB,[],options,cearly,clate,mearly,mlate,treatment);

    pm03 = [1; 0.95; 0.3];
    
    [pm3,tssr3] = fmincon(@fssr,pm03,[],[],[],[],LB,UB,[],options,cearly,clate,mearly,mlate,treatment);

    pm04 = [0.8; 0.9; 0.05];
    
    [pm4,tssr4] = fmincon(@fssr,pm04,[],[],[],[],LB,UB,[],options,cearly,clate,mearly,mlate,treatment);

    pm05 = [0.8; 0.8; 0.1];
    
    [pm5,tssr5] = fmincon(@fssr,pm05,[],[],[],[],LB,UB,[],options,cearly,clate,mearly,mlate,treatment);

    pm06 = [0.8; 0.95; 0.3];
    
    [pm6,tssr6] = fmincon(@fssr,pm06,[],[],[],[],LB,UB,[],options,cearly,clate,mearly,mlate,treatment);
    
    pm07 = [1.1; 0.8; 0.05];
    
    [pm7,tssr7] = fmincon(@fssr,pm07,[],[],[],[],LB,UB,[],options,cearly,clate,mearly,mlate,treatment);

    pm08 = [1.1; 0.95; 0.3];
    
    [pm8,tssr8] = fmincon(@fssr,pm08,[],[],[],[],LB,UB,[],options,cearly,clate,mearly,mlate,treatment);
    
    tssrmin = min([tssr1 tssr2 tssr3 tssr4 tssr5 tssr6 tssr7 tssr8]);
    
    if tssr1 <= tssrmin
        
        pm = pm1; tssr = tssrmin;
        
    elseif tssr2 <= tssrmin
        
        pm = pm2; tssr = tssrmin;
        
    elseif tssr3 <= tssrmin
        
        pm = pm3; tssr = tssrmin;

    elseif tssr4 <= tssrmin
        
        pm = pm4; tssr = tssrmin;

    elseif tssr5 <= tssrmin
        
        pm = pm5; tssr = tssrmin;
        
    elseif tssr6 <= tssrmin
        
        pm = pm6; tssr = tssrmin;

    elseif tssr7 <= tssrmin
        
        pm = pm7; tssr = tssrmin;
        
    else
        
        pm = pm8; tssr = tssrmin;
        
    end
    
    beta = pm(1); delta = pm(2); 
    
    ep = 0.0001;
    
    if pm(3)>= ep 
        A = pm(3);        
    else
        A = 0;
    end
    
%    sst = sum((y-x).^2); r2 = 1-ssr/sst;
    
    btsample = bstrap(pm,cearly,clate,mearly,mlate,treatment,nboot);
    
    cov_disc = cov(btsample(:,1:2));
    
    se_beta = sqrt(cov_disc(1,1));
    se_delta = sqrt(cov_disc(2,2));
    
    cov_bd = cov_disc(1,2);
    
    se_A = std(btsample(:,3));
      
    for j = 1:100
        
        mE = mearly(j); mL = mlate(j); mT = treatment(j);
        
        pd = opdem(beta,delta,A,A,mE,mL,mT);
        
        Pred = [Pred; id, j, mT, pd(1),pd(2), mE, mL]; 
    end
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

    out = [id,beta,se_beta,delta,se_delta,cov_bd, A,se_A,tssr];
    
    results = [results;out];
    btsample_all = [btsample_all; id*ones(nboot,1), btsample];
 
    
save Results results
save Bootsample btsample_all
save Pred Pred

    end
    
end


