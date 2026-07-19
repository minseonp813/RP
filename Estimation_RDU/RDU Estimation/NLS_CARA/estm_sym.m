%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Nonlinear least squares (NLS) Estimation with CRRA specification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clc

% Load the data file
% data_daegu has six column vectors. The first column is about subject ID and
% the second is about the number of decision rounds.
% The third to the six column contain the decisions and the parameter in the budget set,
% (id, round, x, y, xmax, ymax). 

load data_daegu.mat; 
dt = data_daegu; 

n = ; % n is the number of subjects or pairs

options = optimset('Display','final');

results = []; bt_results = [];

for i = 1:n
    
    m1 = 18*(i-1)+1; m2 = 18*i; id = dt(m1,1);

    % Call the demands in x and y accounts and the x and y intercepts in the budget sets
    
    x = dt(m1:m2,3); y = dt(m1:m2,4);  xmax = dt(m1:m2,5); ymax = dt(m1:m2,6);


%%%%%%%%%%%%%%%%%%%%%%%%%%%% The NLLS estimation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    vssr = []; vpm = [];

    
    % 0.1

    pm0_011 = [0.1, 0.0001];
    [pm011,ssr011] = fminsearch(@fssr2,pm0_011,options,x,y,xmax,ymax);
    vssr = [vssr; ssr011];
    vpm = [vpm; pm011];
    
    pm0_012 = [0.1, 0.01];
    [pm012,ssr012] = fminsearch(@fssr2,pm0_012,options,x,y,xmax,ymax);
    vssr = [vssr; ssr012];
    vpm = [vpm; pm012];
    
    pm0_013 = [0.1, 0.05];
    [pm013,ssr013] = fminsearch(@fssr2,pm0_013,options,x,y,xmax,ymax);
    vssr = [vssr; ssr013];
    vpm = [vpm; pm013];
    
    pm0_014 = [0.1, 0.1];
    [pm014,ssr014] = fminsearch(@fssr2,pm0_014,options,x,y,xmax,ymax);
    vssr = [vssr; ssr014];
    vpm = [vpm; pm014];
    
    pm0_015 = [0.1, 0.2];
    [pm015,ssr015] = fminsearch(@fssr2,pm0_015,options,x,y,xmax,ymax);
    vssr = [vssr; ssr015];
    vpm = [vpm; pm015];
    
    pm0_016 = [0.1, 0.5];
    [pm016,ssr016] = fminsearch(@fssr2,pm0_016,options,x,y,xmax,ymax);
    vssr = [vssr; ssr016];
    vpm = [vpm; pm016];
    
    pm0_017 = [0.1, 1];
    [pm017,ssr017] = fminsearch(@fssr2,pm0_017,options,x,y,xmax,ymax);
    vssr = [vssr; ssr017];
    vpm = [vpm; pm017];
    
    pm0_018 = [0.1, 2];
    [pm018,ssr018] = fminsearch(@fssr2,pm0_018,options,x,y,xmax,ymax);
    vssr = [vssr; ssr018];
    vpm = [vpm; pm018];
    
    pm0_019 = [0.1, 4];
    [pm019,ssr019] = fminsearch(@fssr2,pm0_019,options,x,y,xmax,ymax);
    vssr = [vssr; ssr019];
    vpm = [vpm; pm019];
    
    
    % 0.3

    pm0_021 = [0.3, 0.0001];
    [pm021,ssr021] = fminsearch(@fssr2,pm0_021,options,x,y,xmax,ymax);
    vssr = [vssr; ssr021];
    vpm = [vpm; pm021];
    
    pm0_022 = [0.3, 0.01];
    [pm022,ssr022] = fminsearch(@fssr2,pm0_022,options,x,y,xmax,ymax);
    vssr = [vssr; ssr022];
    vpm = [vpm; pm022];
    
    pm0_023 = [0.3, 0.05];
    [pm023,ssr023] = fminsearch(@fssr2,pm0_023,options,x,y,xmax,ymax);
    vssr = [vssr; ssr023];
    vpm = [vpm; pm023];
    
    pm0_024 = [0.3, 0.1];
    [pm024,ssr024] = fminsearch(@fssr2,pm0_024,options,x,y,xmax,ymax);
    vssr = [vssr; ssr024];
    vpm = [vpm; pm024];
    
    pm0_025 = [0.3, 0.2];
    [pm025,ssr025] = fminsearch(@fssr2,pm0_025,options,x,y,xmax,ymax);
    vssr = [vssr; ssr025];
    vpm = [vpm; pm025];
    
    pm0_026 = [0.3, 0.5];
    [pm026,ssr026] = fminsearch(@fssr2,pm0_026,options,x,y,xmax,ymax);
    vssr = [vssr; ssr026];
    vpm = [vpm; pm026];
    
    pm0_027 = [0.3, 1];
    [pm027,ssr027] = fminsearch(@fssr2,pm0_027,options,x,y,xmax,ymax);
    vssr = [vssr; ssr027];
    vpm = [vpm; pm027];
    
    pm0_028 = [0.3, 2];
    [pm028,ssr028] = fminsearch(@fssr2,pm0_028,options,x,y,xmax,ymax);
    vssr = [vssr; ssr028];
    vpm = [vpm; pm028];
    
    pm0_029 = [0.3, 4];
    [pm029,ssr029] = fminsearch(@fssr2,pm0_029,options,x,y,xmax,ymax);
    vssr = [vssr; ssr029];
    vpm = [vpm; pm029];
    
    
    % 0.5

    pm0_031 = [0.5, 0.0001];
    [pm031,ssr031] = fminsearch(@fssr2,pm0_031,options,x,y,xmax,ymax);
    vssr = [vssr; ssr031];
    vpm = [vpm; pm031];
    
    pm0_032 = [0.5, 0.01];
    [pm032,ssr032] = fminsearch(@fssr2,pm0_032,options,x,y,xmax,ymax);
    vssr = [vssr; ssr032];
    vpm = [vpm; pm032];
    
    pm0_033 = [0.5, 0.05];
    [pm033,ssr033] = fminsearch(@fssr2,pm0_033,options,x,y,xmax,ymax);
    vssr = [vssr; ssr033];
    vpm = [vpm; pm033];
    
    pm0_034 = [0.5, 0.1];
    [pm034,ssr034] = fminsearch(@fssr2,pm0_034,options,x,y,xmax,ymax);
    vssr = [vssr; ssr034];
    vpm = [vpm; pm034];
    
    pm0_035 = [0.5, 0.2];
    [pm035,ssr035] = fminsearch(@fssr2,pm0_035,options,x,y,xmax,ymax);
    vssr = [vssr; ssr035];
    vpm = [vpm; pm035];
    
    pm0_036 = [0.5, 0.5];
    [pm036,ssr036] = fminsearch(@fssr2,pm0_036,options,x,y,xmax,ymax);
    vssr = [vssr; ssr036];
    vpm = [vpm; pm036];
    
    pm0_037 = [0.5, 1];
    [pm037,ssr037] = fminsearch(@fssr2,pm0_037,options,x,y,xmax,ymax);
    vssr = [vssr; ssr037];
    vpm = [vpm; pm037];
    
    pm0_038 = [0.5, 2];
    [pm038,ssr038] = fminsearch(@fssr2,pm0_038,options,x,y,xmax,ymax);
    vssr = [vssr; ssr038];
    vpm = [vpm; pm038];
    
    pm0_039 = [0.5, 4];
    [pm039,ssr039] = fminsearch(@fssr2,pm0_039,options,x,y,xmax,ymax);
    vssr = [vssr; ssr039];
    vpm = [vpm; pm039];
    
    
    % 0.65

    pm0_041 = [0.65, 0.0001];
    [pm041,ssr041] = fminsearch(@fssr2,pm0_041,options,x,y,xmax,ymax);
    vssr = [vssr; ssr041];
    vpm = [vpm; pm041];
  
    pm0_042 = [0.65, 0.01];
    [pm042,ssr042] = fminsearch(@fssr2,pm0_042,options,x,y,xmax,ymax);
    vssr = [vssr; ssr042];
    vpm = [vpm; pm042];
    
    pm0_043 = [0.65, 0.05];
    [pm043,ssr043] = fminsearch(@fssr2,pm0_043,options,x,y,xmax,ymax);
    vssr = [vssr; ssr043];
    vpm = [vpm; pm043];

    pm0_044 = [0.65, 0.1];
    [pm044,ssr044] = fminsearch(@fssr2,pm0_044,options,x,y,xmax,ymax);
    vssr = [vssr; ssr044];
    vpm = [vpm; pm044];
    
    pm0_045 = [0.65, 0.2];
    [pm045,ssr045] = fminsearch(@fssr2,pm0_045,options,x,y,xmax,ymax);
    vssr = [vssr; ssr045];
    vpm = [vpm; pm045];
    
    pm0_046 = [0.65, 0.5];
    [pm046,ssr046] = fminsearch(@fssr2,pm0_046,options,x,y,xmax,ymax);
    vssr = [vssr; ssr046];
    vpm = [vpm; pm046];
    
    pm0_047 = [0.65, 1];
    [pm047,ssr047] = fminsearch(@fssr2,pm0_047,options,x,y,xmax,ymax);
    vssr = [vssr; ssr047];
    vpm = [vpm; pm047];
    
    pm0_048 = [0.65, 2];
    [pm048,ssr048] = fminsearch(@fssr2,pm0_048,options,x,y,xmax,ymax);
    vssr = [vssr; ssr048];
    vpm = [vpm; pm048];
    
    pm0_049 = [0.65, 4];
    [pm049,ssr049] = fminsearch(@fssr2,pm0_049,options,x,y,xmax,ymax);
    vssr = [vssr; ssr049];
    vpm = [vpm; pm049];

    % 0.8

    pm0_051 = [0.8, 0.0001];
    [pm051,ssr051] = fminsearch(@fssr2,pm0_051,options,x,y,xmax,ymax);
    vssr = [vssr; ssr051];
    vpm = [vpm; pm051];
    
    pm0_052 = [0.8, 0.01];
    [pm052,ssr052] = fminsearch(@fssr2,pm0_052,options,x,y,xmax,ymax);
    vssr = [vssr; ssr052];
    vpm = [vpm; pm052];
    
    pm0_053 = [0.8, 0.05];
    [pm053,ssr053] = fminsearch(@fssr2,pm0_053,options,x,y,xmax,ymax);
    vssr = [vssr; ssr053];
    vpm = [vpm; pm053];
    
    pm0_054 = [0.8, 0.1];
    [pm054,ssr054] = fminsearch(@fssr2,pm0_054,options,x,y,xmax,ymax);
    vssr = [vssr; ssr054];
    vpm = [vpm; pm054];
    
    pm0_055 = [0.8, 0.2];
    [pm055,ssr055] = fminsearch(@fssr2,pm0_055,options,x,y,xmax,ymax);
    vssr = [vssr; ssr055];
    vpm = [vpm; pm055];
    
    pm0_056 = [0.8, 0.5];
    [pm056,ssr056] = fminsearch(@fssr2,pm0_056,options,x,y,xmax,ymax);
    vssr = [vssr; ssr056];
    vpm = [vpm; pm056];
    
    pm0_057 = [0.8, 1];
    [pm057,ssr057] = fminsearch(@fssr2,pm0_057,options,x,y,xmax,ymax);
    vssr = [vssr; ssr057];
    vpm = [vpm; pm057];
    
    pm0_058 = [0.8, 2];
    [pm058,ssr058] = fminsearch(@fssr2,pm0_058,options,x,y,xmax,ymax);
    vssr = [vssr; ssr058];
    vpm = [vpm; pm058];
    
    pm0_059 = [0.8, 4];
    [pm059,ssr059] = fminsearch(@fssr2,pm0_059,options,x,y,xmax,ymax);
    vssr = [vssr; ssr059];
    vpm = [vpm; pm059];

    
    % 0.6

    pm0_061 = [0.9, 0.0001];
    [pm061,ssr061] = fminsearch(@fssr2,pm0_061,options,x,y,xmax,ymax);
    vssr = [vssr; ssr061];
    vpm = [vpm; pm061];
    
    pm0_062 = [0.9, 0.01];
    [pm062,ssr062] = fminsearch(@fssr2,pm0_062,options,x,y,xmax,ymax);
    vssr = [vssr; ssr062];
    vpm = [vpm; pm062];
    
    pm0_063 = [0.9, 0.05];
    [pm063,ssr063] = fminsearch(@fssr2,pm0_063,options,x,y,xmax,ymax);
    vssr = [vssr; ssr063];
    vpm = [vpm; pm063];
    
    pm0_064 = [0.9, 0.1];
    [pm064,ssr064] = fminsearch(@fssr2,pm0_064,options,x,y,xmax,ymax);
    vssr = [vssr; ssr064];
    vpm = [vpm; pm064];
    
    pm0_065 = [0.9, 0.2];
    [pm065,ssr065] = fminsearch(@fssr2,pm0_065,options,x,y,xmax,ymax);
    vssr = [vssr; ssr065];
    vpm = [vpm; pm065];
    
    pm0_066 = [0.9, 0.5];
    [pm066,ssr066] = fminsearch(@fssr2,pm0_066,options,x,y,xmax,ymax);
    vssr = [vssr; ssr066];
    vpm = [vpm; pm066];
    
    pm0_067 = [0.9, 1];
    [pm067,ssr067] = fminsearch(@fssr2,pm0_067,options,x,y,xmax,ymax);
    vssr = [vssr; ssr067];
    vpm = [vpm; pm067];
    
    pm0_068 = [0.9, 2];
    [pm068,ssr068] = fminsearch(@fssr2,pm0_068,options,x,y,xmax,ymax);
    vssr = [vssr; ssr068];
    vpm = [vpm; pm068];
    
    pm0_069 = [0.9, 4];
    [pm069,ssr069] = fminsearch(@fssr2,pm0_069,options,x,y,xmax,ymax);
    vssr = [vssr; ssr069];
    vpm = [vpm; pm069];
    
    if (id == 1388305)
        
        pm0_070 = [0.5, 0.002247];
        [pm070,ssr070] = fminsearch(@fssr2,pm0_070,options,x,y,xmax,ymax);
        vssr = [vssr; ssr070];
        vpm = [vpm; pm070];
        
        
    elseif (id == 5707701)
        pm0_070 = [0.5, 0.001821];
        [pm070,ssr070] = fminsearch(@fssr2,pm0_070,options,x,y,xmax,ymax);
        vssr = [vssr; ssr070];
        vpm = [vpm; pm070];

        
    elseif (id == 8339902)
        
         pm0_070 = [0.5, 0.001616];
        [pm070,ssr070] = fminsearch(@fssr2,pm0_070,options,x,y,xmax,ymax);
        vssr = [vssr; ssr070];
        vpm = [vpm; pm070];
        
       
    end
       
    ssrmin = min(vssr);
    
    nv = length(vssr);
    
    eps = 0;
    
    kr = 0;
    
    mpm = []; mssr = [];
    
    for k = 1:nv
        
        if (vssr(k) <= ssrmin + eps)
            
            kr = kr + 1;
            
            mpm = [mpm; vpm(k,:)];
            mssr = [mssr; vssr(k)];
                 
        end
    end
    
    nm = length(mssr);
    
    rangen = nm*rand(1,1);
    
    for t = 1:nm
        
        if (rangen >= (t-1))&&(rangen < t)
            
            pm = mpm(t,:); ssr = mssr(t);
                       
        end
        
    end
    
    alpha = pm(1); rho = pm(2);
       
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% Estimation Results for Subject id
% log alpha and Rho, their standard errors (using a boostrap method), p-value (5% significance level) under the null of log alpha equal to zero
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    btout = bstrap(x,y,xmax,ymax,500,pm);
%    btid = ones(500,1)*id; 
%    btres = [btid,btout];

    se1 = std(btout(:,1));
    se2 = std(btout(:,2));
    
%    sort_bt1 = sort(btout(:,1));
%    sort_bt2 = sort(btout(:,2));
    
%    ci_lb1 = sort_bt1(13);
%    ci_ub1 = sort_bt1(487);
    
%    ci_lb2 = sort_bt2(13);
%    ci_ub2 = sort_bt2(487);
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

    out = [id,alpha,se1,rho,se2,ssr];
    
    results = [results;out];
%    bt_results = [bt_results;btres];
 
    save results_04102013 results
%    save bt_results bt_results
  
end



