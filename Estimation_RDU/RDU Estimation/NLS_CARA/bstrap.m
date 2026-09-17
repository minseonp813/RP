function output = bstrap(xd,yd,xdmax,ydmax,nr,pm0)

% Compute the standard errors of alpha and rho using the bootstrap method
% nr : the number of replication in boostrapping

unif = 18*rand(nr,18);

talpha = []; trho = [];

options = optimset('Display','off');

for i = 1:nr    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Bootstrap sampling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    rsam = unif(i,:);    
    y = []; x = []; ymax = []; xmax = [];
    
    for j = 1:18        
        for k = 1:18
            if (((k-1) <= rsam(j))&& (rsam(j)<k))  
                y = [y;yd(k)]; x = [x;xd(k)];
                ymax = [ymax;ydmax(k)]; xmax = [xmax;xdmax(k)];
            end            
        end 
    end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    vssr = []; vpm = [];
    
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
    
    
    % 0.7

    pm0_051 = [0.7, 0.0001];
    [pm051,ssr051] = fminsearch(@fssr2,pm0_051,options,x,y,xmax,ymax);
    vssr = [vssr; ssr051];
    vpm = [vpm; pm051];
    
    pm0_052 = [0.7, 0.01];
    [pm052,ssr052] = fminsearch(@fssr2,pm0_052,options,x,y,xmax,ymax);
    vssr = [vssr; ssr052];
    vpm = [vpm; pm052];
    
    pm0_053 = [0.7, 0.05];
    [pm053,ssr053] = fminsearch(@fssr2,pm0_053,options,x,y,xmax,ymax);
    vssr = [vssr; ssr053];
    vpm = [vpm; pm053];
    
    pm0_054 = [0.7, 0.1];
    [pm054,ssr054] = fminsearch(@fssr2,pm0_054,options,x,y,xmax,ymax);
    vssr = [vssr; ssr054];
    vpm = [vpm; pm054];
    
    pm0_055 = [0.7, 0.2];
    [pm055,ssr055] = fminsearch(@fssr2,pm0_055,options,x,y,xmax,ymax);
    vssr = [vssr; ssr055];
    vpm = [vpm; pm055];
    
    pm0_056 = [0.7, 0.5];
    [pm056,ssr056] = fminsearch(@fssr2,pm0_056,options,x,y,xmax,ymax);
    vssr = [vssr; ssr056];
    vpm = [vpm; pm056];
    
    pm0_057 = [0.7, 1];
    [pm057,ssr057] = fminsearch(@fssr2,pm0_057,options,x,y,xmax,ymax);
    vssr = [vssr; ssr057];
    vpm = [vpm; pm057];
    
    pm0_058 = [0.7, 2];
    [pm058,ssr058] = fminsearch(@fssr2,pm0_058,options,x,y,xmax,ymax);
    vssr = [vssr; ssr058];
    vpm = [vpm; pm058];
    
    pm0_059 = [0.7, 4];
    [pm059,ssr059] = fminsearch(@fssr2,pm0_059,options,x,y,xmax,ymax);
    vssr = [vssr; ssr059];
    vpm = [vpm; pm059];

    
    
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
       
    
    
    talpha = [talpha; alpha]; trho = [trho; rho]; 
    
end

output = [talpha,trho];



