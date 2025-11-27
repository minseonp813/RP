function output = bstrapm1(xd,yd,xdmax,ydmax,nb)

% Compute the standard errors of alpha and rho using the bootstrap method
% nr : the number of replication in boostrapping
% vecotrized loops & confined the number of extensive serach(to 9 nitmes)  bstrap

global M

talpha = zeros(nb,1); trho = zeros(nb,1);

options = optimset('Display','off');

    
for i = 1:nb    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Bootstrap sampling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
            k = round(0.5+M*(rand(18,1))) ;
                y = yd(k); x = xd(k); 
                ymax = ydmax(k); xmax = xdmax(k); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 a = [0.1 0.3 0.5 0.7 0.9] ;
    r = [0.01 0.1 0.5 1 2] ;
    na = size(a',1) ; nr = size(r',1);
    nar = na * nr;
    vpm = zeros(nar,2); vssr = zeros(nar,1); 
    aa=[a;a;a;a;a]; aa=aa(:);
    rr=[r,r,r,r,r]';
    var=[aa rr];
    
    lb=[0 0]; ub=[1 5];
    
    for j=1:nar
    [vpm(j,:),vssr(j)] = fmincon(@fssr2,var(j,:),[],[],[],[],lb,ub,[],options,x,y,xmax,ymax);
    end
    
    ssrmin = min(vssr);
    eps = 0;
   
    mpm = vpm(vssr<=ssrmin+eps,:);    
    
    kr= size(mpm,1);  
     
    t = round(0.5+kr*rand(1));
    
    pm = mpm(t,:); 
    
    alpha = pm(1);  rho = pm(2);
           
    talpha(i) = alpha; trho(i) = rho; 
   
end

output = [talpha,trho];



