function output = bstrapm1(pm0,xd,yd,xdmax,ydmax,nb)

% Compute the standard errors of alpha and rho using the bootstrap method
% nr : the number of replication in boostrapping
% vecotrized loops & confined the number of extensive serach(to 9 nitmes)  bstrap

global M
global lb
global ub

bvar = [pm0; 0.5 0.1; 0.5 0.5];
bvpm = zeros(3,2); bvssr = zeros(3,1);

talpha = zeros(nb,1); trho = zeros(nb,1);

options = optimset('Display','off');
    
for i = 1:nb    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Bootstrap sampling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
            k = round(0.5+M*(rand(18,1))) ;
                y = yd(k); x = xd(k); 
                ymax = ydmax(k); xmax = xdmax(k); 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    for j=1:3
    [bvpm(j,:),bvssr(j)] = fmincon(@fssr2,bvar(j,:),[],[],[],[],lb,ub,[],options,x,y,xmax,ymax);
    end
    
    ssrmin = min(bvssr);
    eps = 0;
   
    mpm = bvpm(bvssr<=ssrmin+eps,:);    
    
    kr= size(mpm,1);  
     
    t = round(0.5+kr*rand(1));
    
    pm = mpm(t,:); 
    
    alpha = pm(1);  rho = pm(2);
           
    talpha(i) = alpha; trho(i) = rho; 
   
end

output = [talpha,trho];



