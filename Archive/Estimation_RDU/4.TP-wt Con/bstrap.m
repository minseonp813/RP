function output = bstrap(pm0,y,x,ymax,xmax,treatment,nr)

% Compute the standard errors of alpha and rho using the bootstrap method
% nr : the number of replication in boostrapping

    LB = [0 0 0 0]; UB = [2 2 3 3];
 
options = optimset('Display','final');

    tbeta = zeros(nr,1); tdelta = zeros(nr,1);
    tA1 = zeros(nr,1); tA2 = zeros(nr,1);
    vpm=zeros(3,4); vssr=zeros(3,1);
    kk1=sum(treatment==1);
    kk2=sum(treatment==2);
    
for i = 1:nr    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Bootstrap sampling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         k1 = round(0.5+kk1*(rand(kk1,1))) ;
         k2 = round(kk1+0.5+kk2*(rand(kk2,1)));
         k = [k1;k2];
         yy = y(k); xx = x(k); yymax = ymax(k); xxmax = xmax(k); ttreatment = treatment(k);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
    pm01 = pm0;
    [vpm(1,:),vssr(1)] = fmincon(@fssr,pm01,[],[],[],[],LB,UB,[],options,yy,xx,yymax,xxmax,ttreatment);

    pm02 = [1; 0.9; 0.05; 0.05];
    [vpm(2,:),vssr(2)] = fmincon(@fssr,pm02,[],[],[],[],LB,UB,[],options,yy,xx,yymax,xxmax,ttreatment);
    
    pm03 = [0.9; 0.95; 0.1; 0.1];
    [vpm(3,:),vssr(3)] = fmincon(@fssr,pm03,[],[],[],[],LB,UB,[],options,yy,xx,yymax,xxmax,ttreatment);
    
    
    ssrmin = min(vssr);
    eps = 0;
     
    mpm = vpm(vssr<=ssrmin+eps,:);
    kr= size(mpm,1);  
   
    t = round(0.5+kr*rand(1));
    
    pm = mpm(t,:); 
    
    beta = pm(1); delta = pm(2); 
    
    ep = 0.000001;
         
    A1 = (pm(3)>=ep)*pm(3)+(pm(3)<ep)*0 ;
    A2 = (pm(4)>=ep)*pm(4)+(pm(4)<ep)*0 ;    
    
    tbeta(i) = beta; tdelta(i) = delta;
    tA1(i) = A1; tA2(i) = A2;
    
end

output = [tbeta, tdelta, tA1 tA2];





