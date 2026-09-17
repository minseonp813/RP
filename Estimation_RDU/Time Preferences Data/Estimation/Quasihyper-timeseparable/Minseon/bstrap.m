function output = bstrap(pm0,y,x,ymax,xmax,treatment,nr)

% Compute the standard errors of alpha and rho using the bootstrap method
% nr : the number of replication in boostrapping

    LB = [0 0 0 0]; UB = [2 2 3 3];

 
options = optimset('Display','final');

    tbeta = zeros(nr,1); tdelta = zeros(nr,1);
    tA1 = zeros(nr,1); tA2 = zeros(nr,1);
    
    var=[pm0; 1 0.9 0.05 0.05; 0.9 0.95 0.1 0.1]; 
    vpm=zeros(3,4); vssr=zeros(3,1);
    
for i = 1:nr    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Bootstrap sampling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         k1 = round(0.5+50*(rand(50,1))) ;
         k2 = round(50+0.5+50*(rand(50,1)));
                yy1 = y(k1); xx1 = x(k1); 
                yymax1 = ymax(k1); xxmax1 = xmax(k1); 
                yy2 = y(k2); xx2 = x(k2); 
                yymax2 = ymax(k2); xxmax2 = xmax(k2);
        
    yy = [yy1;yy2]; xx = [xx1;xx2]; yymax = [yymax1;yymax2]; xxmax = [xxmax1;xxmax2];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    for j=1:3
    [vpm(j,:),vssr(j)] = fmincon(@fssr,var(j,:),[],[],[],[],LB,UB,[],options,yy,xx,yymax,xxmax,treatment);
    end
    
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





