
clc

load Data-Xlab-TPE.mat; 
data(:,9) = (data(:,7) >= 10 & data(:,8) >= 10);
dt = data(data(:,1) ~= 4 | (data(:,1) == 4 & data(:,9) == 1),:); 

n = size(unique(data(:,2)),1); 
nboot = 200;

btsample_all = []; 
Pred=zeros(n,7); results=zeros(n,11);
    
    b = [0.8 1 1.1]; d = [0.8 0.95];
    aa1 = [0.05 3]; aa2 = [0.05 3];
    nar = size(b',1)* size(d',1) * size(aa1',1) *size(aa2',1);
    vpm = zeros(nar,4); vssr = zeros(nar,1); 
    tvar={b,d,aa1,aa2};
    [bb, dd, aaa1, aaa2]=ndgrid(tvar{:});
    var=[bb(:), dd(:), aaa1(:), aaa2(:)];
    clear bb dd aaa1 aaa2 tvar;
    
options = optimset('Display','final');
LB = [0 0 0 0]; UB = [2 2 3 3];

m1=1;
for i = 1:10
    
    [~,~,m] = find(dt==dt(m1,2)) ;
    M=sum(m);
    m2=m1+M-1 ;
    id = dt(m1,2);
    session = dt(m1,1);
    
    treatment = dt(m1:m2,3);
    
    mlate = dt(m1:m2,8); mearly = dt(m1:m2,7); 
    clate = dt(m1:m2,6); cearly = dt(m1:m2,5);
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%% The NLLS estimation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    for j=1:nar
    [vpm(j,:),vssr(j)] = fmincon(@fssr,var(j,:),[],[],[],[],LB,UB,[],options,cearly,clate,mearly,mlate,treatment);
    end
    
    ssrmin = min(vssr);
    eps = 0;
     
    mpm = vpm(vssr<=ssrmin+eps,:);
    mssr = vssr(vssr<=ssrmin+eps);
    kr= size(mpm,1);  
   
    t = round(0.5+kr*rand(1));
    
    pm = mpm(t,:); ssr = mssr(t);
    
    beta = pm(1); delta = pm(2); 
    
    ep = 0.0001;
    
    A1 = (pm(3)>=ep)*pm(3)+(pm(3)<ep)*0 ;
    A2 = (pm(4)>=ep)*pm(4)+(pm(4)<ep)*0 ;    
    
    
%    sst = sum((y-x).^2); r2 = 1-ssr/sst;
    
    btsample = bstrap(pm,cearly,clate,mearly,mlate,treatment,nboot);
    
    cov_disc = cov(btsample(:,1:2));
    
    se_beta = sqrt(cov_disc(1,1));
    se_delta = sqrt(cov_disc(2,2));
    
    cov_bd = cov_disc(1,2);
    
    se_A1 = std(btsample(:,3));
    se_A2 = std(btsample(:,4));
    
    pd = opdem(beta,delta,A1,A2,mearly,mlate,treatment);
    Pred((i-1)*100+1:i*100,:) = [repmat(id,[100 1]), (1:100)', treatment, pd, mearly, mlate]; 
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

    out = [id,beta,se_beta,delta,se_delta,cov_bd, A1, se_A1,A2, se_A2, ssr];
    
    results(i,:) = out;
    btsample_all = [btsample_all; id*ones(nboot,1), btsample];
 
    
save Results_1 results
save Bootsample_1 btsample_all
save Pred_1 Pred

m1=m1+M;
end
    


