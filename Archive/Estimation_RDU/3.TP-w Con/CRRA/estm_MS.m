
clc

load Data-Xlab-TPE.mat; 
data(:,9) = (data(:,7) >= 10 & data(:,8) >= 10);
dt = data(data(:,1) ~= 4 | (data(:,1) == 4 & data(:,9) == 1),:); 

n = size(unique(data(:,2)),1); 
nboot = 100;

btsample_all = []; 
Pred = []; 
results=zeros(n,10); 
    
    b = [0.8 1 1.1]; d = [0.8 0.95];
    rr = [0.01 0.1 0.5 2 6] ;
    nar = size(b',1)* size(d',1) * size(rr',1) ;
    vpm = zeros(nar,3); vssr = zeros(nar,1); 
    tvar={b,d,rr};
    [bb, dd, rrr]=ndgrid(tvar{:});
    var=[bb(:), dd(:), rrr(:)];
    clear b d rr bb dd rrr tvar;
    
options = optimset('Display','final');
LB = [0 0 0]; UB = [2 2 10];

m1=1;
for i = 1:n
    
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
    
    beta = pm(1); delta = pm(2); rho = pm(3) ;    
        
%    sst = sum((y-x).^2); r2 = 1-ssr/sst;
    
    btsample = bstrap(pm,cearly,clate,mearly,mlate,treatment,nboot);
    
    cov_disc = cov(btsample(:,1:2));
    
    se_beta = sqrt(cov_disc(1,1));
    se_delta = sqrt(cov_disc(2,2));
    
    cov_bd = cov_disc(1,2);
    
    se_rho = std(btsample(:,3));
    
    for j = 1:M
        
        mE = mearly(j); mL = mlate(j); mT = treatment(j);
        omega=0.01;
        pd = opdem_crra(beta,delta,rho,omega,mE,mL,mT);
        
        Pred = [Pred; id, j, mT, pd(1),pd(2), mE, mL]; 
    end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    

    results(i,:) = [id,beta,se_beta,delta,se_delta,cov_bd,rho, se_rho, ssr, M];
    
    btsample_all = [btsample_all; id*ones(nboot,1), btsample];
 
    
save Results results
save Bootsample btsample_all
save Pred Pred
m1=m1+M;
end

UserName = 'minseonp813@gmail.com';
passWord = 'tkrydbr12';
setpref('Internet','E_mail',UserName);
setpref('Internet','SMTP_Server','smtp.gmail.com');
setpref('Internet','SMTP_Username',UserName);
setpref('Internet','SMTP_Password',passWord);
props = java.lang.System.getProperties;
props.setProperty('mail.smtp.auth','true');
props.setProperty('mail.smtp.socketFactory.class', ...
                  'javax.net.ssl.SSLSocketFactory');
props.setProperty('mail.smtp.socketFactory.port','465');
emailto = 'minseonp813@gmail.com'; % recipient's email
sendmail(emailto, 'My Subject', 'My message','Results.mat');
    


