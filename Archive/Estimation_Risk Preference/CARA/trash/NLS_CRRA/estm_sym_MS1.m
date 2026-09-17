%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Nonlinear least squares (NLS) Estimation with CARA specification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clc

% Load the data file
% data_daegu has six column vectors. The first column is about subject ID and
% the second is about the number of decision rounds.
% The third to the six column contain the decisions and the parameter in the budget set,
% (id, round, x, y, xmax, ymax). 

load Riskchoices.mat ; 

n = unique(VarName1); % n is the number of subjects or pairs
N= size(n,1);

options = optimset('Display','final');

results = zeros(1.5*N,7); bt_results = [];
   
    a = [0.1 0.3 0.5 0.7 0.9] ;
    r = [0.01 0.1 0.5 2 6] ;
    nar = size(a',1)*size(r',1);
    vpm = zeros(nar,2); vssr = zeros(nar,1); 
    tvar={a,r};
    [aa,rr]=ndgrid(tvar{:});
    var=[aa(:), rr(:)];
    
global lb
global ub

    lb=[0.5 0]; ub=[0.5 5];
    
global M 

m1 = 1;
for i =  1:1.5*N
    [~,~,m] = find(VarName1==VarName1(m1) & VarName7==VarName7(m1)) ;
    M=sum(m);
    m2=m1+M-1 ;
    
    % Call the demands in x and y accounts and the x and y intercepts in the budget sets
   
    id = VarName1(m1)
    x = VarName3(m1:m2); y = VarName4(m1:m2); 
    xmax = VarName5(m1:m2); ymax = VarName6(m1:m2);
    ry = y./(x+y);
    
    if sum((1./xmax>1./ymax).*(ry>0.95)+(1./xmax<1./ymax).*(ry<0.05)) >= 18    
    alpha=0.5; se1=0; rho=0; se2=0; 
    ssr=sum((1./xmax>1./ymax).*(1-ry)+(1./xmax<=1./ymax).*(ry-0));
    results(i,:) = [VarName1(m1),VarName7(m1),alpha,se1,rho,se2,ssr];
    
    elseif sum((0.45<ry).*(ry<0.55)) >= 18
    alpha=0.5; se1=0; rho=5; se2=0;
    ssr=sum(abs(ry-0.5));
    results(i,:) = [VarName1(m1),VarName7(m1),alpha,se1,rho,se2,ssr];
    
    else
%%%%%%%%%%%%%%%%%%%%%%%%%%%% The NLLS estimation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    for j=1:nar
    [vpm(j,:),vssr(j)] = fmincon(@fssr2,var(j,:),[],[],[],[],lb,ub,[],options,x,y,xmax,ymax);
    end
    
    ssrmin = min(vssr);
    eps = 0;
     
    mpm = vpm(vssr<=ssrmin+eps,:);
    mssr = vssr(vssr<=ssrmin+eps);
    
    kr= size(mpm,1);  
   
    t = round(0.5+kr*rand(1));
    
    pm = mpm(t,:); ssr = mssr(t);
    
    alpha = pm(1); rho = pm(2);
       
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% Estimation Results for Subject id
% log alpha and Rho, their standard errors (using a boostrap method), p-value (5% significance level) under the null of log alpha equal to zero
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    btout = bstrapm1(pm,x,y,xmax,ymax,200);
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

    results(i,:) = [VarName1(m1),VarName7(m1),alpha,se1,rho,se2,ssr];
    
    end
%    bt_results = [bt_results;btres];   
%    save bt_results bt_results
m1=m1+M ;  
  
     save CRRA_alpha05_rho_0_5 results
end

%UserName = 'minseonp813@gmail.com';
%passWord = 'tkrydbr12';
%setpref('Internet','E_mail',UserName);
%setpref('Internet','SMTP_Server','smtp.gmail.com');
%setpref('Internet','SMTP_Username',UserName);
%setpref('Internet','SMTP_Password',passWord);
%props = java.lang.System.getProperties;
%props.setProperty('mail.smtp.auth','true');
%props.setProperty('mail.smtp.socketFactory.class', ...
%'javax.net.ssl.SSLSocketFactory');
%props.setProperty('mail.smtp.socketFactory.port','465');
%emailto = 'minseonp813@gmail.com'; % recipient's email
%sendmail(emailto, 'My Subject', 'My message','results1210.mat');


