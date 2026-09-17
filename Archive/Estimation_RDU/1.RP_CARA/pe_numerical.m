clc
[data] = xlsread('data_PE_numerical.xls');

% set parameters
tol=0.000001; ncol=size(data,1);

xm=zeros(ncol,2); xpe=zeros(ncol,2);
u=zeros(ncol,3); J=zeros(ncol,1);

for i=1:ncol;
    alpha=data(i,8); rho=data(i,9);
    
    % change all data to make x axes represents a cheaper good
    xc=[data(i,2),data(i,7)]*(data(i,4)>=data(i,5))+[data(i,7),data(i,2)]*(data(i,4)<data(i,5));
    px=1/data(i,4)*(data(i,4)>=data(i,5))+1/data(i,5)*(data(i,4)<data(i,5));
    py=1/data(i,5)*(data(i,4)>=data(i,5))+1/data(i,4)*(data(i,4)<data(i,5));
    
    xmi=data(i,10)*ones(1,2); % worst choice (lower initial value)
    xpi=[xc(2),(1-px*xc(2))*(1/py)]; % observed collective choice (upper initial value)
    
    u_xc=-alpha*exp(-rho*min(xc))-(1-alpha)*exp(-rho*max(xc));
    
for j=1:30 ;  %when J=30, no case is binding for J in searching xpi  
    xj=(xmi+xpi)/2 ;
    u_xj=-alpha*exp(-rho*min(xj))-(1-alpha)*exp(-rho*max(xj));
  
    if u_xj>u_xc-(u_xc)*tol;
    xpi=xj;
    elseif u_xj<u_xc; 
    xmi=xj;
    else
        break
    end;        
end;

J(i,:)=j;
u(i,:)=[u_xc,-alpha*exp(-rho*min(xmi))-(1-alpha)*exp(-rho*max(xmi)),-alpha*exp(-rho*min(xpi))-(1-alpha)*exp(-rho*max(xpi))];

% Mirror estimated values when y is cheaper in the original data
xm(i,:)=[xmi(1),xmi(2)]*(data(i,4)>=data(i,5))+[xmi(2),xmi(1)]*(data(i,4)<data(i,5)); 
xpe(i,:)=[xpi(1),xpi(2)]*(data(i,4)>=data(i,5))+[xpi(2),xpi(1)]*(data(i,4)<data(i,5)); 

end;

filename = 'numericalPE';
A = [data(:,1),data(:,6),xpe];

xlswrite(filename,A)