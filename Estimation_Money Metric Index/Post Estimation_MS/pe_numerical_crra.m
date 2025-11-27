%%% THIS CODE SEARCH PARETO EFFICIENT CHOICE NUMERICALLY, FOR OBSERVED
%%% JOINT CHOICES THAT VIOLATES FOSD + ELATION LOVING. 
%%% WRITTEN BY MINSEON PARK, 12/28/2019
%%% CRRA CASES

%%% INPUT: [id, coord_x, coord_y, intercept_x, intercept_y, round_number, coord_y_real, alpha_ind, rho_ind, midpoint]
    %%% midpoint: coord_x at the middle on the budget set
%%% OUTPUT: [id, round_number, (numerically searched) PE Choice)

%%% THIS CODE SEARCH PARETO EFFICIENT CHOICE NUMERICALLY, FOR OBSERVED
%%% JOINT CHOICES THAT VIOLATES FOSD + ELATION LOVING. 
%%% WRITTEN BY MINSEON PARK, 12/28/2019

%%% INPUT: [id, coord_x, coord_y, intercept_x, intercept_y, round_number, coord_y_real, alpha_ind, rho_ind, midpoint]
    %%% midpoint: coord_x at the middle on the budget set
%%% OUTPUT: [id, round_number, (numerically searched) PE Choice)

clc
clear all

%% Setup
data = readtable('data_PE_numerical_crra.csv');

I = size(data,1); 
tol = 0.001; J = 10000; % maximum number of iteration
omega = 0.001;

u=zeros(I,3); ite = zeros(I,1); 

% Change data to make x axes represents a cheaper good
xo=[data.coord_x,data.coord_y].*(data.intercept_x>=data.intercept_y)...
        +[data.coord_y,data.coord_x].*(data.intercept_y>=data.intercept_x); % observed choice
p=[1./data.intercept_x,1./data.intercept_y].*(data.intercept_x>=data.intercept_y)...
        +[1./data.intercept_y,1./data.intercept_x].*(data.intercept_y>=data.intercept_x); % price vector

xl=data.x_w2.*ones(I,2); % worst choice (lower initial value)
xu=[xo(:,2),(1-p(:,1).*xo(:,2)).*(1./p(:,2))]; % upper initial value


%% Iteration
for i=1:I
    alpha = data.a_ind(i); rho = data.r_ind(i);       
    
    u_xc= (alpha*(min(xo(i,:))+omega)^(1-rho)+(1-alpha)*(max(xo(i,:))+omega)^(1-rho))/ (1-rho); % utility at the observed choice
    
    for j=1:J  
        xj=(xl(i,:)+xu(i,:))/2 ;
        u_xj=(alpha*(min(xj)+omega)^(1-rho)+(1-alpha)*(max(xj)+omega)^(1-rho))/ (1-rho);
        
        if u_xj > u_xc
        xu(i,:)=xj;
        elseif u_xj < u_xc
        xl(i,:)=xj;
        else
            break
        end     
    end

ite(i)=j;
u(i,:)=[u_xc,(alpha*(min(xl(i,:))+omega)^(1-rho)+(1-alpha)*(max(xl(i,:))+omega)^(1-rho))/ (1-rho),...
            (alpha*(min(xu(i,:))+omega)^(1-rho)+(1-alpha)*(max(xu(i,:))+omega)^(1-rho))/ (1-rho)];
  
end


%% Mirror estimated values when y is cheaper in the original data
xpe=[xu(:,1),xu(:,2)].*(data.intercept_x>=data.intercept_y)...
        +[xu(:,2),xu(:,1)].*(data.intercept_x<data.intercept_y);  
    
filename = 'numericalPE_crra';
A = [data.id,data.round_number,xpe];

xlswrite(filename,A)
