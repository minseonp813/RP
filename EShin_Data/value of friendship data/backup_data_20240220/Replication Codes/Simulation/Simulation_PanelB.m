
clear 

% load important files from regression analysis
temp_load = load('graph_data.mat');
graphdata = temp_load.graphdata;
distancedata1 = temp_load.distancedata1;
distancedata2 = temp_load.distancedata2;
 
clear temp_load

% set a random seed
rng(1)

% simulation parameters
psum = 10;

% get an estimate by using the original network dataset
disp('**************************************************************')
disp('Run a simulation to get results in Panel B')
disp('**************************************************************')

load('nlcoefficients.mat')
coeff00 = nlcoefficients(5,:);
coeff01 = nlcoefficients(6,:);
coeff10 = nlcoefficients(7,:);
coeff11 = nlcoefficients(8,:);

tschools = {13,15};
cschools = {23,25};

%%%%%%%% optimization part
lb = [eps, eps, eps];
ub = [psum-eps, psum-eps, psum-eps];
A = [];
b = [];
Aeq = [];
beq = [];

% simulation start
tic

theta_PanelB = [];
theta_PanelBCI = [];
for treatment = 0:1:1

for time = 0:1:1 

treatment
time
    
%%%%%%%%%% set school codes
if treatment == 1
    schools = tschools;
else
    schools = cschools;
end

% coefficients
if treatment == 0 && time == 0
    d0 = coeff00(1); % treatment = 0 & time = 0
    g0 = coeff00(2); % treatment = 0 & time = 0
elseif treatment == 0 && time == 1
    d0 = coeff01(1); % treatment = 0 & time = 1
    g0 = coeff01(2); % treatment = 0 & time = 1
elseif treatment == 1 && time == 0 
    d0 = coeff10(1); % treatment = 1 & time = 0
    g0 = coeff10(2); % treatment = 1 & time = 0
elseif treatment == 1 && time == 1
    d0 = coeff11(1); % treatment = 1 & time = 1
    g0 = coeff11(2); % treatment = 1 & time = 1
else
    disp('error')
end

% target moment calculations
mij_emp = [];
for k = 1:1:length(schools)
    
    school = schools{1,k};
    my_field = strcat('school',num2str(school),'time',num2str(time));
    school_graphdata = graphdata.(my_field);
    school_distancedata1 = distancedata1.(my_field);
    school_distancedata2 = distancedata2.(my_field);
    
    node_female = school_graphdata.Nodes.female;
    node_classroom = school_graphdata.Nodes.classroom;
    
    schoolsize = size(school_distancedata2,1);
    seqofnodes = 1:schoolsize; % this is for original data
    seqofnodes = seqofnodes';
    
    % empirical moment calculations
    for seqk = 1:1:length(seqofnodes)  
        
    	nodei = seqofnodes(seqk, 1);
        
        % calculate pij
        outdegree = 0;
        n00 = 0;
        n10 = 0;
        %
        n11 = 0;
        for oppnodej = setdiff(1:schoolsize, nodei)
            % read homophily information
            ss = (node_female(nodei) == node_female(oppnodej));
            sc = (node_classroom(nodei) == node_classroom(oppnodej));
        	if school_distancedata1(nodei, oppnodej) == 1 % use the original distance matrix
                outdegree = outdegree + 1;
                if [ss, sc] == [0,0]  
                    n00 = n00+1; 
                elseif [ss, sc] == [1,0]
                    n10 = n10+1;   
                %    
                %    
                elseif sc == 1        
                    n11 = n11+1;             
                else               
                    disp('something is wrong')              
                end
            end
        end
        
        if outdegree > 0
            temp = [n00, n10, n11]/outdegree;
            temp = [temp, outdegree/10]; % add
            mij_emp = [mij_emp; temp];
        end    
    end
end

mij_emp = nanmean(mij_emp);

%%%%%%%%%% find x0
% function setting
bootstrap_indicator = 0;
nofsamples = 100;

fparameters = [bootstrap_indicator, psum, d0, g0, nofsamples, schools, graphdata, distancedata1, distancedata2, time, mij_emp];
fun = @(x)psobjPanelB(x, fparameters);

% set options
options = optimoptions('patternsearch','UseParallel', false, 'Display', 'iter','UseCompletePoll', true, 'UseVectorized', false, 'PlotFcn',@psplotbestf); 
options.MeshTolerance = 1e-4;
options.FunctionTolerance = 1e-4;
options.InitialMeshSize = 1e-1;

% set x0
if treatment == 0 && time == 0
    x0 = [6.16249999999999,4.24375000000000,2.61875000000000];
elseif treatment == 0 && time == 1
    x0 = [5.77500000000000,4.27187500000000,2.44375000000000];
elseif treatment == 1 && time == 0 
    x0 = [5.89218750000000,4.03750000000000,2.42343750000000];
elseif treatment == 1 && time == 1
    x0 = [5.76250000000000,3.79218750000000,1.95625000000000];
else
    disp('error')
end
%x0 = [psum/2, psum/2, psum/2];

rep = 1;
while rep <= 1
[xoptimal, fval, exitflag, output] = patternsearch(fun, x0, A, b, Aeq, beq, lb, ub, options);
% x0 update
x0 = xoptimal
rep = rep+1;
end

temp_data = [treatment, time, xoptimal]
theta_PanelB = [theta_PanelB; temp_data];

%%%%%%%%%% 부트스랩 계산
% function setting
bootstrap_indicator = 1;
nofsamples = 20;
bootstrapnums = 500;

fparameters = [bootstrap_indicator, psum, d0, g0, nofsamples, schools, graphdata, distancedata1, distancedata2, time, mij_emp];
fun = @(x)psobjPanelB(x, fparameters);

% set options
options = optimoptions('patternsearch','UseParallel', false, 'UseCompletePoll', true, 'UseVectorized', false); 
options.MeshTolerance = 1e-4;
options.FunctionTolerance = 1e-4;
options.InitialMeshSize = 1e-1;

parfor boot = 1:1:bootstrapnums
    % run simulation
    [xoptimal, fval, exitflag, output] = patternsearch(fun, x0, A, b, Aeq, beq, lb, ub, options);
    temp_data = [treatment, time, xoptimal];
    % save the data
    theta_PanelBCI = [theta_PanelBCI; temp_data];
end

save('results_PanelBCI.mat', 'theta_PanelBCI')

end

end
toc

data = theta_PanelB;
did = (data(4,:)-data(3,:)) - (data(2,:)-data(1,:));
theta_PanelB = [theta_PanelB;[0,0, did(1,3:5)]];

data = theta_PanelBCI
theta00 = [data(0*bootstrapnums+1:1*bootstrapnums,3), data(bootstrapnums+1:2*bootstrapnums,3), data(2*bootstrapnums+1:3*bootstrapnums,3), data(3*bootstrapnums+1:4*bootstrapnums,3)];
theta00did = (theta00(:,4)-theta00(:,3))-(theta00(:,2)-theta00(:,1));
theta00ci = [quantile(theta00did,0.025),quantile(theta00did,0.975)];  

theta10 = [data(0*bootstrapnums+1:1*bootstrapnums,4), data(bootstrapnums+1:2*bootstrapnums,4), data(2*bootstrapnums+1:3*bootstrapnums,4), data(3*bootstrapnums+1:4*bootstrapnums,4)];
theta10did = (theta10(:,4)-theta10(:,3))-(theta10(:,2)-theta10(:,1));
theta10ci = [quantile(theta10did,0.025),quantile(theta10did,0.975)]; 

%
%
%

theta11 = [data(0*bootstrapnums+1:1*bootstrapnums,5), data(bootstrapnums+1:2*bootstrapnums,5), data(2*bootstrapnums+1:3*bootstrapnums,5), data(3*bootstrapnums+1:4*bootstrapnums,5)];
theta11did = (theta11(:,4)-theta11(:,3))-(theta11(:,2)-theta11(:,1));
theta11ci = [quantile(theta11did,0.025),quantile(theta11did,0.975)]; 

CIs = [theta00ci; theta10ci; theta11ci];

save('results_PanelB.mat','theta_PanelB', 'theta_PanelBCI', 'theta00', 'theta10', 'theta11', 'CIs', 'theta00did', 'theta10did', 'theta11did')
