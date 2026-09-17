
clear 

disp('Do you want to start?')
pause

tic
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
disp('Run a simulation to get results in Panel C: same gender dimension only')
disp('**************************************************************')

load('nlcoefficients.mat')
coeff00 = nlcoefficients(1,:);
coeff01 = nlcoefficients(2,:);
coeff10 = nlcoefficients(3,:);
coeff11 = nlcoefficients(4,:);

% call the six schools
tschools = {11,14,16};
cschools = {21,24,26};

%%%%%%%% optimization part
lb = [eps, eps]; 
ub = [psum-eps, psum-eps];
A = [];
b = [];
Aeq = [];
beq = [];

% simulation start
tic

theta_PanelC = [];
theta_PanelCCI = [];
for treatment = 0:1:1

for time = 0:1:1 

treatment
time

%treatment = 1
%time = 1
    
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
        n0 = 0; % different class
        n1 = 0; % same class
        for oppnodej = setdiff(1:schoolsize, nodei)
            % read homophily information
            ss = (node_female(nodei) == node_female(oppnodej));
            %sc = (node_classroom(nodei) == node_classroom(oppnodej)); % same class indicator
        	if school_distancedata1(nodei, oppnodej) == 1 % use the original distance matrix
                outdegree = outdegree + 1;
                if ss == 0 
                    n0 = n0 + 1; 
                else              
                    n1 = n1 + 1;                         
                end
            end
        end
        
        if outdegree > 0
            temp = [n0, n1]/outdegree;
            temp = [temp, outdegree/10]; % add
            mij_emp = [mij_emp; temp];
        end    
    end
end

mij_emp = nanmean(mij_emp);

% 
disp('pause: end of calculating empirical moments')
%pause

%%%%%%%%%% find x0
% function setting
bootstrap_indicator = 0;
nofsamples = 100;

fparameters = [bootstrap_indicator, psum, d0, g0, nofsamples, schools, graphdata, distancedata1, distancedata2, time, mij_emp];
fun = @(x)psobjPanelC(x, fparameters);

% set options
options = optimoptions('patternsearch','UseParallel', true, 'Display', 'iter','UseCompletePoll', true, 'UseVectorized', false, 'PlotFcn',@psplotbestf); 
options.MeshTolerance = 1e-4;
options.FunctionTolerance = 1e-4;
options.InitialMeshSize = 1e-1;

% set x0
if     treatment == 0 && time == 0
    x0 = [5.8250, 3.8500];
elseif treatment == 0 && time == 1
    x0 = [6.0125, 4.0562];
elseif treatment == 1 && time == 0 
    x0 = [6.2406, 4.5438];
elseif treatment == 1 && time == 1
    x0 = [5.5000, 3.8500];
else
    disp('error')
end

%x0 = [psum/2, psum/2];

rep = 1;
while rep <= 1
[xoptimal, fval, exitflag, output] = patternsearch(fun, x0, A, b, Aeq, beq, lb, ub, options);
% x0 update
x0 = xoptimal
rep = rep+1;
end

temp_data = [treatment, time, xoptimal]
theta_PanelC = [theta_PanelC; temp_data];

% 
disp('pause: end of finding theta values')
%pause

%%%%%%%%%% bootstrapping
bootstrap_indicator = 1;
% simulation parameters
nofsamples = 20;
bootstrapnums = 500;

fparameters = [bootstrap_indicator, psum, d0, g0, nofsamples, schools, graphdata, distancedata1, distancedata2, time, mij_emp];
fun = @(x)psobjPanelC(x, fparameters);

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
    theta_PanelCCI = [theta_PanelCCI; temp_data];
end

save('results_PanelCCI.mat', 'theta_PanelCCI')

end

end
toc

data = theta_PanelC;

save('temp.mat','theta_PanelC')

% 
disp('puase: end of simulation. now calculate theta hat and CIs')
%pause

did = (data(4,:)-data(3,:)) - (data(2,:)-data(1,:));
theta_PanelC = [theta_PanelC;[0,0, did(1,3:4)]];

data = theta_PanelCCI;
theta0 = [data(0*bootstrapnums+1:1*bootstrapnums,3), data(bootstrapnums+1:2*bootstrapnums,3), data(2*bootstrapnums+1:3*bootstrapnums,3), data(3*bootstrapnums+1:4*bootstrapnums,3)];
theta0did = (theta0(:,4)-theta0(:,3))-(theta0(:,2)-theta0(:,1));
theta0ci = [quantile(theta0did,0.025),quantile(theta0did,0.975)]; 

theta1 = [data(0*bootstrapnums+1:1*bootstrapnums,4), data(bootstrapnums+1:2*bootstrapnums,4), data(2*bootstrapnums+1:3*bootstrapnums,4), data(3*bootstrapnums+1:4*bootstrapnums,4)];
theta1did = (theta1(:,4)-theta1(:,3))-(theta1(:,2)-theta1(:,1));
theta1ci = [quantile(theta1did,0.025),quantile(theta1did,0.975)]; 

CIs = [theta0ci; theta1ci];

save('results_PanelC.mat','theta_PanelC', 'theta_PanelCCI', 'theta0', 'theta1', 'CIs', 'theta0did', 'theta1did')

toc