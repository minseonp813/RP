function y = psobjPanelD(x, fparameters)
 
% parameters
bootstrap_indicator = fparameters(:,1);
psum = fparameters(:,2);
d0 = fparameters(:,3);
g0 = fparameters(:,4);
nofsamples = fparameters(:,5);
schools = fparameters(:,6:11);
time = fparameters(:,15);
mij_emp = fparameters(:,16);

bootstrap_indicator = bootstrap_indicator{1,1};
psum = psum{1,1};
d0 = d0{1,1};
g0 = g0{1,1};
nofsamples = nofsamples{1,1};
time = time{1,1};
mij_emp = mij_emp{1,1};

% data
graphdata = fparameters(:,12);
distancedata1 = fparameters(:,13);
distancedata2 = fparameters(:,14);

graphdata = graphdata{1,1};
distancedata1 = distancedata1{1,1};
distancedata2 = distancedata2{1,1};

mij_sim = [];
for k = 1:1:length(schools)
    
    % 준비
    school = schools{1,k};
    my_field = strcat('school',num2str(school),'time',num2str(time));
    school_graphdata = graphdata.(my_field);
    school_distancedata1 = distancedata1.(my_field);
    school_distancedata2 = distancedata2.(my_field);
    
    node_female = school_graphdata.Nodes.female;
    node_classroom = school_graphdata.Nodes.classroom;
    
    schoolsize = size(school_distancedata2,1);
    if bootstrap_indicator == 0
        seqofnodes = 1:schoolsize; % this is for original data
        seqofnodes = seqofnodes';
    elseif bootstrap_indicator == 1
        seqofnodes = randsample(1:schoolsize,schoolsize,true); % this is for bootstrapping
        seqofnodes = seqofnodes';
        mij_emp = []; % reset the emprical moment
        % empirical moment 계산 
        for seqk = 1:1:length(seqofnodes)  

            nodei = seqofnodes(seqk, 1);

            % calculate pij
            outdegree = 0;
            n00 = 0;
            %
            %
            n11 = 0;
            for oppnodej = setdiff(1:schoolsize, nodei)
                % read homophily information
                ss = (node_female(nodei) == node_female(oppnodej));
                sc = (node_classroom(nodei) == node_classroom(oppnodej));
                if school_distancedata1(nodei, oppnodej) == 1 % use the original distance matrix
                    outdegree = outdegree + 1;
                    if sc == 0
                        n00 = n00+1; 
                    %
                    %
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
                temp = [n00, n11]/outdegree;
                temp = [temp, outdegree/10]; % add
                mij_emp = [mij_emp; temp];
            end    
        end
        mij_emp = nanmean(mij_emp);
    end
    
    % sample calculations
    for ksample=1:1:nofsamples
        
    adjacencymatrix_sim = zeros(length(seqofnodes), schoolsize); % there is no link between all nodes!
    for seqk = 1:1:length(seqofnodes)

    	nodei = seqofnodes(seqk, 1);
        
        for oppnodej = setdiff(1:schoolsize, nodei)
            
            % read homophily information
            ss = (node_female(nodei) == node_female(oppnodej));
            sc = (node_classroom(nodei) == node_classroom(oppnodej));
            
            % reset pseudo-homophily information
            if sc == 0
                rcost = betarnd(x(1), psum - x(1));    
            %
            %
            %
            %
            elseif sc == 1               
                rcost = betarnd(x(2), psum - x(2));                
            else               
                disp('something is wrong')              
            end

            if school_distancedata1(nodei, oppnodej) == 1 % if there is link from nodei to oppnodej
                current_value = d0 - rcost;         
                alternative_value = d0*(school_distancedata2(nodei, oppnodej))^g0; % there is no cost of maintaining a link
                if current_value >= alternative_value
                    adjacencymatrix_sim(seqk, oppnodej) = 1; % keep the current link
                end

            else % if there is no link       
                current_value = d0*(school_distancedata1(nodei, oppnodej))^g0; % there is no cost of maintaining a link
                alternative_value = d0 - rcost; 
                if alternative_value >= current_value
                    adjacencymatrix_sim(seqk, oppnodej) = 1; % create a link
                end

            end

        end
        
        outdegree = 0;
        n00 = 0;
        %
        %
        n11 = 0;
        for oppnodej = setdiff(1:schoolsize, nodei)
            % read homophily information
            ss = (node_female(nodei) == node_female(oppnodej));
            sc = (node_classroom(nodei) == node_classroom(oppnodej));
        	if adjacencymatrix_sim(seqk, oppnodej) == 1
                outdegree = outdegree + 1;
                if sc == 0
                    n00 = n00+1; 
                %
                %
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
            temp = [n00, n11]/outdegree;
            temp = [temp, outdegree/10]; %add
            mij_sim = [mij_sim; temp];
        end  
        
    end

    end
    
end

length(mij_sim);
mij_sim = nanmean(mij_sim);

e = mij_sim - mij_emp;

% function value - objective function
y = e*e';
