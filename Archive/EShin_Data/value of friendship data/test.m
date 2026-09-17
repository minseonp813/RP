% Suppose:
% 1) 'nodes' is an N×2 matrix where:
%    - nodes(:,1) contains IDs
%    - nodes(:,2) contains PIDs
% 2) 'links' is an M×2 matrix where each row [a, b] indicates
%    there is a link between node ID a and node ID b.


clear all

% Load your data (replace 'yourData.mat' with the actual file name)
load('graph_data.mat');  % This should load variables 'nodes' and 'links', or similar
pairlist = readtable('pairlist.csv');
strpairlist = string(pairlist{:,:});
pairlist = str2double(strpairlist);

schools = [11,12,13,14,15,16,21,22,23,24,25,26];

data = [];
for k = 1:1:1
%for k = 1:1:length(schools)

    school = schools(k);
    strschool = num2str(school);
    
    % get pairlist data
    % Check if the first two characters are "11"
    mask = startsWith(strpairlist, strschool);
    % Keep only rows where the condition is met
    pairlist_filtered = pairlist;
    pairlist_filtered(mask == 0) = NaN;
    pairlist_filtered = pairlist_filtered(~any(isnan(pairlist_filtered), 2), :);
    
    % get graph data
    my_field = strcat('school',num2str(school),'time0');
    gdata = graphdata.(my_field);
    edges = gdata.Edges;
    nodes = gdata.Nodes;
    
    temp_data = pairlist_filtered;
    k = 0;
    for i = 1:1:size(temp_data(:,1));
        
        a = temp_data(i,1);
        b = temp_data(i,2);
        
        idx_a = find(gdata.Nodes.oid == a);
        idx_b = find(gdata.Nodes.oid == b);
        
        
        if ~isempty(idx_a) && ~isempty(idx_b)
            d = distances(gdata, idx_a, idx_b);
            if isinf(d)
                disp('No path exists between the nodes.');
                
            else
                fprintf('The shortest path distance from %d to %d is %d.\n', a, b, d);
            end
        else
            disp('One or both nodes do not exist in the graph.');
            k = k+1;
        end
        
    end
    
    dd

    name = nodes.name;
    female = nodes.female;
    classroom = nodes.classroom;
    oid = nodes.oid;
    
    

    nodes.("indegree_id") = cell(length(name), 1);
    nodes.("outdegree_id") = cell(length(name), 1);
    nodes.("degree_id") = cell(length(name), 1);

    nodes.("indegree_partner") = cell(length(name), 1);
    nodes.("outdegree_partner") = cell(length(name), 1);
    nodes.("degree_partner") = cell(length(name), 1);
    
    nodes.("distance_id2partner") = cell(length(name), 1);
    nodes.("distance_partner2id") = cell(length(name), 1);
    nodes.("distance_id2partner_ud") = cell(length(name), 1);

    nodes.("nofmutual_k2iandj") = cell(length(name), 1);
    nodes.("nofmutual_iandjtok") = cell(length(name), 1);
    
    gdata.Nodes.oid
    
    dd

end

dd

% Extract unique IDs (assuming the first column of nodes is the ID)
uniqueIDs = nodes(:,1);

% Initialize a vector to store degree centralities
degreeCentrality = zeros(size(uniqueIDs));

% For each unique ID, count how many links it has
for i = 1:length(uniqueIDs)
    currentID = uniqueIDs(i);
    % Count the number of rows in 'links' where currentID appears
    degreeCentrality(i) = sum(links(:,1) == currentID | links(:,2) == currentID);
end

% Create the new data matrix:
%  - Column 1: ID
%  - Column 2: PID
%  - Column 3: Degree centrality
newData = [uniqueIDs, nodes(:,2), degreeCentrality];

% newData is now an N×3 matrix. 
% You can save or export 'newData' as needed:
% save('newData.mat','newData');