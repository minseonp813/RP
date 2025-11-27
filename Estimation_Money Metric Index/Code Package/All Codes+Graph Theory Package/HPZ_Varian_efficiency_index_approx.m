function [VARIAN, type] = HPZ_Varian_efficiency_index_approx (expenditure)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

global Threshold

global identical_choice

% initializations

max_exact = 12;

% number of observations

[rows,~] = size (expenditure);

average_var = 1;
         
%average_var_vec = zeros(rows,1);
         
meanssq_var = 1;
    
%meanssq_var_vec = zeros(rows,1);
    
min_var = 1;
    
%min_var_vec  = zeros(rows,1);


% Calculating the number of cycles in the data (cycles of size 2 for the
% case of two goods)

REF = diag(expenditure)*ones(rows,1)' - expenditure;
 
DRP = ceil((REF+Threshold)/(max(max(abs(REF+Threshold)))+1));
%SDRP = ceil((REF-Threshold)/(max(max(abs(REF-Threshold)))+1));

%Cycles=Johnson(SDRP'); % Use's Johnson's algorithm to extract/list cycles     
[Cycles, Components]=Johnson(DRP'); % Use's Johnson's algorithm to extract/list cycles     

DepMatrix_sf_sb=DependencyCFGK(Cycles,rows); % Create Dependency matrix

[cycles_num_sf_sb,cols] = size(DepMatrix_sf_sb);

%Discard self loops
DepMatrix_sb = zeros(sum(sum(DepMatrix_sf_sb,2) - ones(cycles_num_sf_sb,1)),cols);

cycles_num_sb = 0;

for i=1:cycles_num_sf_sb
   
    if sum(DepMatrix_sf_sb(i,:)) == 2
        
        cycles_num_sb = cycles_num_sb + 1;
        
        DepMatrix_sb(cycles_num_sb,:) = DepMatrix_sf_sb(i,:);
        
    end
    
end

%Discard identical choices
num_of_identical = sum(sum(identical_choice - eye(rows)))/2 ;

DepMatrix = zeros(cycles_num_sb-num_of_identical,cols);

if num_of_identical == 0

    DepMatrix = DepMatrix_sb;
    
    cycles_num = cycles_num_sb;
    
else 

    identical_pairs = zeros(num_of_identical,2);

    identical_index = 0;
    
    for i=1:rows
    
        for j=(i+1):rows
        
            if identical_choice(i,j) == 1
           
                identical_index = identical_index + 1;
            
                identical_pairs(identical_index,1) = i;
            
                identical_pairs(identical_index,2) = j;
            
            end

        end 

    end
    
    cycles_num = 0;
    
    for i=1:cycles_num_sb
        
        no_copy = 0;
       
        for j=1:num_of_identical
        
            if ((DepMatrix_sb(i,identical_pairs(j,1)) == 1) && (DepMatrix_sb(i,identical_pairs(j,2)) == 1))
               
                no_copy = 1;
                
            end
            
        end
        
        if no_copy == 0
            
            cycles_num = cycles_num + 1;
            
            DepMatrix(cycles_num,:) = DepMatrix_sb(i,:);
        
        end
        
    end
    
end

if cycles_num <= max_exact
    
    type = 2;

%The matrix RATIO has at the cell in the i'th row and the j'th
%column, the inverse of the ratio between the value of the bundle that was chosen in 
%observation i and the bundle that was chosen in observation j given the 
%prices of observation i
    
RATIO = expenditure./(diag(expenditure)*ones(rows,1)');


% Construct a matrix that representa all the subsets of possible budget line
% adjustments

check = zeros(2^(2*cycles_num),2*cycles_num+1);

for i=1:2^(2*cycles_num)
    
    num = i-1;
   
    for k=1:(2*cycles_num)
       
        check(i,(2*cycles_num+1)-k)= rem(num,2);
        
        num = (num-rem(num,2))/2;
        
    end
    
end

% Construct reference which is a vector of all relevant adjustments where 
% in each row the first column is the observation number and in the second
% column the size of the corresponding adjustment

total=zeros(cycles_num,cols);
    
counter=ones(1,rows);
    
for i=1:cycles_num

    for j=1:rows
    
        if (DepMatrix(i,j)==1)
            
            break
        
        end
        
    end
    
    for k=j+1:rows
        
        if (DepMatrix(i,k)==1)
            
           total(counter(1,k),k)=RATIO(k,j);
           
           total(counter(1,j),j)=RATIO(j,k);
           
           counter(1,k)=counter(1,k)+1;
           
           counter(1,j)=counter(1,j)+1;
           
        end
        
    end
    
end

max_count = max(max(counter));
    
num_obs = rows - sum(counter(:) == 1); 
    
relevant = zeros(max_count,num_obs);
    
rel_index = 1;
    
for i=1:cols
        
    if counter(1,i)>1
        
       relevant(1,rel_index) = i;
            
       for j=1:(counter(1,i)-1)
               
           relevant(j+1,rel_index) = total(j,i);
                
       end
            
       rel_index = rel_index + 1;
            
   end
        
end
    
relevant = [relevant(1,:);sort(relevant(2:max_count,:),1,'descend')];

[rel_rows,rel_cols] = size(relevant);

reference = zeros((2*cycles_num),2);

counter_ref = 1;

for k=1:rel_cols
    
   for m=2:rel_rows
       
       if relevant(m,k) > 0
          
           reference(counter_ref,1) = relevant (1,k);
           
           reference(counter_ref,2) = relevant (m,k);
           
           counter_ref = counter_ref + 1;
           
       end
       
   end
    
end

% Go over all the possible subsets and check if they relax the relations
% enough to satisfy GARP. If so, calculate the corresponding indices.

no_adjustments = ones(rows,1);

for i=1:(2^(2*cycles_num))
    
    adjustments = no_adjustments;
    
    if check(i,(2*cycles_num+1)) == 0
       
        for j=1:(2*cycles_num)
           
            if check(i,j) == 1
               
                adjustments(reference(j,1),1) = reference(j,2) - (10*Threshold);
                
            end
            
        end        
        
        [rows,cols] = size(expenditure);

        exp_var = expenditure - diag((diag(expenditure)).*(ones(rows,1)-adjustments));

        REF = diag(exp_var)*ones(rows,1)' - exp_var;

        DRP = ceil((REF+Threshold)/(max(max(abs(REF+Threshold)))+1));

        SDRP = ceil((REF-Threshold)/(max(max(abs(REF-Threshold)))+1));

        set_matlab_bgl_default(struct('full2sparse',1));

        NS_RP = all_shortest_paths(DRP);

        RP = eye(rows,rows);

        for j=1:rows

            for k=1:cols
        
                if ~isinf(NS_RP(j,k)) && ~(k==j)
            
                    RP(j,k)=1;
            
                end
        
            end
    
        end

        GARP = RP.*(SDRP');

        GARP_ERRORS = sum(sum(GARP));
    
        if GARP_ERRORS==0
    
            % if GARPE is satisfied we calculate the indices
    
            one_minus_v = (ones(rows,1)-adjustments);
     
            if average_var > mean(one_minus_v) 
         
                average_var = mean(one_minus_v);
         
                %average_var_vec = adjustments;
         
            end

            if meanssq_var > (sqrt(meansqr(one_minus_v))) 
         
                meanssq_var = (sqrt(meansqr(one_minus_v)));
         
                %meanssq_var_vec = adjustments;
         
            end     
     
            if min_var > max(one_minus_v)
         
                min_var = max(one_minus_v);
         
                %min_var_vec = adjustments;         
         
            end 
        
        end
        
    end
    
end

VARIAN = [min_var, average_var , meanssq_var];

else
    
% implementing the algorithm in Varian (1993) using the version in Alcantud
% et al (2010) (Algorithm 3). Note that the example in this paper is wrong.

var = ones(rows,1);

GARPE = 1;

while GARPE
    
    % Line 4
    exp_var = expenditure - diag((diag(expenditure)).*(ones(rows,1)-var));
    
    %The matrix REF has at the cell in the i'th row and the j'th
    %column, the difference between the value of the bundle that was chosen in 
    %observation i and the bundle that was chosen in observation j given the 
    %prices of observation i

    REF = diag(exp_var)*ones(rows,1)' - exp_var;
    
    %The matrix RATIO has at the cell in the i'th row and the j'th
    %column, the inverse of the ratio between the value of the bundle that was chosen in 
    %observation i and the bundle that was chosen in observation j given the 
    %prices of observation i
    
    RATIO = exp_var./(diag(exp_var)*ones(rows,1)');
    
    %The matrix DRP has at the cell in the i'th row and the j'th
    %column, 1 if and only if the bundle that was chosen in 
    %observation i is directly reveal prefered to the bundle that was chosen 
    %in observation j (the corresponding value in REF is greater than -1).

    DRP = ceil((REF+Threshold)/(max(max(abs(REF+Threshold)))+1));
    
    %The matrix SDRP has at the cell in the i'th row and the j'th
    %column, 1 if and only if the bundle that was chosen in 
    %observation i is strictly directly reveal prefered to the bundle that was chosen 
    %in observation j (the corresponding value in REF is greater than -1).

    SDRP = ceil((REF-Threshold)/(max(max(abs(REF-Threshold)))+1));

    % statement needed for the graph theory external package

    set_matlab_bgl_default(struct('full2sparse',1));

    %The matrix NS_RP has at the cell in the i'th row and the j'th
    %column, Inf if and only if the bundle that was chosen in 
    %observation i is not reveal prefered to the bundle that was chosen 
    %in observation j. Otherwise it includes a positive integer.

    NS_RP = all_shortest_paths(DRP);

    %Create RP

    RP = eye(rows,cols);

    %The matrix RP has at the cell in the i'th row and the j'th
    %column, 1 if and only if the bundle that was chosen in 
    %observation i is reveal prefered to the bundle that was chosen 
    %in observation j. 

    for j=1:rows
        for k=1:cols
            if ~isinf(NS_RP(j,k)) && ~(k==j)
                RP(j,k)=1;
            end
        end
    end

    %To test for GARP we will do the following: for every pair of
    %choices x and y if xRy then not yP0x. We will take RP and the transpose of
    %SDRP and multiply element by element. Every 1 correspondes to 
    %xRy and yP0x. The final matrix is the zero matrix if and only if 
    %GARP is satisfied. 

    GARP = RP.*(SDRP');

    GARP_ERRORS = sum(sum(GARP));

    %If the GARP matrix is only zeros then the data satisfies GARP
    if GARP_ERRORS==0
        GARPE=0;        
    else
        % Line 7 - Gv(x_j) is the jth column of GARP
        
        % Line 8 - Pert is a row vector. if the jth column is zero then Gv(x_j) is empty. 
        % otherwise it includes the number required in line 8.
        Pert_mat = GARP.*(RATIO');
        
        for j=1:rows
            for k=1:cols
                if Pert_mat(j,k)>=1
                    Pert_mat(j,k)=0;
                end
            end
        end
        
        % Line 9
        r = max(Pert_mat);
        
        [v,w] = max(r);
        
        % Line 10
        var(w) = v * var(w);
    end    
end

VARIAN = [max(1 - var), mean(1 - var) , (sqrt(meansqr(1 - var)))];

type = 3;

end

end

