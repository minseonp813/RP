function [VARIAN,Var_exact] = HPZ_Varian_efficiency_index (expenditure)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

global Threshold

global identical_choice

max_exact = 26;

[rows,~] = size (expenditure);

average_var = 1;
         
meanssq_var = 1;
    
min_var = 1;
    
REF = diag(expenditure)*ones(rows,1)' - expenditure;

RATIO = expenditure./(diag(expenditure)*ones(rows,1)');
 
DRP = ceil((REF+Threshold)/(max(max(abs(REF+Threshold)))+1));

% Use's Johnson's algorithm to extract/list cycles. Componets is a kxp mtx
% where k is the number of cyles and p is the number of prices (choices).
% Note that it doesn't present short cycles which are included in another
% longer ones.

[~, Components]=Johnson(DRP');     

SDRP = ceil((REF-Threshold)/(max(max(abs(REF-Threshold)))+1));

set_matlab_bgl_default(struct('full2sparse',1));

NS_RP = all_shortest_paths(DRP);

RP = zeros(rows);

for j=1:rows
    for k=1:rows
        if ~isinf(NS_RP(j,k))
            RP(j,k)=1;
        end
    end
end

GARP = RP.*(SDRP');

[rows_comp,~] = size(Components);

counter_adj = 0;

potential_adj = zeros (nchoosek(rows,2),2);

for i=1:rows_comp
    
    num_elements = sum(Components(i,:) > 0); % the number of elements included in cycle i
    
    pairs = nchoosek(Components(i,1:num_elements),2);
    
    for j=1:nchoosek(num_elements,2)
        
       GARP(pairs(j,1),pairs(j,2)) = 0;
       
       GARP(pairs(j,2),pairs(j,1)) = 0;
       
       if identical_choice(pairs(j,2),pairs(j,1)) == 0
           
           if RATIO(pairs(j,1),pairs(j,2)) < 1 - (10*Threshold)
           
                counter_adj = counter_adj + 1;
           
                potential_adj(counter_adj,1) = pairs(j,1);
          
                potential_adj(counter_adj,2) = RATIO(pairs(j,1),pairs(j,2));
                
           end
           
           if RATIO(pairs(j,2),pairs(j,1)) < 1 - (10*Threshold)
          
                counter_adj = counter_adj + 1;
           
                potential_adj(counter_adj,1) = pairs(j,2);
          
                potential_adj(counter_adj,2) = RATIO(pairs(j,2),pairs(j,1));
                
           end
          
       end
        
    end   
    
end

if sum(sum(GARP)) > 0
   
    [GARP_row,GARP_col,~] = find(GARP);
    
    for i=1:length(GARP_row)
        
       if identical_choice(GARP_row(i),GARP_col(i)) == 0
           
           if RATIO(GARP_row(i),GARP_col(i)) < 1 - (10*Threshold)
           
                counter_adj = counter_adj + 1;
           
                potential_adj(counter_adj,1) = GARP_row(i);
          
                potential_adj(counter_adj,2) = RATIO(GARP_row(i),GARP_col(i));
                
           end
           
           if RATIO(GARP_col(i),GARP_row(i)) < 1 - (10*Threshold)
          
                counter_adj = counter_adj + 1;
           
                potential_adj(counter_adj,1) = GARP_col(i);
          
                potential_adj(counter_adj,2) = RATIO(GARP_col(i),GARP_row(i));
                
           end
          
       end
        
    end    
    
end

if counter_adj <= max_exact

% Construct a matrix that represents all the subsets of possible budget line
% adjustments

check = zeros(2^counter_adj,counter_adj+1,'uint8');

for i=1:2^counter_adj
    
    num = i-1;
   
    for k=1:counter_adj
       
        check(i,(counter_adj+1)-k)= rem(num,2);
        
        num = (num-rem(num,2))/2;
        
    end
    
end

reference = sortrows(potential_adj(1:counter_adj,:));

% Go over all the possible subsets and check if they relax the relations
% enough to satisfy GARP. If so, calculate the corresponding indices.

no_adjustments = (1 - (10*Threshold)) * ones(rows,1);

for i=1:(2^counter_adj) 
    
    adjustments = no_adjustments;
    
    if check(i,(counter_adj+1)) == 0
       
        for j=1:counter_adj
           
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

            if meanssq_var > sqrt(meansqr(one_minus_v))
         
                meanssq_var = sqrt(meansqr(one_minus_v));
         
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

Var_exact = 1;

else
    
    [VARIAN,type] = HPZ_Varian_efficiency_index_approx (expenditure);
    
    Var_exact = type;
end

end

