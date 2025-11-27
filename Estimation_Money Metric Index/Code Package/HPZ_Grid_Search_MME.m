function [Waste] = HPZ_Grid_Search_MME (prob_x,first_param,second_param,third_param,flag,max_x1,max_x2,utility,treatment,pref)

% Lion in the desert

num_of_iterations = 15;

Waste_UPPER = 1;

Waste_LOWER = 0;

Waste = 1/2;

param = zeros (1,2);

if pref == 1

param(1,1) = first_param;

if flag==1
    
    param(1,2) = second_param;
    
else
    
    param(1,2) = third_param;
    
end

elseif pref == 2
    
param(1,1) = first_param;

param(1,2) = second_param;    
    
end
    
for i=1:num_of_iterations
    
    temp_price = zeros(1,2);
    
    temp_price(1,1) = 1/(Waste*max_x1);
    
    temp_price(1,2) = 1/(Waste*max_x2);

    x = HPZ_Choices (temp_price,1,treatment,flag,1,param,pref);

    if pref==1
    
        u = HPZ_Utility(prob_x,x,first_param,second_param,third_param,flag);
        
    elseif pref == 2
        
        u = HPZ_OR_Utility (x, first_param, second_param, flag);
        
    end

    if utility-u>0
        Waste_LOWER = Waste;
    else
        Waste_UPPER = Waste;
    end 

    Waste = (1/2)*(Waste_LOWER + Waste_UPPER);        
end

end