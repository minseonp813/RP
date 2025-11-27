function [altered_data] = HPZ_No_Corners (data,obs_num,first)

% The function constructs a data matrix that has no corners, by using the 
% algorithm in CFGK (2007): 

% Small is measured by w (CFGK (2007))
w=0.001;

% disp(['Correcting for corners with omega equals ' , num2str(w)]);

altered_data = data;

for i=1:obs_num
    
    if data(i,first) < w*data(i,(first+1))
        
        altered_data(i,first) = w*data(i,(first+1));
        
    end

    if data(i,(first+1)) < w*data(i,first)
        
        altered_data(i,(first+1)) = w*data(i,first);
        
    end

end

end


