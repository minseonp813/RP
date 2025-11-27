function output = fssr2(pm,x,y,xmax,ymax)

% Compute the sum of Euclidean distance between observed data (x,y) and optimal demand given alpha and gamma

n = length(y);
alpha = pm(1); % Probability weighting: optimism (alpha < 1/2) / pessimism (alpha > 1/2)
rho = pm(2); % CARA coefficent

%h = 0.00000000000000001;
%if (rho < h)
%    rho = h;
%end

out = 0;

for i = 1:n   
    pred = opdemand(alpha,rho,1/xmax(i),1/ymax(i));
    
    if (xmax(i) == ymax(i))
        
        term1 = ((pred(1) - x(i))^2 + (pred(2) - y(i))^2)^(1/2);
        term2 = ((pred(1) - y(i))^2 + (pred(2) - x(i))^2)^(1/2);
        
        term = min(term1, term2);
        
        out = out + term;
             
    else
    
        out = out + ((pred(1) - x(i))^2 + (pred(2) - y(i))^2)^(1/2);
        
    end
end

output = out;

end



