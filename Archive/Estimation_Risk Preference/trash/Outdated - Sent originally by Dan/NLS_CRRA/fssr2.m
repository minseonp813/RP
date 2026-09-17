function output = fssr2(pm,x,y,xmax,ymax)

% Compute the sum of Euclidean distance between observed data (x,y) and optimal demand given alpha and gamma

l = length(y);
alpha = pm(1); % Probability weighting: optimism (alpha < 1/2) / pessimism (alpha > 1/2)
rho = pm(2); % CARA coefficent

%h = 0.00000000000000001;
%if (rho < h)
%    rho = h;
%end

out = 0;

for i = 1:l   
    pred = opdemand(alpha,rho,1/xmax(i),1/ymax(i));
    out = out + abs((pred(2)/(pred(1)+pred(2)))-(y(i)/(x(i)+y(i))));
end

output = out;

end
