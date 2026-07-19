clc
clear

load Bootsample_Feb122013.mat

dt = btsample_all;

n = length(dt)/500;

tSE_A = [];

for i = 1:n
    
    m1 = 500*(i-1)+1; m2 = 500*i;
    
    id = dt(m1,1);
    
    bootA = dt(m1:m2,4);
    
    se_A = std(bootA);
    
    tSE_A = [tSE_A;id, se_A];

end

save tSE_A_Feb142013 tSE_A
