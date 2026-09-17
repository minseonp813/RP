%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Reverse Estimate the Demand Using Estimated Parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clc

load data_daegu_param.mat ; 

ncol = size(VarName1,1);
dem = zeros(ncol, 2);
dem_cor = zeros(ncol,2);
dem_ind = zeros(ncol,2);
dem_ind_cor = zeros(ncol,2);

% id, game_type, x_coord, y_coord, x_intercept, y_intercept, alpha, rho,
% alpha_cor, rho_cor, alph_ind, rho_ind, alpha_cor_ind, rho_cor_ind,
% original distance(will be added)
for i= 1: ncol
    
dem(i,:)=opdemand(VarName7(i),VarName8(i),1/VarName5(i),1/VarName6(i));
dem_cor(i,:)=opdemand(VarName9(i),VarName10(i),1/VarName5(i),1/VarName6(i));

dem_ind(i,:)=opdemand(VarName11(i),VarName12(i),1/VarName5(i),1/VarName6(i));
dem_ind_cor(i,:)=opdemand(VarName13(i),VarName14(i),1/VarName5(i),1/VarName6(i));

end

filename = 'data_daegu_dem';
A = [VarName1 VarName2 VarName3 VarName4 VarName5 VarName6 VarName7 VarName8 VarName9 VarName10 VarName11 VarName12 VarName13 VarName14 dem dem_cor dem_ind dem_ind_cor];
xlswrite(filename,A)