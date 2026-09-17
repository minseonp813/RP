% THIS CODE CALCULATES THE OPTIMAL DEMAND GIVEN ESTIMATED PARAMETERS
% DATA STRUCTURE AS FOLLOWS
% First Col.: id, Second col: round_number (only joint decisions)
% Third, Fourth Col.: price of x and y
% Fifth, Sixth Col.: CARA collective alpha, rho
% Seventh, Eighth Col.: CARA individual alpha, rho
% Ninth, Tenth Col.: CRRA collective alpha, rho
% Eleventh, Twelvth Col.: CARA individual alpha, rho

clear all
clc

data = readtable('data_daegu_collective.csv');

ncol = size(data(:,1),1);
dem_mmi_col = zeros(ncol, 2); dem_mmi_ind = zeros(ncol,2);
dem_rr_mmi_col=zeros(ncol,2); dem_rr_mmi_ind=zeros(ncol,2);

omega = 0.001;  %HPZ_No_Corners.m

for i= 1: ncol

dem_mmi_col(i,:)=opdemand(data.a(i),data.r(i),1/data.intercept_x(i),1/data.intercept_y(i));
dem_mmi_ind(i,:)=opdemand(data.a_ind(i),data.r_ind(i),1/data.intercept_x(i),1/data.intercept_y(i));
dem_rr_mmi_col(i,:)=opdemand_rr(data.a_crra(i),data.r_crra(i),omega,1/data.intercept_x(i),1/data.intercept_y(i));
dem_rr_mmi_ind(i,:)=opdemand_rr(data.a_crra_ind(i),data.r_crra_ind(i),omega,1/data.intercept_x(i),1/data.intercept_y(i));

end

filename = 'estdemand.csv';
A = [table2array(data(:,1:2)), dem_mmi_col, dem_mmi_ind, dem_rr_mmi_col, dem_rr_mmi_ind];
dlmwrite(filename,A,'delimiter', ',', 'precision', 20);