%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Reverse Estimate the Demand Using Estimated Parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clc

[data] = csvread('data_daegu_collective.csv');

ncol = size(data(:,1),1);
dem_nl_col = zeros(ncol, 2); dem_nl_ind = zeros(ncol,2);
dem_rr_nl_col=zeros(ncol,2); dem_rr_nl_ind=zeros(ncol,2);
dem_mmi_col = zeros(ncol, 2); dem_mmi_ind = zeros(ncol,2);
dem_rr_mmi_col=zeros(ncol,2); dem_rr_mmi_ind=zeros(ncol,2);

omega = 0.01;

for i= 1: ncol
    
dem_nl_col(i,:)=opdemand(data(i,7),data(i,8),1/data(i,5),1/data(i,6));
dem_nl_ind(i,:)=opdemand(data(i,9),data(i,10),1/data(i,5),1/data(i,6));
dem_rr_nl_col(i,:)=opdemand_rr(data(i,11),data(i,12),omega,1/data(i,5),1/data(i,6));
dem_rr_nl_ind(i,:)=opdemand_rr(data(i,13),data(i,14),omega,1/data(i,5),1/data(i,6));

dem_mmi_col(i,:)=opdemand(data(i,15),data(i,16),1/data(i,5),1/data(i,6));
dem_mmi_ind(i,:)=opdemand(data(i,17),data(i,18),1/data(i,5),1/data(i,6));
dem_rr_mmi_col(i,:)=opdemand_rr(data(i,19),data(i,20),omega,1/data(i,5),1/data(i,6));
dem_rr_mmi_ind(i,:)=opdemand_rr(data(i,21),data(i,22),omega,1/data(i,5),1/data(i,6));

end

filename = 'estdemand';
A = [data(:,1:2), dem_nl_col, dem_nl_ind, dem_rr_nl_col, dem_rr_nl_ind, dem_mmi_col, dem_mmi_ind, dem_rr_mmi_col, dem_rr_mmi_ind];
xlswrite(filename,A)