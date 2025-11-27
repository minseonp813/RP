function [mat] = HPZ_HZ_2016_Format_Data ()

% The function formats the data of CFGK (2007).
% It returns a matrix of data corresponding to the required treatment.
% The matrix has six columns:
% The first column is the subject ID.
% The second column is the observation number - 50 observations per subject
% The third column is the quantity of good 1 chosen by the subject.
% The fourth column is the quantity of good 2 chosen by the subject.
% The fifth column is the price of good 1. 
% The sixth column is the price of good 2. 

% Read the data of CFGK (2007) as published in the AER website 
% (http://www.aeaweb.org/articles.php?doi=10.1257/aer.97.5.1921). 
% The data file is located under the Data directory in the main directory
% of the downloadable zip file.

[data] = csvread('Data_HPZ_2016.csv');

% The KLP(2016) data:

% The first column is the session number while the second is the treatment
% number (first digit is the design type and second digit is the subject pool). 

% The third column is the universal subject ID: first two digits are the
% treatment number, the third digit is the session number, and the final
% two digits are running subject number.

% The fourth column is original subject ID: the first digit is the session
% number and the last two digits are the computer station.

mat = [data(:,1) data(:,2) data(:,3) data(:,4) 1./data(:,5) 1./data(:,6)];

% counter_mat = 1;
% 
% pilot = zeros(rows,6);
% 
% counter_pilot = 1;
% 
% % in the original data set the Y preceeds the X.
% 
% for i=1:rows
% 
%     if (data(i,1) == 1) && (data(i,2) == 11)
%         
%         pilot(counter_pilot,1) = data(i,3);
%         
%         pilot(counter_pilot,2:6) = [data(i,5) data(i,7) data(i,6) 1./data(i,9) 1./data(i,8)];
%         
%         counter_pilot = counter_pilot + 1;
%         
%     elseif (data(i,1) == 1) && (data(i,2) == 21)
%         
%         pilot(counter_pilot,1) = data(i,3);
%         
%         pilot(counter_pilot,2:6) = [data(i,5) data(i,7) data(i,6) 1./data(i,9) 1./data(i,8)];
%         
%         counter_pilot = counter_pilot + 1;
%         
%     elseif (data(i,1) == 2) && (data(i,2) == 21)
%         
%         pilot(counter_pilot,1) = data(i,3);
%         
%         pilot(counter_pilot,2:6) = [data(i,5) data(i,7) data(i,6) 1./data(i,9) 1./data(i,8)];
%         
%         counter_pilot = counter_pilot + 1;
%         
%     else
% 
%         mat(counter_mat,1) = data(i,3);
%         
%         mat(counter_mat,2:6) = [data(i,5) data(i,7) data(i,6) 1./data(i,9) 1./data(i,8)];
%         
%         counter_mat = counter_mat + 1;
% 
%     end   
%     
% end
% 
% mat( ~any(mat,2), : ) = [];
% 
% pilot( ~any(pilot,2), : ) = [];
% 
% 
% 
% 
