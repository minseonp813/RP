function [mat] = Daegu_2016_Format_Data ()

% The function formats the data of daegu (2016).
% It returns a matrix of data corresponding to the required treatment.
% The matrix has six columns:
% The first column is the subject ID.
% The second column is the observation number - 50 observations per subject
% The third column is the quantity of good 1 chosen by the subject.
% The fourth column is the quantity of good 2 chosen by the subject.
% The fifth column is the price of good 1. 
% The sixth column is the price of good 2. 

[data] = xlsread('Data_daegu_2016.xls');

% The daegu(2016) data: Contataining both individual and collective choice
% data. Especially on joint decision, only one out of two members' data are
% included (such modification is done using Stata)

% The first column is the universal subejec ID, the second is round number.
% The third and fourth are choice of (x1,x2) and fifth and sixth are their
% prices. 
% The last is for game type: 1 if individual 2 if group

% The first column now represents (the original id, game_type)

data(:,1) = data(:,1)*10+data(:,7);

data(:,2) = data(:,2)-18*(data(:,2)>18);
    
mat = [data(:,1) data(:,2) data(:,3) data(:,4) 1./data(:,5) 1./data(:,6)];
