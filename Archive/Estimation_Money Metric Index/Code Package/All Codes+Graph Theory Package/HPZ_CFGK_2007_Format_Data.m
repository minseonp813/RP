function [mat] = HPZ_CFGK_2007_Format_Data (treatment)

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

[data] = csvread('Data_CFGK_2007.csv');

% The CFGK(2007) data:

% The first column is the session number while the second is the treatment
% number. 

% There were six sessions (1-6) and three treatments (1-3).

% We ignore the first session which is a pilot data that is ignored
% in the paper.

% In sessions 2 and 3 the treatment is Treatment 1 which gives the same
% probability to both states (the "Sym" treatment).

% Half is the name of the data file that includes only the data of the 47  
% subjects that participated in the symmetric treatment.

% Half has 2350 rows and 6 columns. 
% The first column is the subject ID (the ID column in CFGK's file) - 47
% subjects
% The second column is the observation number (the Round column in CFGK's 
% file) - 50 observations per subject
% The third column is the quantity of good 1 chosen by the subject (the X
% column in CFGK's file)
% The fourth column is the quantity of good 2 chosen by the subject (the Y
% column in CFGK's file)
% The fifth column is the price of good 1. Since the endowment is
% fixed at 1 the price is the reciprocal of the maximal
% quantity of good 1 (the X-max column in CFGK's file)
% The sixth column is the price of good 2. Again, the price is the 
% reciprocal of the maximal quantity of good 2 (the Y-max column in CFGK's file)

% in the original data set the Y preceeds the X.

Half = [data(901:3250,3) data(901:3250,5) data(901:3250,7) data(901:3250,6) 1./data(901:3250,9) 1./data(901:3250,8)];

% In session 4 the treatment is Treatment 2 which gives probability third 
% to the state where X is realized (the "Asym1" treatment).

% Third is the name of the data file that includes only the data of the 17  
% subjects that participated in the first asymmetric treatment.

% Third has 850 rows and 6 columns. 
% The first column is the subject ID (the ID column in CFGK's file) - 17
% subjects
% The second column is the observation number (the Round column in CFGK's 
% file) - 50 observations per subject
% The third column is the quantity of good 1 chosen by the subject (the X
% column in CFGK's file)
% The fourth column is the quantity of good 2 chosen by the subject (the Y
% column in CFGK's file)
% The fifth column is the price of good 1. We assume that the endowment is
% fixed at 1 and therefore the price is the reciprocal of the maximal
% quantity of good 1 (the X-max column in CFGK's file)
% The sixth column is the price of good 2. We assume that the endowment is
% fixed at 1 and therefore the price is the reciprocal of the maximal
% quantity of good 2 (the Y-max column in CFGK's file)

Third = [data(3251:4100,3) data(3251:4100,5) data(3251:4100,7) data(3251:4100,6) 1./data(3251:4100,9) 1./data(3251:4100,8)];

% In sessions 5 and 6 the treatment is Treatment 3 which gives probability 
% two thirds to the state where X is realized (the "Asym2" treatment).

% Two_Thirds is the name of the data file that includes only the data of 
% the 29 subjects that participated in the second asymmetric treatment.

% Two_Thirds has 1450 rows and 6 columns. 
% The first column is the subject ID (the ID column in CFGK's file) - 29
% subjects
% The second column is the observation number (the Round column in CFGK's 
% file) - 50 observations per subject
% The third column is the quantity of good 1 chosen by the subject (the X
% column in CFGK's file)
% The fourth column is the quantity of good 2 chosen by the subject (the Y
% column in CFGK's file)
% The fifth column is the price of good 1. We assume that the endowment is
% fixed at 1 and therefore the price is the reciprocal of the maximal
% quantity of good 1 (the X-max column in CFGK's file)
% The sixth column is the price of good 2. We assume that the endowment is
% fixed at 1 and therefore the price is the reciprocal of the maximal
% quantity of good 2 (the Y-max column in CFGK's file)

Two_Thirds = [data(4101:5550,3) data(4101:5550,5) data(4101:5550,7) data(4101:5550,6) 1./data(4101:5550,9) 1./data(4101:5550,8)];

% the requested treatment 

if treatment == 1
    mat=Half;
elseif treatment==2
    mat=Third;
elseif treatment==3
    mat=Two_Thirds;
% else
%     mat=simul;
end

