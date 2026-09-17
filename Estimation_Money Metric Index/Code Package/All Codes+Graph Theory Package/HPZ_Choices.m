function [choice_matrix] = HPZ_Choices (prices,endowments,treatment,function_flag,asymmetric_flag,param,pref)
% The input is:
% 1. prices - a matrix. The number of rows is the number of observations while the number of columns is the number of goods.
%    each row is a vector of prices (one for each good).
% 2. endowment - a column vector. The number of elements is the number of observations. Each row is a number.
% 3. treatmentis the number of treatment in CFGK (2007).
% 4. flag is the type of utility function chosen for the prizes:
%    1 - CRRA.
%    2 - CARA.
% 5. mode is the type of corrections implemented on the NLLS algorithm of 
%    CFGK (2007): 
%    1 - As Is.
%    2 - Only for the asymmetric cases, using the correct DA function.
%    3 - Correct DA function and bounding alpha from below by zero. Using our
%     implememntation, it translates into allowing beta to be negative 
%     (but greater than -1).
%    4 - Correct DA function, bounding alpha from below by zero and using the 
%     Euclidean metric for the criterion. 
%    5 - Correct DA function, bounding alpha from below by zero, using the 
%     Euclidean metric and leaving corner data points unchanged. 
% 6. param - a vector parameters for the utility function
% The output is a a matrix. The number of rows is the number of observations while the number of columns is the number of goods.
% Each row is a vector of choices (one quantity for each good).

if pref == 1

prob_x=1/2;

if asymmetric_flag==1
    if treatment==2
        prob_x=1/3;
    elseif treatment==3
        prob_x=2/3;
    end
end

[observations,~]=size(prices);

choice_matrix=zeros(observations,2);

if function_flag==1

	parfor i=1:observations       
        
        % Threshold initialization
        % Thresholds for acceptable difference between optimal solutions
        ratio_diff = 0.05;
        Diff_threshod = 10^(-2);
        
        %% Choose two points A and B near the boundaries from the
        %% budget line:
        
        % compute the slope of the budget line
        x_1_init = endowments(i)/prices(i,1);
        
        % The (2x2) matrix of two initial points: X_init
        X_init = zeros(2,2);
        
        % Point A - The 1st row of the X_init matrix:
        % Divide the budget line to 1001 pieces and take the 102nd point:       
        X_init(1,1) = (102/(1000+1)) * x_1_init; % (x1,_)
        X_init(1,2) = (endowments(i)-(X_init(1,1)*prices(i,1)))/prices(i,2); % (_,x2)
        
        % Point B - The 2nd row of the X_init matrix:
        % Divide the budget line to 1001 pieces and take the 898th point:
        X_init(2,1) = (898/(1000+1)) * x_1_init;  % (x1,_)
        X_init(2,2) = (endowments(i)-(X_init(2,1)*prices(i,1)))/prices(i,2); % (_,x2)
        
        opts_phase1 = optimset('Algorithm','sqp','Display','off');        
        
        % Optimal solution matrix (2x3) for point A and B: [x1,x2,fval]
        Optimal_AB = zeros(2,3);
        
        % Run fmincon using:
        % 1. Default HPZ_Utility_Helper function as objective function
        % 2. Subject to EQUALITY constraint: Ax = b
        % 3. lower bound set to : [0 0]
        % 4. TolX = 0
        % 5. Iinitial points set to A=[X_init(1,:)] AND B=[X_init(2,:)]
        [Optimal_AB(1, 1:2), Optimal_AB(1, 3)] = fmincon (@(x) HPZ_Utility_Helper(prob_x,x,param(1),param(2),0,function_flag), X_init(1,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1);
        [Optimal_AB(2, 1:2), Optimal_AB(2, 3)] = fmincon (@(x) HPZ_Utility_Helper(prob_x,x,param(1),param(2),0,function_flag), X_init(2,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1);

        Diff_Xopts_AB = abs(Optimal_AB(1,1:2) - Optimal_AB(2,1:2));
        ratio_Xopts_AB = abs((Optimal_AB(1,1:2) ./ Optimal_AB(2,1:2)) - 1);
        
        % if the distance between two optimal solutions is less than the
        % threshold:
        if (((Diff_Xopts_AB(1) <= Diff_threshod) && (Diff_Xopts_AB(2) <= Diff_threshod)) || ...
            (ratio_Xopts_AB(1) <= ratio_diff)    && (ratio_Xopts_AB(1) <= ratio_diff))
        else
            % compute the minimum between these two optimal values
            Fval_Diff_phase1 = abs(Optimal_AB(1,3) - Optimal_AB(2,3));

            if (Fval_Diff_phase1 > 0)
                % Alpha_Factor:
                % Fval Multiplier to remove the effect of penalizer factor
                % inside the score function:
                % utility multiplier (used inside GlobalSearch objective
                % function)
                if (Fval_Diff_phase1 >= 1)
                    Alpha_Factor = 10^(6);
                else
                    % compute the number of number of digits after the "0."
                    digit_num = ceil(abs(log10(Fval_Diff_phase1)));
                    % Fval Multiplier to remove the effect of penalizer factor inside the score
                    % function:
                    Alpha_Factor = 10^(digit_num + 5);
                end

                [Optimal_AB(1, 1:2), Optimal_AB(1, 3)] = fmincon (@(x) HPZ_Utility_Helper(prob_x,x,param(1),param(2),0,function_flag,Alpha_Factor), X_init(1,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1);
                [Optimal_AB(2, 1:2), Optimal_AB(2, 3)] = fmincon (@(x) HPZ_Utility_Helper(prob_x,x,param(1),param(2),0,function_flag,Alpha_Factor), X_init(2,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1); %#ok<*PFBNS>

            end
        end
        
        [~,index_tt] = min(Optimal_AB(:,3));
        choice_matrix(i,:)=Optimal_AB(index_tt,1:2);
        
	end
   
else   
    
    %% CARA
    parfor i=1:observations       
        
        % Threshold initialization
        % Thresholds for acceptable difference between optimal solutions
        ratio_diff = 0.05;
        Diff_threshod = 10^(-2);
        
        %% Choose two points A and B near the boundaries from the
        %% budget line:
        
        % compute the slope of the budget line
        x_1_init = endowments(i)/prices(i,1);
        
        % The (2x2) matrix of two initial points: X_init
        X_init = zeros(2,2);
        
        % Point A - The 1st row of the X_init matrix:
        % Divide the budget line to 1001 pieces and take the 102nd point:       
        X_init(1,1) = (102/(1000+1)) * x_1_init; % (x1,_)
        X_init(1,2) = (endowments(i)-(X_init(1,1)*prices(i,1)))/prices(i,2); % (_,x2)
        
        % Point B - The 2nd row of the X_init matrix:
        % Divide the budget line to 1001 pieces and take the 898th point:
        X_init(2,1) = (898/(1000+1)) * x_1_init;  % (x1,_)
        X_init(2,2) = (endowments(i)-(X_init(2,1)*prices(i,1)))/prices(i,2); % (_,x2)
        
        opts_phase1 = optimset('Algorithm','sqp','Display','off');        
        
        % Optimal solution matrix (2x3) for point A and B: [x1,x2,fval]
        Optimal_AB = zeros(2,3);
        
        % Run fmincon using:
        % 1. Default HPZ_Utility_Helper function as objective function
        % 2. Subject to EQUALITY constraint: Ax = b
        % 3. lower bound set to : [0 0]
        % 4. TolX = 0
        % 5. Iinitial points set to A=[X_init(1,:)] AND B=[X_init(2,:)]
        [Optimal_AB(1, 1:2), Optimal_AB(1, 3)] = fmincon (@(x) HPZ_Utility_Helper(prob_x,x,param(1), 0, param(2),function_flag), X_init(1,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1);
        [Optimal_AB(2, 1:2), Optimal_AB(2, 3)] = fmincon (@(x) HPZ_Utility_Helper(prob_x,x,param(1), 0, param(2),function_flag), X_init(2,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1);

        Diff_Xopts_AB = abs(Optimal_AB(1,1:2) - Optimal_AB(2,1:2));
        ratio_Xopts_AB = abs((Optimal_AB(1,1:2) ./ Optimal_AB(2,1:2)) - 1);
        
        % if the distance between two optimal solutions is less than the
        % threshold:
        if (((Diff_Xopts_AB(1) <= Diff_threshod) && (Diff_Xopts_AB(2) <= Diff_threshod)) || ...
            (ratio_Xopts_AB(1) <= ratio_diff)    && (ratio_Xopts_AB(1) <= ratio_diff))
        else
            % compute the minimum between these two optimal values
            Fval_Diff_phase1 = abs(Optimal_AB(1,3) - Optimal_AB(2,3));

            if (Fval_Diff_phase1 > 0)
                % Alpha_Factor:
                % Fval Multiplier to remove the effect of penalizer factor
                % inside the score function:
                % utility multiplier (used inside GlobalSearch objective
                % function)
                if (Fval_Diff_phase1 >= 1)
                    Alpha_Factor = 10^(6);
                else
                    % compute the number of number of digits after the "0."
                    digit_num = ceil(abs(log10(Fval_Diff_phase1)));
                    % Fval Multiplier to remove the effect of penalizer factor inside the score
                    % function:
                    Alpha_Factor = 10^(digit_num + 5);
                end

                [Optimal_AB(1, 1:2), Optimal_AB(1, 3)] = fmincon (@(x) HPZ_Utility_Helper(prob_x,x,param(1),0, param(2),function_flag,Alpha_Factor), X_init(1,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1);
                [Optimal_AB(2, 1:2), Optimal_AB(2, 3)] = fmincon (@(x) HPZ_Utility_Helper(prob_x,x,param(1),0, param(2),function_flag,Alpha_Factor), X_init(2,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1);

            end
        end
        
        [~,index_tt] = min(Optimal_AB(:,3));
        choice_matrix(i,:)=Optimal_AB(index_tt,1:2);
        
    end
 
end

elseif pref == 2

[observations,~]=size(prices);

choice_matrix=zeros(observations,2);

if function_flag==1

	parfor i=1:observations  
        % Threshold initialization
        % Thresholds for acceptable difference between optimal solutions
        ratio_diff = 0.05;
        Diff_threshod = 10^(-2);
        %% Choose two points A and B near the boundaries from the
        %% budget line:
        % compute the slope of the budget line
        x_1_init = endowments(i)/prices(i,1);
        % The (2x2) matrix of two initial points: X_init
        X_init = zeros(2,2);
        % Point A - The 1st row of the X_init matrix:
        % Divide the budget line to 1001 pieces and take the 102nd point:       
        X_init(1,1) = (102/(1000+1)) * x_1_init; % (x1,_)
        X_init(1,2) = (endowments(i)-(X_init(1,1)*prices(i,1)))/prices(i,2); % (_,x2)
        % Point B - The 2nd row of the X_init matrix:
        % Divide the budget line to 1001 pieces and take the 898th point:
        X_init(2,1) = (898/(1000+1)) * x_1_init;  % (x1,_)
        X_init(2,2) = (endowments(i)-(X_init(2,1)*prices(i,1)))/prices(i,2); % (_,x2)
        opts_phase1 = optimset('Algorithm','sqp','Display','off');        
        % Optimal solution matrix (2x3) for point A and B: [x1,x2,fval]
        Optimal_AB = zeros(2,3);
        % Run fmincon using:
        % 1. Default HPZ_Utility_Helper function as objective function
        % 2. Subject to EQUALITY constraint: Ax = b
        % 3. lower bound set to : [0 0]
        % 4. TolX = 0
        % 5. Iinitial points set to A=[X_init(1,:)] AND B=[X_init(2,:)]
        [Optimal_AB(1, 1:2), Optimal_AB(1, 3)] = fmincon (@(x) HPZ_OR_Utility_Helper(x,param(1),param(2),function_flag), X_init(1,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1);
        [Optimal_AB(2, 1:2), Optimal_AB(2, 3)] = fmincon (@(x) HPZ_OR_Utility_Helper(x,param(1),param(2),function_flag), X_init(2,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1);
        Diff_Xopts_AB = abs(Optimal_AB(1,1:2) - Optimal_AB(2,1:2));
        ratio_Xopts_AB = abs((Optimal_AB(1,1:2) ./ Optimal_AB(2,1:2)) - 1);
        % if the distance between two optimal solutions is less than the
        % threshold:
        if (((Diff_Xopts_AB(1) <= Diff_threshod) && (Diff_Xopts_AB(2) <= Diff_threshod)) || ...
            (ratio_Xopts_AB(1) <= ratio_diff)    && (ratio_Xopts_AB(1) <= ratio_diff))
        else
            % compute the minimum between these two optimal values
            Fval_Diff_phase1 = abs(Optimal_AB(1,3) - Optimal_AB(2,3));
            if (Fval_Diff_phase1 > 0)
                % Alpha_Factor:
                % Fval Multiplier to remove the effect of penalizer factor
                % inside the score function:
                % utility multiplier (used inside GlobalSearch objective
                % function)
                if (Fval_Diff_phase1 >= 1)
                    Alpha_Factor = 10^(6);
                else
                    % compute the number of number of digits after the "0."
                    digit_num = ceil(abs(log10(Fval_Diff_phase1)));
                    % Fval Multiplier to remove the effect of penalizer factor inside the score
                    % function:
                    Alpha_Factor = 10^(digit_num + 5);
                end
                [Optimal_AB(1, 1:2), Optimal_AB(1, 3)] = fmincon (@(x) HPZ_OR_Utility_Helper(x,param(1),param(2),function_flag,Alpha_Factor), X_init(1,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1);
                [Optimal_AB(2, 1:2), Optimal_AB(2, 3)] = fmincon (@(x) HPZ_OR_Utility_Helper(x,param(1),param(2),function_flag,Alpha_Factor), X_init(2,:), [], [], prices(i,:), endowments(i), [0 0], [], [], opts_phase1); %#ok<*PFBNS>

            end
        end
        
        [~,index_tt] = min(Optimal_AB(:,3));
        choice_matrix(i,:)=Optimal_AB(index_tt,1:2);
        
    end
   
end

end