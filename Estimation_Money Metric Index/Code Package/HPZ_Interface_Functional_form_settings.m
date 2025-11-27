function [function_flag, beta_flag, zeros_flag] = HPZ_Interface_Functional_form_settings()
%% the following is specific information needed for the estimation
%% of Choi et al. (2007) data set.
% function_flag = 1 ('CRRA')
% function_flag = 2 ('CARA')
% Would you like to allow the Disappoint Aversion parameter to be negative (bounded by -1)?
% beta_flag = 1 (case 'Yes')
% beta_flag = 2 (case 'No')
% beta_zero_flag ("True" value of this boolean flag restricts beta to be
% equal to zero)
% rho_zero_flag ("True" value of this boolean flag restricts rho to
% be equal to zero)
%% if function_flag==1 ('CRRA')
% Would you like to allow for zeros in the choices (else Choi et al. (2007)
% correction is applied)?)
% zeros_flag = 1 ('Yes')
% zeros_flag = 2 ('No')
%% the following is specific information needed for the estimation of Choi
%% et al. (2007) data set if Non-Linear Least Squares is used.
% if ((treatment==2)||(treatment==3))
% Would you like to use the correct Disappointment Aversion bias (else Choi
% et al. (2007) calculation is applied)
% asymmetric_flag = 1 ('Yes')
% asymmetric_flag = 2 ('No')

% "True" means that the estimation calculation is numeric and "False" means
% analytic estimation
global numeric_flag

global beta_zero_flag

%% initialization
function_flag = 0;
numeric_flag = false;
beta_flag = 1;
beta_zero_flag = false;
zeros_flag = 0;

% create the gui figure
sz = [450 450]; % figure size
screensize = get(0,'ScreenSize'); % screen size
xpos = ceil((screensize(3)-sz(2))/2); % center the figure on the
ypos = ceil((screensize(4)-sz(1))/2); % center the figure on the
S.fh = figure('units','pixels',...
    'position',[xpos, ypos, sz(2), sz(1)],...
    'menubar','none',...
    'name','Functional Form Settings',...
    'numbertitle','off',...
    'resize','off');

%% Disappointment Aversion
S.label_DA = uicontrol('style','text',...
    'units','pix',...
    'position',[120 410 200 30],...
    'backgroundc',get(S.fh,'color'),...
    'fontsize',12,'fontweight','bold',...
    'string','Disappointment Aversion');

%% Functional Forms
S.bg_ff = uibuttongroup('units','pix',...
    'title', 'Functional Forms', ...
    'pos',[45 350 360 60]);

S.functional_form_rd(1) = uicontrol(S.bg_ff,...
    'style','rad',...
    'unit','pix',...
    'position',[25 10 80 20],...
    'string',' CRRA');
% S.functional_form_rd(2) = uicontrol(S.bg_ff,...
%     'enable','off', ...
%     'style','rad',...
%     'unit','pix',...
%     'position',[275 5 80 30],...
%     'string',' Others');
S.functional_form_rd(2) = uicontrol(S.bg_ff,...
    'enable','on', ...
    'style','rad',...
    'unit','pix',...
    'position',[150 10 80 20],...
    'string',' CARA');
%% Numeric optimization or Analytical solution
S.bg_na = uibuttongroup('units','pix',...
    'title', 'Solution Options', ...
    'pos',[45 280 360 60]);

S.solution_option_rd(1) = uicontrol(S.bg_na,...
    'style','rad',...
    'unit','pix',...
    'position',[25 10 160 20],...
    'string',' Numerical Approach');

S.solution_option_rd(2) = uicontrol(S.bg_na,...
    'value', 1.0,...
    'style','rad',...
    'unit','pix',...
    'position',[190 5 160 30],...
    'string',' Analytical Approach');

%% Parameter Settings (Zeros, and negative parameter setting)
S.bg_ps = uibuttongroup('units','pix',...
    'title', 'Parameter Setting', ...
    'pos',[45 160 360 110]);

S.negative_beta_ch = uicontrol(S.bg_ps, ...
    'value', 1.0,...
    'style','check',...
    'unit','pix',...
    'position',[5 50 350 25],...
    'string',' Allow DA coefficient to be negative (bounded to -1)',...
    'fontsize',10);

S.param_zero_chk = uicontrol(S.bg_ps,...
    'style','check',...
    'unit','pix',...
    'position',[5 20 150 20],...
    'string',' DA Coefficient = 0',...
    'fontsize',10);

%% Corners
S.bg_cr = uibuttongroup('units','pix',...
    'title', 'Corners', ...
    'pos',[45 60 360 90]);
S.corners_rd(1) = uicontrol(S.bg_cr, ...
    'style','rad',...
    'unit','pix',...
    'position',[15 40 250 20],...
    'string',' Allow boundary choices',...
    'fontsize',10);
S.corners_rd(2) = uicontrol(S.bg_cr, ...
    'style','rad',...
    'unit','pix',...
    'position',[15 10 320 20],...
    'string',' Adjust boundary choices [Choi et al. (2007)]',...
    'fontsize',10);
%% end of settings

S.pb = uicontrol('style','push',...
    'unit','pix',...
    'position',[170 15 100 30],...
    'string','OK',...
    'callback',{@pb_call,S});

set(S.negative_beta_ch,'callback',{@ch_call_1,S})  % Set callback.
set(S.param_zero_chk,'callback',{@ch_call_2,S})  % Set callback.
set(S.functional_form_rd(:),'callback',{@rd_call,S})  % Set callback.

%% Negative Beta and Beta = 0
    function [] = ch_call_1(varargin)
        % Callback for pushbutton.
        S = varargin{3};  % Get the structure.
        
        if get(S.negative_beta_ch,'value') == 1.0
            set(S.param_zero_chk, 'enable', 'off');
            set(S.param_zero_chk, 'value', 0.0);              
        else
            set(S.param_zero_chk, 'enable', 'on');
        end
        
    end

    function [] = ch_call_2(varargin)
        % Callback for pushbutton.
        S = varargin{3};  % Get the structure.
        
        if get(S.param_zero_chk,'value') == 1.0
            set(S.negative_beta_ch, 'enable', 'off');
            set(S.negative_beta_ch, 'value', 0.0);            
        else
            set(S.negative_beta_ch, 'enable', 'on');
        end
        
    end

%% CRRA and Corners and parameter settings
    function [] = rd_call(varargin)
        % Callback for pushbutton.
        S = varargin{3};  % Get the structure.
        
        if (get(S.functional_form_rd(2),'value') == 1.0)
            % CARA 
            % Disable the corner correction
            set(S.corners_rd(:), 'enable', 'off');
            set(S.corners_rd(1), 'value', 0);
            set(S.negative_beta_ch, 'enable', 'on');
            set(S.param_zero_chk, 'enable', 'on');
            set(S.solution_option_rd(1), 'enable', 'on');
            set(S.solution_option_rd(1), 'value', 0);
            set(S.solution_option_rd(2), 'enable', 'on');
            set(S.solution_option_rd(2), 'value', 1);
            
        else % in case of CRRA
            set(S.corners_rd(:), 'enable', 'on');
            set(S.corners_rd(1), 'value', 1.0);
            set(S.negative_beta_ch, 'enable', 'on');
            set(S.param_zero_chk, 'enable', 'on');
            set(S.solution_option_rd(:), 'enable', 'on');
            set(S.solution_option_rd(2), 'value', 1);
        end
    end

uiwait(S.fh)  % Prevent all other processes from starting until closed.

%% OK button
    function [] = pb_call(varargin)
        % Callback for pushbutton.
        S = varargin{3};  % Get the structure.
        % Instead of switch, we could use num2str on:
        % find(get(S.bg,'selectedobject')==S.rd)      (or similar)
        % Note the use of findobj.  This is because of a BUG in MATLAB, whereby if
        % the user selects the same button twice, the selectedobject property will
        % not work correctly.
        
        %% Setting Functional Form
        switch findobj(get(S.bg_ff,'selectedobject'))
            case S.functional_form_rd(1)
                % CRRA
                function_flag = 1;
                
            case S.functional_form_rd(2)
                % CARA
                function_flag = 2;
                % Zeros_Flag
                zeros_flag = 1; % zeros are allowed
                
%             case S.functional_form_rd(2)
%                 % Others
%                 function_flag = 3;
%                 % Zeros_Flag
%                 zeros_flag = 1; % zeros are allowed
        end
        
        %% Solution Options
        if get(S.solution_option_rd(1), 'value') == 1.0
            numeric_flag = true;
        else
            numeric_flag = false;
        end
        
        %% Setting parameters for the seleceted functional form
        if get(S.negative_beta_ch, 'value') == 1.0
            % Beta Negative
            beta_flag = 1;
            beta_zero_flag = false;
            
        elseif get(S.negative_beta_ch, 'value') == 0.0 && get(S.param_zero_chk, 'value') == 1.0
            % Beta = 0
            beta_flag = 2;
            beta_zero_flag = true;
            
        elseif get(S.negative_beta_ch, 'value') == 0.0
            % Rho = 0
            beta_flag = 2;
            beta_zero_flag = false;
            
        end
        
        %% Corners
        if ~(get(S.functional_form_rd(2),'value') == 1.0)
            if get(S.corners_rd(1), 'value') == 1.0
                % Zeros are allowed
                zeros_flag = 1;
            else
                % Choi et al. (2007) for corner correction
                zeros_flag = 2;
            end
        end
        
        % close the window
        close(S.fh);
        
    end

end