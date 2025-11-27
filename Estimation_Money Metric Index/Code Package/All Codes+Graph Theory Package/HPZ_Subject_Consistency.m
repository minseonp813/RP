function [expenditure,DRP,SDRP,RP,SRP,WARP,GARP,SARP,FLAGS,VIO_PAIRS,VIOLATIONS,AFRIAT,VARIAN,Var_exact,HM,HM_exact] = HPZ_Subject_Consistency (Choices)

%Subject_Consistency

%Variables:
%ID - the subjects ID.
%Choices - a mX4 matrix where the rows are observations and the columns
%are X Y PX PY.

% Let A be an amount of tokens. Let B also be an amount of tokens. Then,
% A>B if and only if A-B>Threshold.

global Threshold

global identical_choice

Threshold = eps;

%Choices

%The matrix "expenditure" has at the cell in the i'th row and the j'th
%column, the value of the bundle that was chosen in observation j given the
%prices of observation i

expenditure = (Choices(:,1)*Choices(:,3)' + Choices(:,2)*Choices(:,4)')';

%record the dimensions of expenditure

[rows,cols] = size(expenditure);

identical_choice=zeros(rows);

for j=1:rows
    for k=1:rows
        if Choices(j,1)==Choices(k,1) && Choices(j,2)==Choices(k,2)
            identical_choice(j,k)=1;
        end
    end
end

%The matrix REF has at the cell in the i'th row and the j'th
%column, the difference between the value of the bundle that was chosen in 
%observation i and the bundle that was chosen in observation j given the 
%prices of observation i

REF = diag(expenditure)*ones(rows,1)' - expenditure;

%The matrix DRP has at the cell in the i'th row and the j'th
%column, 1 if and only if the bundle that was chosen in 
%observation i is directly revealed preferred to the bundle that was chosen 
%in observation j.

DRP = ceil((REF+Threshold)/(max(max(abs(REF+Threshold)))+1));

%The matrix SDRP has at the cell in the i'th row and the j'th
%column, 1 if and only if the bundle that was chosen in 
%observation i is strictly directly revealed preferred to the bundle that was chosen 
%in observation j.

SDRP = ceil((REF-Threshold)/(max(max(abs(REF-Threshold)))+1));

% statement needed for the graph theory external package to work
% efficiently

set_matlab_bgl_default(struct('full2sparse',1));

%The matrix NS_RP has at the cell in the i'th row and the j'th
%column, Inf if and only if the bundle that was chosen in 
%observation i is not revealed preferred to the bundle that was chosen 
%in observation j. Otherwise it includes a positive integer.

NS_RP = all_shortest_paths(DRP);

%The matrix RP has at the cell in the i'th row and the j'th
%column, 1 if and only if the bundle that was chosen in 
%observation i is revealed preferred to the bundle that was chosen 
%in observation j. 

RP = zeros(rows);

for j=1:rows
    for k=1:cols
        if ~isinf(NS_RP(j,k))
            RP(j,k)=1;
        end
    end
end

%The matrix NS_SRP has at the cell in the i'th row and the j'th
%column, zero if and only if the bundle that was chosen in 
%observation i is not strictly reveal prefered to the bundle that was chosen 
%in observation j. Otherwise it includes a positive integer.

NS_SRP = (RP*SDRP)*RP;

%Create SRP

SRP = zeros(rows);

%The matrix SRP has at the cell in the i'th row and the j'th
%column, 1 if and only if the bundle that was chosen in 
%observation i is strictly revealed preferred to the bundle that was chosen 
%in observation j. 

for j=1:rows
    for k=1:cols
        if ~(NS_SRP(j,k)==0) 
            SRP(j,k)=1;
        end
    end
end

%To test for SARP we will use the definition of SARP1. For every pair of
%choices x and y if xRy and yRx it must be that x=y. We will take RP and
%its transpose and multiply element by element. Every 1 correspondes to 
%xRy and yRx. Then we take off the identity matrix (all the pairs x=y). 
%The final matrix is the zero matrix if and only if SARP is satisfied. 



% The following conforms with Varian (1982) definition of SARP1 and SARP2
% SARP3 would be SARP = RP.*(DRP') - identical_choice;
% As a binary test these definitions are equivalent. However, SARP1 and
% SARP2 may report more violations.
SARP = RP.*(RP') - identical_choice;

%A flag that keeps the SARP result. It is 0 if and only if the data
%satisfies SARP.
SARP_FLAG = 1;

SARP_ERRORS = sum(sum(SARP));

SARP_VIO_PAIRS = sum(sum(triu(SARP|(SARP'))));

%If the SARP matrix is only zeros then the data satisfies SARP
if SARP_ERRORS==0
  SARP_FLAG = 0;
end 

%To test for GARP we will do the following: for every pair of
%choices x and y if xRy then not yP0x. We will take RP and the transpose of
%SDRP and multiply element by element. Every 1 correspondes to 
%xRy and yP0x. The final matrix is the zero matrix if and only if 
%GARP is satisfied. 

GARP = RP.*(SDRP');

%A flag that keeps the GARP result. It is 0 if and only if the data
%satisfies GARP.
GARP_FLAG = 1;

GARP_ERRORS = sum(sum(GARP))

GARP_VIO_PAIRS = sum(sum(triu(GARP|(GARP'))));

%If the GARP matrix is only zeros then the data satisfies GARP
if GARP_ERRORS==0
  GARP_FLAG = 0;
end 

%To test for WARP we will do the following: for every pair of
%choices x and y if xR0y then not yP0x. We will take DRP and the transpose of
%SDRP and multiply element by element. Every 1 correspondes to 
%xR0y and yP0x. The final matrix is the zero matrix if and only if 
%WARP is satisfied. 

WARP = DRP.*(DRP') - identical_choice;

%A flag that keeps the WARP result. It is 0 if and only if the data
%satisfies WARP.
WARP_FLAG = 1;

WARP_ERRORS = sum(sum(WARP));

WARP_VIO_PAIRS = sum(sum(triu(WARP|(WARP'))));

%If the WARP matrix is only zeros then the data satisfies WARP
if WARP_ERRORS==0
  WARP_FLAG = 0;
end 

FLAGS(1)=WARP_FLAG;
FLAGS(2)=GARP_FLAG;
FLAGS(3)=SARP_FLAG;

VIO_PAIRS(1)=WARP_VIO_PAIRS;
VIO_PAIRS(2)=GARP_VIO_PAIRS;
VIO_PAIRS(3)=SARP_VIO_PAIRS;

VIOLATIONS(1)=WARP_ERRORS;
VIOLATIONS(2)=GARP_ERRORS;
VIOLATIONS(3)=SARP_ERRORS;


AFRIAT = 0;

VARIAN = [0,0,0];

HM = 0;

HM_exact = 1;

Var_exact = 1;

if GARP_FLAG==1 
    AFRIAT = HPZ_Afriat_efficiency_index (expenditure)
    [HM,HM_exact] = HPZ_Houtman_Maks_efficiency_index (Choices)
    [VARIAN,Var_exact] = HPZ_Varian_efficiency_index (expenditure)
end

end

