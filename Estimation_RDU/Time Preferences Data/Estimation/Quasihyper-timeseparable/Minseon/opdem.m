function output = opdem(beta,delta,A1,A2,eMax,lMax,treatment)

dfactor = beta * delta * (treatment==1) +  delta * (treatment==2);
curv =  A1 * (treatment==1) + A2 * (treatment==2);

ep = 0.000001;

term = (curv>=ep).*(1./curv).*log(dfactor.*(lMax./eMax)) + (1/ep)*(curv<ep).*log(dfactor.*(lMax./eMax));

case1= repmat(((term >= -eMax) .* (term <= lMax)),[1 2]);
case2= repmat((term < -eMax),[1 2]);
case3= repmat((term > lMax),[1 2]);
output = case1.*[(1./(1+(lMax./eMax))).*(lMax - term), ((lMax./eMax)./(1+(lMax./eMax))).*(eMax + term)]+case2.*[eMax, zeros(size(eMax,1),1)]+case3.*[zeros(size(eMax,1),1),lMax];
    
end
