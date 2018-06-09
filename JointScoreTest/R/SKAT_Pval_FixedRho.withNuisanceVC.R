SKAT_Pval_FixedRho.withNuisanceVC <-
function(rho, T.f.h, P.tld, X2P, BSBP, V.hf, E.rho, v.rho, Vhf.P.X2, Vhf.P.B, SIG){
n.r<-length(rho)
kernel1 = V.hf%*%P.tld%*%X2P%*%V.hf;  kernel2 = 0.5* V.hf%*%P.tld%*%BSBP%*%V.hf
for(i in 1:n.r){
r.corr<-rho
Q = v.rho*T.f.h - E.rho
if(r.corr == 0){ 
Phi = kernel2
}else if (r.corr == 1){
Phi = kernel1
}else{ 
Phi = r.corr * kernel1 + (1-r.corr)*kernel2
}

 qr1<-qr(cbind(Vhf.P.X2, Vhf.P.B))  
Q11<-qr.Q(qr1)
Phi1<-t(Q11) %*% Phi %*% Q11

pval.fixedrho  = SKAT:::Get_Davies_PVal(Q/2, Phi1)$p.value
 
}

return(pval.fixedrho)
}
