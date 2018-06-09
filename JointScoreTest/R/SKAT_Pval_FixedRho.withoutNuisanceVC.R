SKAT_Pval_FixedRho.withoutNuisanceVC <-
function(rho, T.f.h, P2, PZP1, sigma2.h, E.rho, v.rho, P.X2, PZ, D){ 
n.r<-length(rho)
pval.each =  rep(0, n.r)
for(i in 1:n.r){
r.corr<-rho[i]
Q = v.rho[i]*T.f.h - E.rho[i]
if(r.corr == 0){ 
Phi = PZP1*0.5/sigma2.h
}else if (r.corr == 1){
Phi = P2
}else{ 
Phi = r.corr * P2 + (1-r.corr)*PZP1*0.5/sigma2.h
}
qr1<-qr(cbind(P.X2, PZ))
Q11<-qr.Q(qr1)
Phi1<-t(Q11) %*% Phi %*% Q11

pval.fixedrho = SKAT:::Get_Davies_PVal(Q/2, Phi1)$p.value
 
}

return(pval.fixedrho)


}
