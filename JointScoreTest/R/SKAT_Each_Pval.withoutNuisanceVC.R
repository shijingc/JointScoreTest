SKAT_Each_Pval.withoutNuisanceVC <-
function(rho, T.f.h, P2, PZP1, sigma2.h, E.rho, v.rho, copula.Sig, P.X2, PZ, D){
S.rho = max(T.f.h); 
n.r<-length(rho)
pval.each =  rep(0, n.r)
for(i in 1:n.r){
r.corr<-rho[i]
Q = v.rho[i]*S.rho - E.rho[i]
if(r.corr == 0){ 
Phi = PZP1*0.5/sigma2.h
}else if (r.corr == 1){
Phi = P2
}else{ 
Phi = r.corr * P2 + (1-r.corr)*PZP1*0.5/sigma2.h
}
qr1<-qr(cbind(P.X2, PZ))  ## P2 = P.X2 %*% solve(t(P.X2) %*% P.X2) %*% t(P.X2); and PZP1 = PZ %*% D %*% t(PZ)
Q11<-qr.Q(qr1)
Phi1<-t(Q11) %*% Phi %*% Q11

pval.each[i] = SKAT:::Get_Davies_PVal(Q/2, Phi1)$p.value
 
}


obj.norm.Copula = normalCopula(copula.Sig[lower.tri(copula.Sig)],dim = nrow(copula.Sig),dispstr = "un")
  pvalue.normCopula <- 1-pCopula(1-pval.each, obj.norm.Copula) 
opt.rho = rho[which.max(T.f.h)]

res = list()
res$pvalue.normCopula = pvalue.normCopula;  res$opt.rho = opt.rho; res$pvalue.each = pval.each

return(res)
}
