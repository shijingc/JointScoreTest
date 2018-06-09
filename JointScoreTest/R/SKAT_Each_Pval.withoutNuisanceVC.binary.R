SKAT_Each_Pval.withoutNuisanceVC.binary <-
function(rho, T.f.h, E.h, sd.h,  pp,  PX2P,  PZP, PX2, PZ, copula.Sig){

S.rho = max(T.f.h); 
n.r<-length(rho)
pval.each =  rep(0, n.r)
kernel1 = ( 1/sqrt(pp) ) * t( ( 1/sqrt(pp) ) * PX2P )   ## kernel1 = V.hf%*%PX2P%*%V.hf, V.hf = W.inv.hf
  kernel2 = 0.5*( 1/sqrt(pp) ) * t( ( 1/sqrt(pp) ) * PZP )   ## kernel2 = V.hf%*%PZP%*%V.hf

for(i in 1:n.r){
r.corr<-rho[i]
Q = sd.h[i]*S.rho - E.h[i]
if(r.corr == 0){ 
Phi = kernel2 
}else if (r.corr == 1){
Phi = kernel1
}else{ 
Phi = r.corr * kernel1 + (1-r.corr) * kernel2
}

 Vhf.P.X2 = ( 1/sqrt(pp) ) * PX2  ## Vhf.P.X2 = V.hf %*% PX2, V.hf = W.inv.hf
Vhf.P.Z = ( 1/sqrt(pp) ) * PZ  ## Vhf.P.Z = V.hf %*% PZ,
qr1<-qr(cbind(Vhf.P.X2, Vhf.P.Z))  ## kernel1 = Vhf.P.X2 %*% solve(t(Vhf.P.X2) %*% Vhf.P.X2) %*% t(Vhf.P.X2); 
## and  Vhf.P.Z  =V.hf %*% PZ, kernel2 = Vhf.P.Z %*% D %*% t(Vhf.P.Z)
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
