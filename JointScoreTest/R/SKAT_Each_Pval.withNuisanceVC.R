SKAT_Each_Pval.withNuisanceVC <-
function(rho, T.f.h, P.tld, X2P, BSBP, V.hf, E.rho, v.rho, copula.Sig, Vhf.P.X2, Vhf.P.B, SIG){
S.rho = max(T.f.h); 
n.r<-length(rho)
pval.each =  rep(0, n.r)
kernel1 = V.hf%*%P.tld%*%X2P%*%V.hf;  kernel2 = 0.5* V.hf%*%P.tld%*%BSBP%*%V.hf
for(i in 1:n.r){
r.corr<-rho[i]
Q = v.rho[i]*S.rho - E.rho[i]
if(r.corr == 0){ 
Phi = kernel2
}else if (r.corr == 1){
Phi = kernel1
}else{ 
Phi = r.corr * kernel1 + (1-r.corr)*kernel2
}

 qr1<-qr(cbind(Vhf.P.X2, Vhf.P.B))  ## kernel1 = Vhf.P.X2 %*% solve(t(Vhf.P.X2) %*% Vhf.P.X2) %*% t(Vhf.P.X2); 
## and kernel2 = 0.5*Vhf.P.B %*% SIG %*% t(Vhf.P.B)
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
