JointScoreTest_withNuisanceVC <-
function(y, X, S, U, Z, group, SIGMA, rho = c(0.00, 0.25, 0.50, 0.75, 1.00), out_type = "C", binom_size = 1, B0 = 500){

set.seed(12345)

X1 = S; X2 = X; B = U; SIG = SIGMA    ## needs to include a column of value 1 in X1 if wants an intercept
rho2=rho^2;   p2 = dim(X2)[2];   N = length(y)


if(out_type == "C"){  ## coutinuous response

H0 = lmer(y ~ X1 + (1|group))
theta.h = as.data.frame(VarCorr(H0))$sdcor[1]; 
phi.h = as.data.frame(VarCorr(H0))$sdcor[2]

BSB = B%*%SIG%*%t(B)
V = theta.h^2 * Z %*% t(Z) + diag(phi.h^2, N)
V.inv = solve(V)
V.X1 = t(X1)%*%V.inv
P.tld = V.inv - t(V.X1) %*% solve(V.X1%*%X1) %*% V.X1    ##P.tld = V.inv - V.inv %*% X1 %*% solve(t(X1)%*%V.inv%*%X1) %*% t(X1) %*% V.inv
V.hf = V%^% (0.5)
V.inv.hf = V%^% (-0.5)
P.hat = V.hf %*% P.tld %*% V.hf

P.X2 = P.tld%*%X2
X2.tld2 =t(X2)%*%P.X2
X2P = X2%*%solve(X2.tld2)%*%t(P.X2)   # X2P = X2%*%solve(t(X2)%*%P.tld%*%X2)%*%t(X2)%*%P.tld
BSBP = BSB%*%P.tld
E.h = -sum(diag(BSBP))/2
P.X2.P = P.tld %*% X2P
P2 = V.hf %*% P.X2.P %*% V.hf  

Vhf.P.X2 = V.hf %*% P.X2
Vhf.P.B = V.hf %*% P.tld%*%B

Q1.h = t(y)%*%P.X2.P%*%y;     
Q2.h = t(y)%*%P.tld%*%BSBP%*%y/2
E.rho = -rho*p2 + (1-rho)*E.h
obs.num.h = E.rho + (1-rho)*Q2.h + rho*Q1.h

obs.den2 = 2*rho^2*p2
obs.den1.h = (1-rho)^2*sum(sapply(1:N, function(x) sum(BSBP[,x]*BSBP[x,])))/2  ## obs.den1.h = (1-rho)^2*sum(diag(BSBP%*%BSBP))/2
obs.den3.h = 2*rho*(1-rho)*sum(sapply(1:N, function(x) sum(X2P[,x]*BSBP[x,])))   ## obs.den3.h = 2*rho*(1-rho)*sum(diag(X2P%*%BSBP))

v.rho = sqrt(obs.den1.h + obs.den2+ obs.den3.h)
T.f.h = obs.num.h/v.rho
obs.T.f =  max(T.f.h)
rho.T.f = rho[which.max(T.f.h)]

cm = chol(X2.tld2)
X2.tld1 = solve(X2.tld2)
Q12.tld.K = (P.hat-P2)%*%V.inv.hf%*%BSB%*%V.inv.hf%*%(P.hat+P2)
lambda12 = Get_Lambda12(0.5*(Q12.tld.K+t(Q12.tld.K)))
X2.tld3 = X2.tld1 %*% t(P.X2) %*% BSBP %*% X2 %*% X2.tld1

a0.res = matrix(rnorm(B0*p2, mean = 0, sd = 1), ncol = B0)
a.res = t(cm)%*%a0.res
mc.samples.nullT = apply(a.res, 2, 'mc.score.null.withNuisanceVC', rho=rho,  E.rho=E.rho, v.rho = v.rho, X2.tld1 = X2.tld1, lambda12 = lambda12, X2.tld3 = X2.tld3 )
copula.Sig =  cor( t(mc.samples.nullT) ,method = "spearman")

pCopula.rho = SKAT_Each_Pval.withNuisanceVC(rho, T.f.h, P.tld, X2P, BSBP, V.hf, E.rho, v.rho, copula.Sig, Vhf.P.X2, Vhf.P.B, SIG)

}else{

H0 = glmer(cbind(y,binom_size-y) ~ X1 + (1|group), family = binomial)
mu.h = predict(H0, type = "response")
pp = as.vector(binom_size*mu.h*(1-mu.h))
#W = diag(pp)
#W.inv = diag(1/pp)
#W.hf = diag(sqrt(pp))

theta.h = as.data.frame(VarCorr(H0))$sdcor[1]; 

V = Z %*% diag(theta.h^2, dim(Z)[2]) %*% t(Z) + diag(1/pp)   ## W.inv
V.inv = solve(V)
V.X1 = t(X1)%*%V.inv
P.tld = V.inv - t(V.X1) %*% solve(V.X1%*%X1) %*% V.X1    ##P.tld = V.inv - V.inv %*% X1 %*% solve(t(X1)%*%V.inv%*%X1) %*% t(X1) %*% V.inv
V.hf = V%^% (0.5)
V.inv.hf = V%^% (-0.5)

P.hat = V.hf %*% P.tld %*% V.hf

BSB = B%*% ( SIG%*%t(B) )
X2.Ptld = t(X2)%*%P.tld
X2.tld2 = X2.Ptld %*% X2    # X2.tld2 = t(X2)%*%P.tld%*%X2
X2.tld1 = solve(X2.tld2)
X2P = X2 %*% X2.tld1 %*% X2.Ptld  ## X2P = X2%*%solve(t(X2)%*%P.tld%*%X2)%*%t(X2)%*%P.tld
BSBP = BSB%*%P.tld
E.h = -sum(diag(BSBP))/2
PX2P = P.tld %*% X2P;  P2 = V.hf %*% PX2P %*% V.hf
PBSBP = P.tld %*% BSBP  ;   PB2 = V.hf %*% PBSBP %*% V.hf

Vhf.P.X2 = V.hf %*% t(X2.Ptld)
Vhf.P.B = V.hf %*% P.tld%*%B

Y = predict(H0) + (1/pp)* (y-mu.h)   ### Y = predict(H0) + W.inv %*% (y-mu.h)   
Q1.h = t(Y) %*% PX2P %*% Y;     
Q2.h = t(Y) %*% PBSBP %*% Y/2
E.rho = -rho*p2 + (1-rho)*E.h
obs.num.h = E.rho + (1-rho)*Q2.h + rho*Q1.h

obs.den1.h =  0.5*(1-rho)^2*sum(sapply(1:N, function(x) sum(BSBP[x,]*BSBP[,x])))   # obs.den1.h = 0.5*(1-rho)^2*sum(diag(BSBP%*%BSBP))
obs.den2 = 2*rho^2*p2
obs.den3.h = 2*rho*(1-rho)*sum(sapply(1:N, function(x) sum(X2P[x,]*BSBP[,x])))   ## obs.den3.h = 2*rho*(1-rho)*sum(diag(X2P%*%BSBP))
v.rho = sqrt(obs.den1.h + obs.den2+ obs.den3.h)

T.f.h = obs.num.h/v.rho
obs.T.f =  max(T.f.h)
rho.T.f = rho[which.max(T.f.h)]

cm = chol(X2.tld2)
a0 = matrix(rnorm(B0*p2, mean = 0, sd = 1), ncol = B0)
a = t(cm)%*%a0
Q12.tld.K = (P.hat-P2)%*%V.inv.hf%*%BSB%*%V.inv.hf%*%(P.hat+P2)
lambda12 = Get_Lambda12(0.5*(Q12.tld.K+t(Q12.tld.K)))
X2.tld3 = X2.tld1 %*% X2.Ptld %*% BSBP %*% X2 %*% X2.tld1  ## X2.tld3 = X2.tld1 %*% t(X2)%*%P.tld %*% BSBP %*% X2 %*% X2.tld1
mc.samples.nullT = apply(a, 2, 'mc.score.null.withNuisanceVC.binary', rho = rho, E.rho = E.rho, v.rho = v.rho, X2.tld1 = X2.tld1, X2.tld3 = X2.tld3, lambda12 = lambda12  )
copula.Sig =  cor( t(mc.samples.nullT) ,method = "spearman")

pCopula.rho = SKAT_Each_Pval.withNuisanceVC.binary(rho, T.f.h, P2, PB2, E.rho, v.rho, copula.Sig, Vhf.P.X2, Vhf.P.B, SIG)


}


rs = list()
rs$pvalue = pCopula.rho$pvalue.normCopula; rs$opt.rho = pCopula.rho$opt.rho ;  rs$score.each = T.f.h

return(rs)



}
