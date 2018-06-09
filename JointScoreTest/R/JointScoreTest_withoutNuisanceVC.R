JointScoreTest_withoutNuisanceVC <-
function(y, X, S, U, SIGMA, rho = c(0.00, 0.25, 0.50, 0.75, 1.00), out_type = "C", binom_size = 1, B0 = 500){

set.seed(12345)

X1 = S; X2 = X; Z = U; D = SIGMA  ## needs to include a column of value 1 in X1 if wants an intercept
rho2=rho^2;  n = dim(X2)[1];  p2  = dim(X2)[2]

if(out_type == "C"){  ## coutinuous response

X1.proj.partial = X1%*%solve(t(X1)%*%X1)
P = diag(1,n) - X1.proj.partial %*%t(X1)   # P = diag(1,n) - X1%*%solve(t(X1)%*%X1)%*%t(X1)
X1.X2 = t(X1)%*%X2
P.X2 = X2 - X1.proj.partial %*% X1.X2
P2 = P.X2 %*% solve( t(P.X2)%*% P.X2 ) %*% t(P.X2)   # P2 = P%*%X2%*%solve(t(X2)%*%P%*%X2)%*%t(X2)%*%P
X2.X2 = solve(t(X2)%*%X2)
X2.tld = X2 %*% X2.X2 %*% t(X2)  # X2.tld = X2%*%solve(t(X2)%*%X2)%*%t(X2)
ZD = Z %*% D
Z.tld =  ZD %*% t(Z) 
Delta1 = P.X2  %*% X2.X2 %*% t(P.X2)    # Delta1 = P%*%X2.tld%*%P
PZ = P %*% Z
PZP = ZD %*% t(PZ)
PZP1 = PZ %*% D %*% t(PZ)
X2.tld2 = t(X2) %*% P.X2  # X2.tld2 =t(X2)%*%P%*%X2
X2.tld1 = solve(X2.tld2)
X2.tld3 = X2.tld1%*%t(P.X2)%*%Z.tld%*%P.X2%*%X2.tld1
Z1 = PZ;  Z2 = P2 %*% Z

Q12.tld.K = Z1 %*% D %*% t(Z1) - Z2 %*% D %*% t(Z2)
lambda12 = Get_Lambda12(Q12.tld.K)

E.num = -sum(diag(PZP))            
var.term1.num = sum(sapply(1:n, function(x) sum(PZP[,x]*PZP[x,])))  # var.term1.num = sum(diag(PZP%*%PZP))

cm = chol(X2.tld2)
tr.ZX2 = sum(sapply(1:n, function(x) sum(Z.tld[x,]*P2[,x]))) # tr.ZX2 = sum(diag(Z.tld%*%P2))

obs.den1 =(1-rho)^2*var.term1.num
obs.den2 =2*rho^2*p2
obs.den3 =2*rho*(1-rho)*tr.ZX2

### Score Test ###

H0 = lm(y ~ X1)
sigma.h = summary(H0)$sigma; sigma2.h = sigma.h^2

E.h = E.num/(2*sigma2.h)
var.term1.h = var.term1.num/(2*sigma2.h^2)

obs.den1.h = obs.den1/(2*sigma2.h^2)
obs.den3.h = obs.den3/sigma2.h

Q1.h = t(y)%*%P2%*%y/sigma2.h;     Q2.h = t(y)%*%PZP1%*%y/(2*sigma2.h^2)
E.rho = -rho*p2 + (1-rho)*E.h
obs.num.h = E.rho + (1-rho)*Q2.h[1,1] + rho*Q1.h[1,1]

v.rho = sqrt(obs.den1.h + obs.den2+ obs.den3.h)
T.f.h = obs.num.h/v.rho
obs.T.f =  max(T.f.h)
rho.T.f = rho[which.max(T.f.h)]

a0.res = matrix(rnorm(B0*p2, mean = 0, sd = 1), ncol = B0)
a.res = t(cm)%*%a0.res
mc.samples.nullT = apply(a.res, 2, 'mc.score.null.withoutNuisanceVC',  rho=rho, E.rho=E.rho, v.rho = v.rho, X2.tld1 = X2.tld1, lambda12 = lambda12, X2.tld3 = X2.tld3, sigma2.h = sigma2.h )
copula.Sig =  cor( t(mc.samples.nullT) ,method = "spearman")

pCopula.rho = SKAT_Each_Pval.withoutNuisanceVC(rho, T.f.h, P2, PZP1, sigma2.h, E.rho, v.rho, copula.Sig, P.X2=P.X2, PZ=PZ, D=D)

}else{  ## binary or binomial response

ZD = Z %*% D
Z.tld =  ZD %*% t(Z) 

## needs to include a column of value 1 in X1 if wants an intercept
if( dim(X1)[2] >1 ){ ## X1 contains more than one covariate
H0 = glm(cbind(y,binom_size-y) ~ X1-1, family=binomial)
}else{  ## X1 contains only one covariate, this covariate can be an intercept only
H0 = glm(cbind(y,binom_size-y) ~ X1-1, family=binomial)
}

#beta1.h = H0$coefficients
#Xbeta1.h = X1 %*% beta1.h 
#mu.h = exp(Xbeta1.h)/(1+exp(Xbeta1.h))
mu.h = predict(H0, type='response')
pp = as.vector(binom_size*mu.h*(1-mu.h))
#W = diag(pp)
#W.inv = diag(1/pp)
#W.hf = diag(sqrt(pp))

P.hat = diag(1,n) - (sqrt(pp)*X1)%*%solve(t(X1)%*%(pp*X1))%*%(t(X1)*sqrt(pp))  ## P.hat = diag(1,n) - W.hf%*%X1%*%solve(t(X1)%*%W%*%X1)%*%t(X1)%*%W.hf
P.tlda =  sqrt(pp) * t(sqrt(pp)*P.hat)     ## P.tlda =  W.hf%*%P.hat%*%W.hf
PX2 = P.tlda %*% X2;  PZ = P.tlda %*% Z
X2.tld1 = t(PX2) %*% X2
X2.tld = X2 %*% ( solve(X2.tld1)%*%t(X2) )
P2 = P.hat %*% (sqrt(pp)*X2.tld) %*% (sqrt(pp)*P.hat)   ## P2 = P.hat %*%W.hf %*%X2.tld%*%W.hf%*%P.hat
WZW = sqrt(pp)* t(sqrt(pp)* Z.tld )   ## WZW = W.hf %*%Z.tld %*% W.hf
ZP = Z.tld%*%P.tlda
PX2P = P.tlda%*%X2.tld%*%P.tlda
PZP = P.tlda%*%ZP
WPX2PW = (1/pp) * t( (1/pp) * PX2P )   ## WPX2PW = W.inv%*%PX2P%*%W.inv
WPZPW = (1/pp) * t( (1/pp) * PZP )    ## WPZPW = W.inv%*%PZP%*%W.inv

X2.tld1.inv = solve(X2.tld1)
X2.X2.tld1 = X2 %*% X2.tld1.inv
X2.tld3 = t(X2.X2.tld1) %*% PZP %*% X2.X2.tld1  ## X2.tld3 = solve(X2.tld1)%*%t(X2)%*%PZP%*%X2%*%solve(X2.tld1)

Q12.tld.K = (P.hat-P2)%*%WZW%*%(P.hat+P2)

E.h = -(rho*p2 + 0.5*(1-rho)*sum(diag(ZP)))
var.h1 = 0.5*(1-rho)^2*sum(sapply(1:n, function(x) sum(ZP[x,]*ZP[,x])))   # var.h1 = 0.5*(1-rho)^2*sum(diag(ZP%*%ZP))
var.h2 = 2*rho*(1-rho)*sum(sapply(1:n, function(x) sum(X2.tld[x,]*PZP[,x])))   ## var.h2 = 2*rho*(1-rho)*sum(diag(X2.tld%*%PZP))
var.h = 2*rho2*p2 + var.h1 + var.h2
sd.h = sqrt(var.h)

lambda12 = Get_Lambda12(0.5*(Q12.tld.K+t(Q12.tld.K)))
cm = chol(X2.tld1)

Q1.h = t(y-mu.h)%*%WPX2PW%*%(y-mu.h);     Q2.h = 0.5*t(y-mu.h)%*%WPZPW%*%(y-mu.h)

T.f.h = (rho*Q1.h + (1-rho)*Q2.h + E.h)/sd.h
obs.T = max(T.f.h)
rho.T.f = rho[which.max(T.f.h)]

a0.res = matrix(rnorm(B0*p2, mean = 0, sd = 1), ncol = B0)
a.res = t(cm)%*%a0.res
mc.samples.nullT = apply(a.res, 2, 'mc.score.null.withoutNuisanceVC.binary',  X2.tld1.inv=X2.tld1.inv, lambda12=lambda12, X2.tld3 = X2.tld3, rho = rho,  E.h = E.h, sd.h = sd.h )
copula.Sig =  cor( t(mc.samples.nullT) , method = "spearman")

pCopula.rho = SKAT_Each_Pval.withoutNuisanceVC.binary(rho, T.f.h, E.h, sd.h,  pp,  PX2P,  PZP, PX2, PZ, copula.Sig)

}

rs = list()
rs$pvalue = pCopula.rho$pvalue.normCopula; rs$opt.rho = pCopula.rho$opt.rho ;  rs$score.each = T.f.h
return(rs)
}
