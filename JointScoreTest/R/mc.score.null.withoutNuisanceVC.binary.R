mc.score.null.withoutNuisanceVC.binary <-
function(a, X2.tld1.inv, lambda12, X2.tld3 , rho, E.h, sd.h ){

Q1.h1 = t(a)%*%X2.tld1.inv%*%a
Q12.tld.h1 = sum(rchisq(n=length(lambda12),df=1)*lambda12)/2
Q3.tld.h1 = t(a)%*%X2.tld3%*%a/2

###fixed var
t.f = (E.h + (1-rho)*Q12.tld.h1 + rho*Q1.h1 + (1-rho)*Q3.tld.h1)/sd.h


return(t.f)
}
