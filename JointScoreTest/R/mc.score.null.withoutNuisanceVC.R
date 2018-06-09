mc.score.null.withoutNuisanceVC <-
function(a,  rho, E.rho, v.rho, X2.tld1, lambda12, X2.tld3, sigma2.h  ){

Q1.h1 = t(a)%*%X2.tld1%*%a
Q12.tld.h1 = sum(rchisq(n=length(lambda12),df=1)*lambda12)/(2*sigma2.h)
Q3.tld.h1 = t(a)%*%X2.tld3%*%a/(2*sigma2.h)

t.f = (E.rho + (1-rho)*Q12.tld.h1 + rho*Q1.h1 + (1-rho)*Q3.tld.h1)/v.rho

return(t.f)
}
