mtrx.power <-
function(x, n){
eig = eigen(x)
values = Get_Lambda(x)
mtrx.power = eig$vectors %*% (c(values[which(values>0)]^n, rep(0, sum(values < 10^(-20))) ) * t(eig$vectors))
return(mtrx.power)
}
