Get_Lambda <-
function(K){

k = min(200, nrow(K))
lambda1 = eigs_sym(K, k = k, opts = list(retvec = FALSE))$values
IDX1<-which(lambda1 >= 0)

lambda0 = rep(0, length(lambda1))
IDX3 = which(lambda1 < 10^(-20)) ## very very small eigenvalues, set those to zero

lambda1[IDX3] = lambda0[IDX3] ## set those small eigenvalues to zero

return(lambda1)

}
