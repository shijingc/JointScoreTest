Get_Lambda12 <-
function(K){
k = min(50, nrow(K))
lambda1 = eigs_sym(K, k = k, opts = list(retvec = FALSE))$values
IDX2 = which(abs(lambda1) > max(abs(lambda1))/100000)
lambda = lambda1[IDX2]

return(lambda)
}
