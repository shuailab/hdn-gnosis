diagonosis <- function(X, fit) {
  num_node <- ncol(X)  
  omega <- matrix(data = 0, nrow = num_node, ncol = num_node)
  rc_b <- rep(0, num_node)
  for (i in 1:num_node) {
    coef <- coef(bn_fit)[[i]]
    rc_b[i] <- coef[1]
    coef <- coef[-1]
    pos <- which(names(X) %in% names(coef))
    omega[i, pos] = coef
  }
  W <- solve(diag(num_node) - omega) # causal effect matrix
  
  rc <- t(W %*% t(X)) # likelihood of root cause
  rc <- rc - rc_b
  names(rc) <- names(X)
  barplot(rc,horiz = FALSE,names.arg = names(X),cex.names=?)  
  return(colMeans(rc))
}
