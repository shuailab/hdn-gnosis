trend <- function(X, nseq = ?) {
  res <- list()    
  X_diff <- diff(as.matrix(X))
  for (i in 1:(nrow(X)-nseq)) {
    beg <- i
    end <- i+nseq-1
    var_pos <- data.frame(no=0, slope=0)
    var_neg <- data.frame(no=0, slope=0)
    for (j in 1:ncol(X)) {
      dir <- sum(sign(X_diff[beg:end, j]))
      if (dir == nseq) {
        var_pos <- rbind(var_pos, c(j, (X[end,j]-X[beg,j])/nseq))
      } else if (dir == -nseq) {
        var_neg <- rbind(var_neg, c(j, (X[end,j]-X[beg,j])/nseq))
      }
    }
    var_pos <- var_pos[-1,]; var_neg <- var_neg[-1,];   
    if (nrow(var_pos) != 0) res <- 
      c(res, list(list(start = beg, end = end, dir = "+", var = var_pos)))
    if (nrow(var_neg) != 0) res <- 
      c(res, list(list(start = beg, end = end, dir = "-", var = var_neg)))
  }
  # each trend contains (start_time, end_time, is_uptrend, c(variables in this trend))
  return(res)
}
