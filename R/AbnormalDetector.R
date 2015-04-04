abnormal <- function(X, type = ?, reduce = ?) {
  if (reduce == TRUE) X <- predict(prcomp(X))[,?:?]
  qc <- switch(type,
               t = mult.chart(X, type = type, alpha = ?),
               mcusum2 = mult.chart(X, type = type, alpha = ?, h = ?))
  return(round(qc$t/qc$ucl, digits = ?))
}


