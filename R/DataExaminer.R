DataExaminer <- function(X) {
  res_name <- names(X)
  res_pvalue <- c()
  res_missing <- c()
  for (i in 1:ncol(dat_KPI)) {
    res_pvalue <- c(res_pvalue, shapiro.test(X[,i])$p.value)
    res_missing <- c(res_missing, sum(is.na(X[,i])))
  }
  return(data.frame(cbind(name = res_name, 
                          p.value = round(res_pvalue, ?), 
                          missing = res_missing)))
}
