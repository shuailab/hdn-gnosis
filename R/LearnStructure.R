learnStructure <- function(X, layer) {
  # define layers, more layers can be defined
  X1 <- X[,layer == 1]
  X2 <- X[,layer == 2]
  names1 <- names(X1)
  names2 <- names(X2)
  # only connects from X1 to X2 are permitted.  
  # more constraints can be defined
  blacklist1 <- data.frame(from = as.vector(sapply(names1, function (x) rep(x,length(names1)))),
                           to = rep(names2))
  # learn structure
  # algorithms can be switched 
  bn_dag <- inter.iamb(cbind(X1, X2), blacklist = rbind(blacklist1, blacklist2, blacklist3),
               alpha = ?)
  # learn arc
  graph <- arc.strength(bn_dag, cbind(X1, X2))
  return(list(dag = bn_dag, graph = graph, algorithm = "inter.iamb", alpha = ?, validation = ?))
}
