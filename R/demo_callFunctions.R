# network discovery and diagnosis
# '?' represents parameters of the model 

library(NetQCC)
library(MSQC)
library(bnlearn)

#load prepared data. KPI_list and Driver_list are customizable
load("./data/survey.rda")
dat_KPI <- survey[,KPI_list]
dat_DRIVER <- survey[,Driver_list]

DataExaminer(dat_KPI)
DataExaminer(dat_DRIVER)

score <- abnormal(dat_KPI, "mcusum2")
abnormal(dat_KPI, ?, reduce = ?)

res <- trend(dat_DRIVER, nseq = ?)
trend(score, ?)

fit <- learnStructure(survey, c(rep(1,ncol(dat_KPI)), rep(2,ncol(dat_DRIVER))))
bn_fit <- bn.fit(fit$dag, cbind(dat_KPI, dat_DRIVER))
diagonosis(survey[?,], bn_fit) 
