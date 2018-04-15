#################################Running cox regression elastic net (lambda1+lambda2)######################################################3

library(penalized)


rcox=read.table("../all_data.txt", header=T, row.names=1)
fit1 <- profL1(Surv(survival_time,status)~., data=rcox, fold=10, plot=TRUE)
fit2 <- profL2(Surv(survival_time,status)~., data=rcox,fold=fit1$fold, minl = 0.01, maxl = 1000)
opt1=optL1(Surv(survival_time,status)~., data=rcox, fold=fit1$fold)
opt2=optL2(Surv(survival_time,status)~., data=rcox, fold=fit2$fold)
fit=penalized(Surv(survival_time,status)~., data=rcox, lambda1=opt1$lambda, lambda2=opt2$lambda)
