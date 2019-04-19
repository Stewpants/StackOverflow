getwd()
setwd("C:/Users/roger/Documents/R Code/Analysis of Markets")

dat <- read.csv("2017 ConceptTest Data forR.csv", header=TRUE)


summary(lm(likelihood~X,data=dat))$coef
summary(lm(likelihood~X==0,data=dat))$coef
t.test(dat$likelihood[dat$X==0],dat$likelihood[dat$X==1],var.equal=TRUE)
chisq.test(table(dat$likelihood,dat$X))
