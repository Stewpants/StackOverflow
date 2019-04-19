getwd()
setwd("C:/Users/roger/Documents/R Code/Analysis of Markets")

dat <- read.csv("2017 Chicago Pizza Dataset forR.csv", header=TRUE)

summary(dat)

dat$sale.feat <- 0
dat$price.feat <- 0

cov.sale <- cov(log(dat[,6:11]))
cov.price <- cov(log(dat[,12:17]))

for(i in 1:nrow(dat)){
  dat[i,]$sale.feat <- (as.matrix(log(dat[i,6:11])))  %*% cov.sale  %*% t(as.matrix(log(dat[i,6:11])))
}

for(i in 1:nrow(dat)){
  dat[i,]$price.feat <- (as.matrix(log(dat[i,12:17])))  %*% cov.price  %*% t(as.matrix(log(dat[i,12:17])))
}

plot(dat$price3, dat$sale3, main = "Units Sales as a Function of Price", xlab = "Price", ylab = "Jack's Unit Sales")
lines(lowess(dat$price3, dat$sale3), col="red", lwd=3)
abline(q4.mod, col= 'blue', lwd=3)
cor(dat$price3, dat$sale3)

plot(dat$price3, log(dat$sale3))
lines(lowess(dat$price3, log(dat$sale3)), col="red", lwd=3)
cor(dat$price3, log(dat$sale3))

q4.mod <- lm(sale3 ~ price3, data = dat)
summary(q4.mod)
anova(q4.mod)
confint(q4.mod)

predict(q4.mod, data.frame(price3=3))

par(mfrow=c(2,2))
plot(q4.mod)
par(mfrow=c(1,1))

q5.mod <- lm(sale3 ~ price3 + feature3 + display3, data = dat)
summary(q5.mod)
anova(q5.mod)
confint(q5.mod)

par(mfrow=c(2,2))
plot(q5.mod)
par(mfrow=c(1,1))

correlation <- cor(dat)

q7.mod <- lm(log(sale3) ~ log(price1) + log(price2) + log(price3)
               + log(price4) + log(price5) + log(price6) + feature3 + display3, data = dat)
summary(q7.mod)
anova(q7.mod)
confint(q7.mod)

par(mfrow=c(2,2))
plot(q7.mod)
par(mfrow=c(1,1))

which.max(dat$sale3)
dat[609,]
dat$week3 <- 0
dat[609,]$week3 <- 1
summary(dat$week3)

q9.mod <- lm(log(sale3) ~ log(price1) + log(price2) + log(price3)
               + log(price4) + log(price5) + log(price6) + feature3 + display3 + week3, data = dat)
summary(q9.mod)
anova(q9.mod)
confint(q9.mod)

par(mfrow=c(2,2))
plot(q9.mod)
par(mfrow=c(1,1))
    
table(dat$feature3, dat$display3)
table(dat$feature3, dat$display3)

dat$feat.only <- 0
dat$disp.only <- 0
dat$feat.disp <- 0

dat[which(dat$feature3 == 1 & dat$display3 == 0),]$feat.only <- 1
dat[which(dat$feature3 == 0 & dat$display3 == 1),]$disp.only <- 1
dat[which(dat$feature3 == 1 & dat$display3 == 1),]$feat.disp <- 1

q10.mod <- lm(log(sale3) ~ log(price1) + log(price2) + log(price3)
              + log(price4) + log(price5) + log(price6) 
              + feat.only + disp.only + week3 + feat.disp, data = dat)
summary(q10.mod)
anova(q10.mod)
confint(q10.mod)

par(mfrow=c(2,2))
plot(q10.mod)
par(mfrow=c(1,1))

dat$lag.sales <- 0
dat$lag.feat <- 0

for(i in 1:nrow(dat)){
  if(dat[i,]$week == 1){
    dat[i,]$lag.sales <- 0
    dat[i,]$lag.feat <- 0
  }
  else {
 dat[i,]$lag.sales <- dat[which(dat$store == dat[i,]$store & dat$week == dat[i,]$week - 1),]$sale3
 dat[i,]$lag.feat <- dat[which(dat$store == dat[i,]$store & dat$week == dat[i,]$week - 1),]$sale.feat 
  }
}

dat_new <- dat[-c(which(dat$week==1)),]
nrow(dat_new)
plot(log(dat_new$lag.sales), log(dat_new$sale3), main = 'Lag Sales versus Sales',
     xlab = 'Lag Sales', ylab = 'Sales')

q11.mod <- lm(log(sale3) ~ log(price1) + log(price2) + log(price3)
              + log(price4) + log(price5) + log(price6) 
              + feat.only + disp.only + week3 + feat.disp + log(lag.sales), data = dat_new)
summary(q11.mod)
anova(q11.mod)

par(mfrow=c(2,2))
plot(q11.mod)
par(mfrow=c(1,1))

plot(log(dat_new$sale3), q11.mod$fitted.values)
lines(lowess(log(dat_new$sale3), q11.mod$fitted.values), col="red", lwd=3)
anova(q11.mod)
confint(q11.mod)

q12.mod <- lm(log(sale3) ~ log(price1) + log(price2) + log(price3)
              + log(price4) + log(price5) + log(price6) + as.factor(store)
              + feature3 + display3 + week3 + feature3*display3 + log(lag.sales) + price.feat, data = dat_new)
summary(q12.mod)

par(mfrow=c(2,2))
plot(q12.mod)
par(mfrow=c(1,1))

plot(log(dat_new$sale3), q12.mod$fitted.values)
lines(lowess(log(dat_new$sale3), q12.mod$fitted.values), col="red", lwd=3)
anova(q12.mod)
confint(q12.mod)

q13.mod <- lm(log(sale3) ~ feature3 + display3 + week3 + feature3*display3 + lag.feat
              + log(lag.sales) + as.factor(store), data = dat_new)
summary(q13.mod)

par(mfrow=c(2,2))
plot(q13.mod)
par(mfrow=c(1,1))

plot(log(dat_new$sale3), q13.mod$fitted.values)
lines(lowess(log(dat_new$sale3), q13.mod$fitted.values), col="red", lwd=3)
anova(q13.mod)
confint(q13.mod)

pairs(log(sale3) ~ log(price1) + log(price2) + log(price3) + lag.feat
      + log(price4) + log(price5) + log(price6)  + price.feat , data = dat_new)
par(mfrow=c(1,1))

summary(dat)
  
