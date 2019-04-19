getwd()
setwd("C:/Users/roger/Documents/R Code/Analysis of Markets")

dat <- read.csv("2017 Tombstone Pizza data forR.csv", header=TRUE)
summary(dat)
str(dat)
dim(dat)

ls()

table(dat$feature, dat$display)
plot(dat$price, dat$usales)
hist(log(dat$usales))

max(dat$price)
which.max(dat$price)
sort(dat$price, decreasing = TRUE)

length((which(dat$usales>mean(dat$usales) + 2 * sd(dat$usales))))

dat[which(dat$usales==100),]
dat[which(dat$usales>mean(dat$usales) + 2 * sd(dat$usales)),]

dat[which(dat$store==155 & dat$quarter==1),]


dat[which(dat$usales < 20 | dat$usales > 2000),]

require(sqldf)

sqldf("SELECT * 
      FROM dat 
      WHERE usales = 100")

sqldf("SELECT * 
      FROM dat 
      WHERE store = 155 AND quarter = 1")

sqldf("SELECT * 
      FROM dat 
      WHERE usales < 20 OR usales > 2000")

aggregate(usales ~ store, data=dat, mean)
aggregate(dat$usales, by=list(Store=dat$store), sd)

unique(dat$store)

sqldf("SELECT store, SUM(usales), AVG(usales)
      FROM dat
      GROUP BY store")


reg <- lm(log(usales)~ price + factor(feature) + factor(display) + factor(dat$store) + week, data = dat)
summary(reg)
anova(reg)

par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))

prediction <- predict(reg, data=dat)

dat_new <- cbind(dat,prediction)

plot(log(dat_new$usales), dat_new$prediction)
lines(lowess(log(dat_new$usales), dat_new$prediction), col="red", lwd=3)

