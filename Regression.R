setwd('C:/Users/roger/Documents/R Code/Advance Statistics')
.libPaths("C:/Users/roger/Documents/R/win-library/3.4")
getwd()
library(dplyr)
library(mosaic)


rm(list=ls())

#############################################
# Salary Data                               #
#############################################

salary = read.table("SalaryData.txt",header=T)
# Turn the year hired variable into years of experience; the command
# below makes a new column in the salary data frame called Exp
salary$Exp = 96-salary$YrHired
View(salary)

salaryfit = lm(Salary~Gender, data=salary)
coef(salaryfit)
summary(salaryfit)
confint(salaryfit)

salaryfit_exp = lm(Salary~Gender+Exp, data=salary)
summary(salaryfit_exp)
coef(salaryfit_exp)
summary(salaryfit_exp)
confint(salaryfit_exp)

salaryfit_int = lm(Salary~Gender+Exp+Gender*Exp, data=salary)
summary(salaryfit_int)
coef(salaryfit_int)
summary(salaryfit_int)
confint(salaryfit_int)

par(mfrow = c(2,2))
plot(salaryfit_int)
par(mfrow = c(1,1))


Xfuture <- data.frame(Gender="Female",Exp=10)
yhat1 = predict(salaryfit_exp, Xfuture, interval = "prediction",se.fit=T,level=0.95)


# The plotModel function is very useful for 
# models with interactions
library(mosaic)
plotModel(salaryfit_exp, Salary~Exp)

## Another Housing Data
#######################

housing = read.csv("MidCity.csv",header=T)


# Rescale the response to be in $1000 & Size in 1000sqft
housing$Price = housing$Price/1000
housing$Size = housing$SqFt/1000

# We can also do some transformations *inside* of an lm formula
# For example, below we convert Nbhd into a factor
housing_fit = lm(Price~factor(Nbhd) + Size, data=housing)
coef(housing_fit)
summary(housing_fit)

Xfuture <- data.frame(Nbhd=rep(3,10),Size=seq(1,2,length=10))
yhat1 = predict(housing_fit, Xfuture, interval = "prediction",se.fit=T,level=0.95)

housing_int = lm(Price~factor(Nbhd)*Size, data=housing)
coef(housing_int)
summary(housing_int)

Xfuture <- data.frame(Nbhd=rep(3,10),Size=seq(1,2,length=10))
yhat1 = predict(housing_int, Xfuture, interval = "prediction",se.fit=T,level=0.95)

plotModel(housing_int,Price~Size)

housing_all = lm(Price~factor(Nbhd) + Offers + Bedrooms + Bathrooms + Size + factor(Brick), data=housing)
coef(housing_all)
summary(housing_all)

par(mfrow = c(2,2))
plot(housing_all)
par(mfrow = c(1,1))

Xfuture <- data.frame(Nbhd=rep(3,10),Size=seq(1,2,length=10))
yhat1 = predict(housing_all, housing, interval = "prediction",se.fit=T,level=0.95)
plot(yhat1$fit[,1], housing$Price)

## Back to Salary Data
############################

plotModel(salaryfit_exp, Salary~Exp)

salaryfit_int = lm(Salary~Gender*Exp, data=salary)
summary(salaryfit_int)

plotModel(salaryfit_int, Salary~Exp)


#######################################################
#  California Housing Example (Model Selection)
#######################################################
rm(list=ls())
library(glmnet)

ca <- read.csv("CAhousing.csv")
logMedVal <- log(ca$medianHouseValue)

ca <- ca[,-c(4,5,9)] # lose lmedval and the room totals
n = dim(ca)[1]
tr = sample(1:n,5000)


## create a full matrix of interactions (only necessary for linear model)
## do the normalization only for main variables.
XXca <- model.matrix(~.*longitude*latitude, data=data.frame(scale(ca)))[,-1]
CAdata = data.frame(logMedVal,XXca)

new.data <- data.frame(logMedVal, ca)

##Can replace CAdata with new.data
null = lm(logMedVal~1, data=CAdata[tr,])
full = lm(logMedVal~., data=CAdata[tr,])
summary(full)

regForward = step(null, scope=formula(full), direction="forward", k=log(length(tr)))
regBack = step(full, direction="backward", k=log(length(tr)))
regBoth = step(null, scope=formula(full), direction="both", k=log(length(tr)))

summary(regForward)
summary(regBack)
summary(regBoth)

XXca = scale(XXca)

Lasso.Fit = glmnet(XXca[tr,],logMedVal[tr])
Ridge.Fit = glmnet(XXca[tr,],logMedVal[tr],alpha=0)

par(mfrow=c(1,2))
plot(Lasso.Fit)
plot(Ridge.Fit)
par(mfrow=c(1,1))

CV.L = cv.glmnet(XXca[tr,], logMedVal[tr],alpha=1)
CV.R = cv.glmnet(XXca[tr,], logMedVal[tr],alpha=0)

LamR = CV.R$lambda.1se
LamL = CV.L$lambda.1se


par(mfrow=c(1,2))
plot(log(CV.R$lambda),sqrt(CV.R$cvm),main="Ridge CV (k=10)",xlab="log(lambda)",ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(LamR),lty=2,col=2,lwd=2)
plot(log(CV.L$lambda),sqrt(CV.L$cvm),main="LASSO CV (k=10)",xlab="log(lambda)",ylab = "RMSE",col=4,type="b",cex.lab=1.2)
abline(v=log(LamL),lty=2,col=2,lwd=2)


coef.R = predict(CV.R,type="coefficients",s=LamR)
coef.L = predict(CV.L,type="coefficients",s=LamL)

par(mfrow=c(1,1))
plot(abs(coef.R[2:20]),abs(coef.L[2:20]),ylim=c(0,1),xlim=c(0,1))
abline(0,1)
