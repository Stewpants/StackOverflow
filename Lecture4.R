setwd('C:/Users/roger/Documents/R Code/Advance Statistics')
.libPaths("C:/Users/roger/Documents/R/win-library/3.4")
getwd()

library(kknn)
rm(list=ls())
#####################################
### Logistic Regression NBA data ####
#####################################

NBA = read.csv("NBAspread.csv")
attach(NBA)
n = nrow(NBA)

par(mfrow=c(1,2))
hist(NBA$spread[favwin==1], col=5, main="", xlab="spread")
hist(NBA$spread[favwin==0], add=TRUE, col=6)
legend("topright", legend=c("favwin=1", "favwin=0"), fill=c(5,6), bty="n")
boxplot(NBA$spread ~ NBA$favwin, col=c(6,5), horizontal=TRUE, ylab="favwin", xlab="spread")

nbareg = glm(favwin~spread-1, family=binomial)
s = seq(0,30,length=100)
fit = exp(s*nbareg$coef[1])/(1+exp(s*nbareg$coef[1]))

par(mfrow=c(1,1))
plot(s, fit, typ="l", col=4, lwd=2, ylim=c(0.5,1), xlab="spread", ylab="P(favwin)")

bic = cbind( extractAIC(nbareg, k=log(n)),
             extractAIC(glm(favwin ~ spread, family=binomial), k=log(n)),
             extractAIC(glm(favwin ~ spread + favhome, family=binomial), k=log(n)))[2,]



### Making predictions...

newdata=data.frame(spread=c(8,4))
predict(nbareg,newdata,type="response")

## or
pred = nbareg$coef[1]*newdata$spread
exp(pred)/(1+exp(pred))


par(mfrow=c(2,2))

plot(spread,favwin,type="n",main="linear regression")
points(spread[favwin==0],favwin[favwin==0],col="red",pch=19)
points(spread[favwin==1],favwin[favwin==1],col="blue",pch=19)
abline(lsfit(spread,favwin,intercept=FALSE),lwd=2,lty=2)


ind=order(spread)
plot(spread[ind],nbareg$fitted[ind], typ="l", col=1, lwd=2, lty=2,ylim=c(0,1), xlab="spread", ylab="favwin",main="logistic regression")
points(spread[favwin==0],favwin[favwin==0],col="red",pch=19)
points(spread[favwin==1],favwin[favwin==1],col="blue",pch=19)


train = data.frame(spread,as.factor(favwin))
test = data.frame(spread,as.factor(favwin))
ind = order(test[,1])
test =test[ind,]

near = kknn(as.factor(favwin)~spread,train,test,k=5,kernel = "rectangular")
plot(spread,favwin,type="n",main="knn(5)")
points(spread[favwin==0],favwin[favwin==0],col="red",pch=19)
points(spread[favwin==1],favwin[favwin==1],col="blue",pch=19)
lines(test[,1],near$prob[,2],col=1,lwd=2,lty=2)


near = kknn(as.factor(favwin)~spread,train,test,k=20,kernel = "rectangular")
plot(spread,favwin,type="n",main="knn(20)")
points(spread[favwin==0],favwin[favwin==0],col="red",pch=19)
points(spread[favwin==1],favwin[favwin==1],col="blue",pch=19)
lines(test[,1],near$prob[,2],col=1,lwd=2,lty=2)

### German Credit Scoring Data ###
#################################
rm(list=ls())
credit = read.csv("germancredit.csv")
train = 1:800

# build a model assuming you have credit history in there (a good idea) ## Stepwise regression

null = glm(GoodCredit~history3, family=binomial, data=credit[train,])
full = glm(GoodCredit~., family=binomial, data=credit[train,])
reg = step(null, scope=formula(full), direction="forward", k=log(length(train)))

# Now predictiction: the function defaults to give X'b,
# so you tell it to give you predictions of type="response"
predreg = predict(reg, newdata=credit[-train,], type="response") 
predfull = predict(full, newdata=credit[-train,], type="response") 
prednull = predict(null, newdata=credit[-train,], type="response") 

errorreg = credit[-train,1]-(predreg >= .5) # 1 is a false negative, -1 is a false positive
errorfull = credit[-train,1]-(predfull >= .5)
errornull = credit[-train,1]-(prednull >= .5)

# misclassification rates out of sample:
mean(abs(errorreg))
mean(abs(errorfull))
mean(abs(errornull))


rm(list=ls())
#--------------------------------------------------
#get Default data from book
library(ISLR)
data(Default)
attach(Default)
#--------------------------------------------------
#plot default (binary y) vs balance (numeric x)

par(mfrow=c(1,1))
plot(balance~default,data=Default,col=c('lightblue','orange'),cex.axis=1.5,cex.lab=1.5)


##################################################
#plot logistic function
##################################################

z = seq(from=-5,to=5,length.out=1000)
Fz = exp(z)/(1+exp(z))
plot(z,Fz,type='l',col='blue',lwd=2.5,xlab=expression(eta),ylab='F',cex.lab=1.3)

##################################################
# Do the logit fit for default ~ balance, show the two steps.
##################################################
#--------------------------------------------------
#fit logit for default~balance

F = function(x) {exp(x)/(1+exp(x))}

glm.fit = glm(default~balance,data=Default,family=binomial)
eta = predict(glm.fit) #get eta
pyx = predict(glm.fit,type='response') #get phat = F(eta)

par(mfrow=c(2,1))
n=nrow(Default)
ii = sample(1:n,50)

plot(balance,eta)
z = seq(from=-11,to=8,length.out=1000)
points(balance[ii],eta[ii],pch=16,col='red')
title(main='eta = -10.65 + .0055 balance',cex.main=1.5)

oo = order(balance)
plot(balance[oo],pyx[oo],type='l',col='red',xlab='x=balance',ylab='P(Y=Yes | x=balance)',
     lwd=2)
title(main='x=balance vs. P(Y=Yes | balance)',cex.main=1.5)

print(summary(glm.fit))

##################################################
# Fit logit with various choices for x from Default data frame
##################################################
#fit logit for default using various number of x
glm.fit = glm(default~balance+student+income,data=Default,family=binomial)
phat = predict(glm.fit,type='response')
glm.fit.1 = glm(default~balance,data=Default,family=binomial)
phat.1 = predict(glm.fit.1,type='response')
glm.fit.2 = glm(default~student,data=Default,family=binomial)
phat.2 = predict(glm.fit.2,type='response')
glm.fit.3 = glm(default~balance+student,data=Default,family=binomial)
phat.3 = predict(glm.fit.3,type='response')

#--------------------
#plot balance vs. phat for students and non-students using default~balance+student fit.
par(mfrow=c(1,1))
plot(range(balance),range(c(phat.3,phat.1,phat.2)),xlab='balance',ylab='P(Y=1|x)',type='n',cex.lab=1.3)
ii=student=="Yes"
points(balance[ii],phat.3[ii],col='orange')
ii=student=="No"
points(balance[ii],phat.3[ii],col='lightblue')
legend('topleft',legend=c('student','not-student'),col=c('orange','lightblue'),pch=1)

#--------------------
#write out regression output for default~balance+student+income

print(summary(glm.fit))


#--------------------
#plot phats with and without income
plot(phat,phat.3,xlab='phat all Xs',ylab='phat without income',cex.lab=1.3)
abline(0,1,col='red')

#--------------------
#write out regression output for default~balance+student+income

print(summary(glm.fit.3))


#--------------------
#write out regression output for default~student

print(summary(glm.fit.2))


##################################################
# lift function.
##################################################
#--------------------------------------------------
#plots lift function
liftf = function(yl,phatl,dopl=TRUE) {
  oo = order(-phatl)
  sy = cumsum(yl[oo])/sum(yl==1)
  if(dopl) {
    ii = (1:length(sy))/length(sy)
    plot(ii,sy,type='l',lwd=2,col='blue',xlab='% tried',ylab='% of successes',cex.lab=1.5)
    abline(0,1,lty=2)
  }
  return(sy)
}



#--------------------------------------------------
#fit simple logit 
glm.b = glm(default~balance,data=Default,family=binomial) 
phat.b = predict(glm.b,type='response') 
#--------------------------------------------------
#do for one s
s=.5
yhat = ifelse(phat.b<s,0,1)
y = as.numeric(default)-1 #all
tbl = table(yhat,y)

print(tbl)

#--------------------------------------------------
#compute lift and roc
ns=1000
sv = seq(from=.0,to=.99,length.out=ns)
FP=rep(0,ns)
TP=rep(0,ns)
N=rep(0,ns)
n0=sum(y==0)
for(i in 1:ns) {
  N[i] = sum(phat.b>sv[i])/length(y)
  TP[i] = sum((phat.b>sv[i]) & (y==1))/sum(y==1)
  FP[i] = sum((phat.b>sv[i]) & (y==0))/sum(y==0)
}

par(mfrow=c(1,3))
par(mai=c(0.9,0.9,.4,.4))
plot(sv,N,xlab='s',type='l',col='blue',cex.lab=2.0)
title(main='s vs. N',cex.main=2)
plot(sv,TP,xlab='s',type='l',col='blue',cex.lab=2.0)
title(main='s vs. TP',cex.main=2)
plot(sv,FP,xlab='s',type='l',col='blue',cex.lab=2.0)
title(main='s vs. FP',cex.main=2)

par(mai=c(0.9,0.9,.4,.4))
par(mfrow=c(1,2))
plot(FP,TP,type='l',col='blue',cex.lab=2.0)
abline(0,1,lty=2)
title(main='ROC',cex.main=2)
plot(N,TP,type='l',col='blue',cex.lab=2.0)
abline(0,1,lty=2)
title(main='Lift',cex.main=2)
temp = liftf(y,phat.b,dopl=FALSE)
lines((1:length(y))/length(y),temp,col='red',lty=3)


temp = liftf(y,phat.b)


Default$balsq = Default$balance^2
#--------------------------------------------------
#train/test
set.seed(99)
n=nrow(Default)
ntrain=floor(.75*n)
iitrain = sample(1:n,ntrain)
Deftrain = Default[iitrain,]
Deftest = Default[-iitrain,]
#--------------------------------------------------
#more on lift
glm.b = glm(default~balance,Deftrain,family=binomial)
phat.b = predict(glm.b,newdata=Deftest,type='response')

glm.i = glm(default~income,Deftrain,family=binomial)
phat.i = predict(glm.i,newdata=Deftest,type='response')

glm.b2 = glm(default~balance+balsq,Deftrain,family=binomial)
phat.b2 = predict(glm.b2,newdata=Deftest,type='response')

ytest = as.numeric(Deftest$default)-1

l.b = liftf(ytest,phat.b,dopl=FALSE)
l.i = liftf(ytest,phat.i,dopl=FALSE)
l.b2 = liftf(ytest,phat.b2,dopl=FALSE)

lmat = cbind(l.i,l.b,l.b2)
rgy = range(lmat)
ii = (1:nrow(lmat))/nrow(lmat)
rgx = range(ii)

plot(rgx,rgy,xlab='N',ylab='lift',type='n')
for(i in 1:ncol(lmat)) 
  lines(ii,lmat[,i],col=i+1)
abline(0,1,lty=2)
legend('bottomright',legend=c('income','balance','balance+square'),col=c(2,3,4),lwd=3)

###################################################
## Fit a regression tree to medv~lstat in the boston housing data.
## The tree is plotted as well as a plot of the corresponding step function
## fit to the data.
## The cutpoints from tree are added to the plot so you can see how
## the tree corresponds to the function.
###################################################

library(tree)
library(MASS)

head(Default)

#first get a big tree using a small value of mindev
temp = tree(default~student + balance + income ,data=Default,mindev=.0001)
cat('first big tree size: \n')
print(length(unique(temp$where)))
summary(temp)

boston.tree=prune.tree(temp,best=7)
cat('pruned tree size: \n')
print(length(unique(boston.tree$where)))
summary(boston.tree)

#plot the tree and the fits.
par(mfrow=c(1,1))

#plot the tree
plot(boston.tree,type="uniform")
text(boston.tree,col="blue",label=c("yval"),cex=.8)
boston.fit = predict(boston.tree) #get training fitted values

library(randomForest)
library(MASS)

#--------------------------------------------------
#get rf fits for different number of trees
#note: to get this to work I had to use maxnodes parameter of randomForest!!!
set.seed(99)
n = nrow(Default)
ntreev = c(10,500,5000)
nset = length(ntreev)
fmat = matrix(0,n,nset)
for(i in 1:nset) {
  cat('doing Boston rf: ',i,'\n')
  rffit = randomForest(as.factor(default)~student + balance + income ,data=Default ,ntree=ntreev[i],maxnodes=15)
  fmat[,i] = predict(rffit)
}
#--------------------------------------------------
#plot oob error using last fitted rffit which has the largest ntree.


par(mfrow=c(1,1))
plot(rffit)

rfbest$confusion

library(pROC)
rf.roc<-roc(Default$default,rffit$votes[,2])

plot(rf.roc)
auc(rf.roc)


