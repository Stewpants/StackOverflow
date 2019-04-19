setwd('C:/Users/roger/Documents/R Code/Advance Statistics')
.libPaths("C:/Users/roger/Documents/R/win-library/3.4")
getwd()

###################################################
## Fit a regression tree to medv~lstat in the boston housing data.
## The tree is plotted as well as a plot of the corresponding step function
## fit to the data.
## The cutpoints from tree are added to the plot so you can see how
## the tree corresponds to the function.
###################################################

library(tree)
library(MASS)
data(Boston)
attach(Boston)

#--------------------------------------------------
#fit a tree to boston data just using lstat.

#first get a big tree using a small value of mindev
temp = tree(medv~lstat,data=Boston,mindev=.0001)
cat('first big tree size: \n')
print(length(unique(temp$where)))
summary(temp)

#then prune it down to one with 7 leaves
boston.tree=prune.tree(temp,best=7)
cat('pruned tree size: \n')
print(length(unique(boston.tree$where)))
summary(boston.tree)

#--------------------------------------------------
#plot the tree and the fits.
par(mfrow=c(1,2))

#plot the tree
plot(boston.tree,type="uniform")
text(boston.tree,col="blue",label=c("yval"),cex=.8)

#plot data with fit
boston.fit = predict(boston.tree) #get training fitted values

plot(lstat,medv,cex=.5,pch=16) #plot data
oo=order(lstat)
lines(lstat[oo],boston.fit[oo],col='red',lwd=3) #step function fit

cvals=c(9.725,4.65,3.325,5.495,16.085,19.9) #cutpoints from tree
for(i in 1:length(cvals)) abline(v=cvals[i],col='magenta',lty=2) #cutpoints

rm(list=ls())

################################################################################
## Fit a regression tree to mev~dis+lstat from the Boston housing data.
## The tree is plotted as well as the corresponding partition of the two-dimensional
## x=(dis,lstat) space.
################################################################################

library(MASS)
data(Boston)
attach(Boston)
library(tree)

#--------------------------------------------------
df2=Boston[,c(8,13,14)] #pick off dis,lstat,medv
print(names(df2))

#--------------------------------------------------
#big tree
temp = tree(medv~.,df2,mindev=.0001)
cat('first big tree size: \n')
print(length(unique(temp$where)))

#--------------------------------------------------
#then prune it down to one with 7 leaves
boston.tree=prune.tree(temp,best=7)
cat('pruned tree size: \n')
print(length(unique(boston.tree$where)))

#--------------------------------------------------
# plot tree and partition in x.
par(mfrow=c(1,2))
plot(boston.tree,type="u")
text(boston.tree,col="blue",label=c("yval"),cex=.8)
partition.tree(boston.tree)

rm(list=ls())

################################################################################
## Read in the California Housing data.
## Transform some of the variables.
## Divide data into train,val,test.
################################################################################

source('cal_setup.R')

################################################################################
# Fit a regression tree to California Housing data:
#    logMedVal ~ longitude+latitude.
# Plot: The tree and the 2-dim partitoon.
# Plot: The California map with the fits from the tree coded with colors.
################################################################################

library(tree)
library(maps)


#--------------------------------------------------
#first get big tree
temp = tree(logMedVal~longitude+latitude,catrain,mindev=.0001)
cat('first big tree size: \n')
print(length(unique(temp$where)))
#then prune it
caltrain.tree = prune.tree(temp,best=50)
cat('pruned tree size: \n')
print(length(unique(caltrain.tree$where)))
help(prune.tree)

#--------------------------------------------------
#plot the tree
par(mfrow=c(1,2))
plot(caltrain.tree,type="u")
text(caltrain.tree,col="blue",label=c("yval"),cex=.8)
partition.tree(caltrain.tree)




rm(list=ls())


################################################################################
## Fit a big tree to medv~lstat in the Boston housing data using rpart instead of tree
##  and using cross-validation.
## Use rpart plotcp so do cross-validation.
## Plot: rpart plotcp cross-validation.
## Big off min loss cp value and plot tree for that cp value as well 
## as a bigger cp value (smaller tree) and a smaller cp value (bigger tree).
## Plot: the three trees (from the three cp values) as well as the fitted function.
## Plot: the best tree using rpart.
################################################################################


library(tree)
library(rpart)
library(MASS)
data(Boston)
attach(Boston)
#--------------------------------------------------
#reduce df to just lmed and lrat
bdf = Boston[,c(13,14)] #lstat and medv
#--------------------------------------------------
#fit a big tree using rpart.control
big.tree = rpart(medv~lstat,method="anova",data=bdf,
                        control=rpart.control(minsplit=5,cp=.0005))
nbig = length(unique(big.tree$where))
cat('size of big tree: ',nbig,'\n')

#--------------------------------------------------
#look at cross-validation
par(mfrow=c(1,1))
plotcp(big.tree)

#--------------------------------------------------
#show fit from some trees

oo=order(bdf$lstat)
bestcp=big.tree$cptable[which.min(big.tree$cptable[,"xerror"]),"CP"]
cat('bestcp: ',bestcp,'\n')
cpvec = c(.0157,bestcp,.004)
par(mfrow=c(3,2))
for(i in 1:3) {
   plot(bdf,pch=16,col='blue',cex=.5)
   ptree = prune(big.tree,cp=cpvec[i])
   pfit = predict(ptree)
   lines(bdf$lstat[oo],pfit[oo],col='red',lwd=2)
   title(paste('alpha = ',round(cpvec[i],3)))
   plot(ptree,uniform=TRUE)
   text(ptree,digits=4)
}

#--------------------------------------------------
#plot best tree

par(mfrow=c(1,1))
best.tree = prune(big.tree,cp=bestcp)
plot(best.tree,uniform=TRUE,branch=.5,margine=.5)
text(best.tree,digits=4,use.n=TRUE,fancy=TRUE,bg='lightblue')

rm(list=ls())



################################################################################
## Random Forests: fit medv~lstat using random forests.
## Plot: oob error esitmation.
## Plot: fit from random forests for three different number of trees in forest.
################################################################################


library(randomForest)
library(MASS)
data(Boston)
attach(Boston)

#--------------------------------------------------
#get rf fits for different number of trees
#note: to get this to work I had to use maxnodes parameter of randomForest!!!
set.seed(99)
n = nrow(Boston)
ntreev = c(10,500,5000)
nset = length(ntreev)
fmat = matrix(0,n,nset)
for(i in 1:nset) {
   cat('doing Boston rf: ',i,'\n')
   rffit = randomForest(medv~lstat,data=Boston,ntree=ntreev[i],maxnodes=15)
   fmat[,i] = predict(rffit)
}
#--------------------------------------------------
#plot oob error using last fitted rffit which has the largest ntree.


par(mfrow=c(1,1))
plot(rffit)

#--------------------------------------------------
#plot fits

par(mfrow=c(1,3))
oo = order(Boston$lstat)
for(i in 1:nset) {
   plot(Boston$lstat,Boston$medv,xlab='lstat',ylab='medv')
   lines(Boston$lstat[oo],fmat[oo,i],col=i,lwd=3)
   title(main=paste('bagging ntrees = ',ntreev[i]))
}

#--------------------------------------------------
rm(list=ls())

################################################################################
## Fit medv~lstat, Boston Housing using boosting.
## Plot: fits for three different values of number of trees.
################################################################################


library(gbm) #boost package
library(MASS)
data(Boston)
attach(Boston)
#--------------------------------------------------
#fit boosting for various number of trees
set.seed(99)
n = nrow(Boston)
ntreev = c(5,20,100)
nset = length(ntreev)
fmat = matrix(0,n,nset)
for(i in 1:nset) {
   cat('doing Boston boost: ',i,'\n')
   boostfit = gbm(medv~lstat,data=Boston,distribution='gaussian',
                interaction.depth=2,n.trees=ntreev[i],shrinkage=.2)
   fmat[,i] = predict(boostfit,n.trees=ntreev[i])
}
#--------------------------------------------------
#plot fits

par(mfrow=c(1,3))
oo = order(Boston$lstat)
for(i in 1:nset) {
   plot(Boston$lstat,Boston$medv,xlab='lstat',ylab='medv')
   lines(Boston$lstat[oo],fmat[oo,i],col=i+1,lwd=3,lty=1)
   title(main=paste('boosting, ntree= ',ntreev[i]))
}

#--------------------------------------------------
rm(list=ls())


################################################################################
## Variable Importance:
## Fit boosting and random forests and plot variable importance.
################################################################################

library(gbm) #boost package
library(randomForest) 
library(MASS)
data(Boston)
attach(Boston)

#--------------------------------------------------
#fit boost and plot  variable importance
boostfit = gbm(medv~.,data=Boston,distribution='gaussian',
                interaction.depth=2,n.trees=100,shrinkage=.2)

par(mfrow=c(1,1))
p=ncol(Boston)-1
vsum=summary(boostfit,plotit=FALSE) #this will have the variable importance info
row.names(vsum)=NULL #drop varable names from rows.

#plot variable importance
#the package does this automatically, but I did not like the plot

plot(vsum$rel.inf,axes=F,pch=16,col='red')
axis(1,labels=vsum$var,at=1:p)
axis(2)
for(i in 1:p) lines(c(i,i),c(0,vsum$rel.inf[i]),lwd=4,col='blue')

#--------------------------------------------------
#fit random forest and plot variable importance

rffit = randomForest(medv~.,data=Boston,mtry=3,ntree=500)

varImpPlot(rffit)


rm(list=ls())




################################################################################
## California Housing data, fit regression trees on train, get loss on validation
## for a vector of cp values.
## Get cp values from rpart.
## Write fits (on validation) from best tree to file (thetreepred.txt)
################################################################################


source('cal_setup.R')
library(rpart)
#--------------------------------------------------
#get big tree
big.tree = rpart(logMedVal~.,method="anova",data=catrain,
               control=rpart.control(minsplit=5,cp=.0001)) 
nbig = length(unique(big.tree$where))
cat('size of big tree: ',nbig,'\n')
#--------------------------------------------------
#fit on train, predict on val for vector of cp.
cpvec = big.tree$cptable[,"CP"] #cp values to try
ntree = length(cpvec) #number of cv values = number of trees fit.
iltree = rep(0,ntree) #in-sample loss
oltree = rep(0,ntree) #out-of-sample loss
sztree = rep(0,ntree) #size of each tree
for(i in 1:ntree) {
   if((i %% 10)==0) cat('tree i: ',i,'\n')
   temptree = prune(big.tree,cp=cpvec[i])
   sztree[i] = length(unique(temptree$where))
   iltree[i] = sum((catrain$logMedVal-predict(temptree))^2)
   ofit = predict(temptree,caval)
   oltree[i] = sum((caval$logMedVal-ofit)^2)
}
oltree=sqrt(oltree/nrow(caval)); iltree = sqrt(iltree/nrow(catrain))
#--------------------------------------------------
#plot losses

rgl = range(c(iltree,oltree))
plot(range(sztree),rgl,type='n',xlab='tree size',ylab='loss')
points(sztree,iltree,pch=15,col='red')
points(sztree,oltree,pch=16,col='blue')
legend("topright",legend=c('in-sample','out-of-sample'),lwd=3,col=c('red','blue'))

#--------------------------------------------------
#write val preds
iitree = which.min(oltree)
thetree = prune(big.tree,cp=cpvec[iitree])
thetreepred = predict(thetree,caval)
write(thetreepred,file='thetreepred.txt',ncol=1)
#--------------------------------------------------
rm(list=ls())


################################################################################
## California Housing data: fit boosting for values of 
##   (i) depth (ii) number of trees (iii) lamda = shrinkage.
## Fit on train, get loss on validation.
## Write fits on validition from best to file thebpred-2.txt.
################################################################################



source('cal_setup.R')
library(gbm)
#--------------------------------------------------
set.seed(1)
idv = c(4,10)
ntv = c(1000,5000)
lamv=c(.001,.2)
parmb = expand.grid(idv,ntv,lamv)
colnames(parmb) = c('tdepth','ntree','lam')
print(parmb)
nset = nrow(parmb)
olb = rep(0,nset)
ilb = rep(0,nset)
bfitv = vector('list',nset)
for(i in 1:nset) {
    cat('doing boost ',i,' out of ',nset,'\n')
   tempboost = gbm(logMedVal~.,data=catrain,distribution='gaussian',
                interaction.depth=parmb[i,1],n.trees=parmb[i,2],shrinkage=parmb[i,3])
   ifit = predict(tempboost,n.trees=parmb[i,2])
   ofit=predict(tempboost,newdata=caval,n.trees=parmb[i,2])
   olb[i] = sum((caval$logMedVal-ofit)^2)
   ilb[i] = sum((catrain$logMedVal-ifit)^2)
   bfitv[[i]]=tempboost
}
ilb = round(sqrt(ilb/nrow(catrain)),3); olb = round(sqrt(olb/nrow(caval)),3)
#--------------------------------------------------
#print losses

print(cbind(parmb,olb,ilb))

#--------------------------------------------------
#write val preds
iib=which.min(olb)
theb = bfitv[[iib]] 
thebpred = predict(theb,newdata=caval,n.trees=parmb[iib,2])
write(thebpred,file='thebpred-2.txt',ncol=1)

rm(list=ls())


################################################################################
## California Housing Data: fit random forests on train, get loss on validation
## using values of (i) mtry (ii) number of trees.
## Write out fits on validation from best t file  therfpred.txt.
################################################################################



source('cal_setup.R')
library(randomForest)
#--------------------------------------------------
set.seed(1)
p=ncol(catrain)-1
mtryv = c(p,sqrt(p))
ntreev = c(100,500)
parmrf = expand.grid(mtryv,ntreev)
colnames(parmrf)=c('mtry','ntree')
nset = nrow(parmrf)
olrf = rep(0,nset)
ilrf = rep(0,nset)
rffitv = vector('list',nset)
for(i in 1:nset) {
    cat('doing rf ',i,' out of ',nset,'\n')
   temprf = randomForest(logMedVal~.,data=catrain,mtry=parmrf[i,1],ntree=parmrf[i,2])
   ifit = predict(temprf)
   ofit=predict(temprf,newdata=caval)
   olrf[i] = sum((caval$logMedVal-ofit)^2)
   ilrf[i] = sum((catrain$logMedVal-ifit)^2)
   rffitv[[i]]=temprf
}
ilrf = round(sqrt(ilrf/nrow(catrain)),3); olrf = round(sqrt(olrf/nrow(caval)),3)
#----------------------------------------
#print losses

print(cbind(parmrf,olrf,ilrf))

#----------------------------------------
#write val preds
iirf=which.min(olrf)
therf = rffitv[[iirf]]
therfpred=predict(therf,newdata=caval)
write(therfpred,file='therfpred.txt',ncol=1)

rm(list=ls())


################################################################################
## California Housing Data: fit best boosting on (train, val), get fit on test.
## Get rmse on test.
## Write file: finbrmse.txt with just rmse on test.
## Plot: fit vs. y on test.
## Plot: variable importance.
## Plot: partial dependence plots. 
## Write file: cal-boost-varimport.txt with variable importance measures.
################################################################################


source('cal_setup.R')
library(gbm)

#--------------------------------------------------
#fit on train+val
set.seed(1)
catrainval = rbind(catrain,caval)
ntrees=5000
finb = gbm(logMedVal~.,data=catrainval,distribution='gaussian',
              interaction.depth=4,n.trees=ntrees,shrinkage=.2)
finbpred=predict(finb,newdata=catest,n.trees=ntrees)
#--------------------------------------------------
#plot y vs yhat for test data and compute rmse on test.


finbrmse = sqrt(sum((catest$logMedVal-finbpred)^2)/nrow(catest))
cat('finbrmse: ',finbrmse,'\n')
plot(catest$logMedVal,finbpred,xlab='test logMedVal',ylab='boost pred')
abline(0,1,col='red',lwd=2)

#--------------------------------------------------
#plot variable importance
p=ncol(catrain)-1 #want number of variables for later
vsum=summary(finb) #this will have the variable importance info
row.names(vsum)=NULL #drop varable names from rows.



#write variable importance table
cat('\\begin{verbatim}\n')
print(vsum)
cat('\\end{verbatim}\n')

#plot variable importance
#the package does this automatically, but I did not like the plot
plot(vsum$rel.inf,axes=F,pch=16,col='red')
axis(1,labels=vsum$var,at=1:p)
axis(2)
for(i in 1:p) lines(c(i,i),c(0,vsum$rel.inf[i]),lwd=4,col='blue')


#--------------------------------------------------
rm(list=ls())


################################################################################
## California Housing: fit random forests on train+validation using best.
## Predict on test.
## Plot: fits vs test y.
## Plot: Variable importance.
## Write: rmse on test to file finrfrmse.txt. 
################################################################################


source('cal_setup.R')
library(randomForest)

#--------------------------------------------------
#fit on train+val
set.seed(1)
catrainval = rbind(catrain,caval)
finrf = randomForest(logMedVal~.,data=catrainval,mtry=3,ntree=500)
finrfpred=predict(finrf,newdata=catest)
#--------------------------------------------------
#plot y vs yhat for test data

finrfrmse = sqrt(sum((catest$logMedVal-finrfpred)^2)/nrow(catest))
cat('finrfrmse: ',finrfrmse,'\n')
plot(catest$logMedVal,finrfpred,xlab='test logMedVal',ylab='rf pred')
abline(0,1,col='red',lwd=2)

#--------------------------------------------------
#plot variable importance

varImpPlot(finrf)

#--------------------------------------------------
rm(list=ls())

####Ranger####
library(ranger)
source('cal_setup.R')

ranger.mod <- ranger(logMedVal~., data = catrain, mtry = 3, num.trees = 500)
ranger.pred <- predict(ranger.mod, data = catest)

ranger.rmse <- sqrt(sum((catest$logMedVal - ranger.pred$predictions)^2)/nrow(catest))

cat('ranger.rmse: ', ranger.rmse, '\n')
plot(catest$logMedVal, ranger.pred$predictions, xlab = 'test logMedVal', ylab = 'rf pred')
abline(0,1,col = 'red', lwd = 2)

####xgboost####
library(xgboost)
library(Matrix)
source('cal_setup.R')

train.mtx <- sparse.model.matrix(logMedVal ~ ., data = catrain)[,-1]
test.mtx <- sparse.model.matrix(logMedVal ~ ., data = catest)[,-1]
head(train.mtx)

bst <- xgboost(data = train.mtx, label = catrain$logMedVal, max_depth = 5,
               eta = 0.3, nrounds = 500, nthread = 2)

bst.pred <- predict(bst, newdata = test.mtx)

bst.rmse <- sqrt(sum((catest$logMedVal - bst.pred)^2)/nrow(catest))

cat('bst.rmse: ', bst.rmse, '\n')
plot(catest$logMedVal, bst.pred, xlab = 'test logMedVal', ylab = 'rf pred')
abline(0,1,col = 'red', lwd = 2)

importance <- xgb.importance(feature_names = colnames(train.mtx), model = bst)
xgb.plot.importance(importance_matrix = importance)

####Parameter Tuning####
#https://www.kaggle.com/pelkoja/visual-xgboost-tuning-with-caret
library(caret)
library(dplyr)
library(ggplot2)
library(glue)
library(ModelMetrics)
library(OpenMPController) # for Kaggle backend
library(readr)
library(vtreat)
library(xgboost)

nrounds = 1000
input_x <- as.matrix(select(catrain, -logMedVal))
input_y <- as.matrix(catrain$logMedVal)

grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "xgbLinear",
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_base <- caret::train(
  x = input_x,
  y = input_y,
  trControl = train_control,
  tuneGrid = grid_default,
  method = "xgbLinear",
  verbose = TRUE
)

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_tune <- caret::train(
  x = input_x,
  y = input_y,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbLinear",
  verbose = TRUE
)

# helper function for the plots
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

tuneplot(xgb_tune)