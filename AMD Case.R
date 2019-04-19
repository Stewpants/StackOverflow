getwd()
setwd("C:/Users/roger/Documents/R Code/People Analytics")

dat <- read.csv("AMD Performance Management - 2018.csv", header=TRUE)

summary(dat)


dat_retained <- dat[which(complete.cases(dat)),]
summary(dat_retained)

cor.table <- cor(dat_retained[,10:ncol(dat_retained)])

hist(log(dat$tenure))
hist(dat$level)
cor(log(dat$tenure), dat$level)
plot(log(dat$tenure), dat$level)

mod1 <- lm(qual_conv ~ as.factor(pilot)*as.factor(manager), data = dat_retained)

summary(mod1)

par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))

plot(dat_retained$perf_eval, mod1$fitted.values, col=ifelse(dat_retained$manager== 'Manager', 'red', 'blue'))
plot(dat_retained$perf_eval, mod1$fitted.values, col=ifelse(dat_retained$pilot== 'Pilot Group', 'red', 'blue'))

#####KNN####

myvars <- c("pilot", "manager", "tenure")
dat.sub <- dat_retained[,10:ncol(dat)]
dat.labels <- dat_retained[c("perf_eval")]

ind <- sample(2, nrow(dat.sub), replace=TRUE, prob=c(0.67, 0.33))
dat.train <- dat.sub[ind==1,]
dat.test <- dat.sub[ind==2,]

train.labels <- dat.labels[ind==1,]
test.labels <- dat.labels[ind==2,]

library(class)
mod.knn <- knn(train = dat.train, test = dat.test, cl = train.labels, k = 2)
summary(mod.knn)
plot(mod.knn)

test.labels <- data.frame(test.labels)
merge <- data.frame(mod.knn, test.labels)

merge

library(gmodels)

test.labels <- dat.labels[ind==2,]
CrossTable(x = test.labels, y = mod.knn, prop.chisq=FALSE)


####Fucking around####

mod2 <- lm(perf_eval ~., data=train1)
summary(mod2)


library(caret)
# Create index to split based on labels  
index <- createDataPartition(dat_retained$perf_eval, p=0.75, list=FALSE)

# Subset training set with index
iris.training <- dat_retained[index,]

# Subset test set with index
iris.test <- dat_retained[-index,]

# Overview of algos supported by caret
names(getModelInfo())

# Train a model
model_knn <- train(iris.training[, 10:ncol(iris.training)], iris.training[, 9], method='knn')

# Predict the labels of the test set
predictions<-predict(object=model_knn,iris.test[,10:ncol(iris.test)])

# Evaluate the predictions
table(predictions)

# Confusion matrix 
confusionMatrix(predictions,iris.test[,9])
plot(predictions,iris.test[,9])

library(caretEnsemble)

education <- iris.training[, 9:ncol(iris.training)]


####SVM Model####

TrainingDataIndex <- createDataPartition(education$qual_conv, p=0.75, list = FALSE)
# Create Training Data 
trainingData <- education[TrainingDataIndex,]
testData <- education[-TrainingDataIndex,]
TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=10)

SVModel <- train(as.factor(qual_conv) ~ ., data = trainingData,
                 method = "svmPoly",
                 trControl= TrainingParameters,
                 tuneGrid = data.frame(degree = 1,
                                       scale = 1,
                                       C = 1),
                 preProcess = c("pca","scale","center"),
                 na.action = na.omit
)
SVMPredictions <-predict(SVModel, testData)
test.pred <- data.frame(testData,SVMPredictions)
plot(test.pred$qual_conv, SVMPredictions)
# Create confusion matrix
cmSVM <-confusionMatrix(SVMPredictions, as.factor(testData$qual_conv))
print(cmSVM)


####Decision Tree ####
DecTreeModel <- train(as.factor(qual_conv) ~ ., data = trainingData, 
                      method = "C5.0",
                      preProcess=c("scale","center"),
                      trControl= TrainingParameters,
                      na.action = na.omit
)

#Predictions
DTPredictions <-predict(DecTreeModel, testData, na.action = na.pass)
# Print confusion matrix and results
cmTree <-confusionMatrix(DTPredictions, as.factor(testData$qual_conv))
print(cmTree)

####Naive Bayes####
NaiveModel <- train(trainingData[,1:ncol(trainingData)-1], as.factor(trainingData$qual_conv), 
                    method = "nb",
                    preProcess=c("scale","center"),
                    trControl= TrainingParameters,
                    na.action = na.omit
)

#Predictions
NaivePredictions <-predict(NaiveModel, testData, na.action = na.pass)
cmNaive <-confusionMatrix(NaivePredictions, as.factor(testData$qual_conv))
print(cmNaive)

#### Neural Network ####
NNModel <- train(trainingData[,1:ncol(trainingData)-1], as.factor(trainingData$qual_conv),
                 method = "nnet",
                 trControl= TrainingParameters,
                 preProcess=c("scale","center"),
                 na.action = na.omit
)

NNPredictions <-predict(NNModel, testData)
# Create confusion matrix
cmNN <-confusionMatrix(NNPredictions, as.factor(testData$qual_conv))
print(cmNN)

trainingData$qual_conv <- as.factor(trainingData$qual_conv)

econtrol <- trainControl(method="cv", number=10, savePredictions=TRUE, classProbs=FALSE)
model_list <- caretList(qual_conv ~., data=trainingData,
                        methodList=c("svmPoly", "nnet", "C5.0", "nb"),
                        preProcess=c("scale","center"),
                        trControl = econtrol
)
summary(model_list)
model_list$svmPoly
results <- resamples(model_list)

mcr <- modelCor(results)
print(mcr)

library(ggplot2)

