setwd('C:/Users/roger/OneDrive/Documents/McCombs/2019 Spring/Financial Technology - Fracassi/GitHub/FinTech-MBA-ROGER-STEWART/HW6')
getwd()

library(psych)
#####Part 1.1####
load(file = 'training_10.rda')
load(file = 'testing_11.rda')

#####Part 1.2####
describeBy(training_10$CSCORE_B, training_10$Delq.90.days)
#Defaults have lower credit scores
describeBy(training_10$OLTV, training_10$Delq.90.days)
#Defaults have higher OLTV
describeBy(training_10$OCLTV, training_10$Delq.90.days)
#Defaults have higher OCLTV
describeBy(training_10$DTI, training_10$Delq.90.days)
#Defaults have higher DTI
#There is a noticeable difference for all of these, but the standard deviations overlap.
#This may show correlation but not statistically significant differences.
#However, there are many less defaults, which may affect the standard deviations for each ratio

state.mat <- describeBy(training_10$Delq.90.days, training_10$STATE, mat = TRUE)
#VI at 0.031128405 (Virgin Islands)
date.mat <- describeBy(training_10$Delq.90.days, substr(training_10$ORIG_DTE, start = 1, stop = 2), mat = TRUE)
#January at 0.006866441

#####Part 1.3####
rate.log = glm(Delq.90.days ~ ORIG_RT, family=binomial, data = training_10)
summary(rate.log)

#####Part 1.4####
rich.log = glm(Delq.90.days ~ CSCORE_B + OLTV + OCLTV + DTI + as.factor(OCC_STAT) + as.factor(STATE), 
               family=binomial, data = training_10)
summary(rich.log)

#####Part 1.5####
library(h2o)
h2o.init()

training_10$OCC_STAT <- as.factor(training_10$OCC_STAT)
training_10$STATE <- as.factor(training_10$STATE)
training_10$Delq.90.days <- as.factor(training_10$Delq.90.days)

testing_11$OCC_STAT <- as.factor(testing_11$OCC_STAT)
testing_11$STATE <- as.factor(testing_11$STATE)
testing_11$Delq.90.days <- as.factor(testing_11$Delq.90.days)

training.hex <- as.h2o(training_10, destination_frame = 'training.hex')
testing.hex <- as.h2o(testing_11, destination_frame = 'testing.hex')

h2o.log <- h2o.glm(y = "Delq.90.days", x = c("CSCORE_B","OLTV","OCLTV","DTI", "OCC_STAT", "STATE"), 
                   training_frame = training.hex, family = "binomial", nfolds = 5, keep_cross_validation_predictions = TRUE,
                   seed = 12)
summary(h2o.log)

rich.forest <- h2o.randomForest(y = "Delq.90.days", x = c("CSCORE_B","OLTV","OCLTV","DTI", "OCC_STAT", "STATE"), 
                   training_frame = training.hex, nfolds = 5, keep_cross_validation_predictions = TRUE,
                   seed = 12)
summary(rich.forest)

h2o.pred <- predict(rich.forest, testing.hex)
h2o.pred.df <- as.data.frame(h2o.pred)

#####Part 1.6####
library(pROC)

rate.pred <- predict(rate.log, testing_11)
rate.roc <- roc(testing_11$Delq.90.days, rate.pred)

rich.pred <- predict(rich.log, testing_11)
rich.roc <- roc(testing_11$Delq.90.days, rich.pred)

forest.roc <- roc(testing_11$Delq.90.days, h2o.pred.df[,2])

auc(rate.roc)
#0.6963
auc(rich.roc)
#0.8078
auc(forest.roc)
#0.7225

plot(rate.roc, col = 'black')
lines(rich.roc, col = 'green')
lines(forest.roc, col = 'blue')
#The rich logistic regression does the best. It has the highest ROC curve and AUC (See AUCs above)
#It out performs the other two significantly, with and AUC over 0.8
#The random forest may have overfit, or may not be the best alogorithm for this data
#You may be able to change the parameters to get a better fit


####Bonus####
dl.model <- h2o.deeplearning(y = "Delq.90.days", x = c("CSCORE_B","OLTV","OCLTV","DTI", "OCC_STAT", "STATE"), 
  training_frame = training.hex, hidden=c(10,10), epochs=0.1, nfolds = 5, keep_cross_validation_predictions = TRUE,
  seed = 12
  #balance_classes=T  ## enable this for high class imbalance
)

summary(dl.model) ## Now the model metrics contain AUC for binary classification

dl.pred <- predict(dl.model, testing.hex)
dl.pred.df <- as.data.frame(dl.pred)

dl.roc <- roc(testing_11$Delq.90.days, dl.pred.df[,2])
auc(forest.roc)
#0.7243

lines(dl.roc, col = 'red')

####Ensemble####
gbm.mod <- h2o.gbm(x = c("CSCORE_B","OLTV","OCLTV","DTI", "OCC_STAT", "STATE"), y = "Delq.90.days" , 
                   training_frame = training.hex, nfolds = 5, keep_cross_validation_predictions = TRUE,
                   seed = 12)


ensemble <- h2o.stackedEnsemble(x = c("CSCORE_B","OLTV","OCLTV","DTI", "OCC_STAT", "STATE"), y = "Delq.90.days" , 
                                training_frame = training.hex,
                                base_models = list(gbm.mod, rich.forest, gbm.mod, h2o.log, dl.model))

summary(ensemble) ## Now the model metrics contain AUC for binary classification

ensemble.pred <- predict(ensemble, testing.hex)
ensemble.pred.df <- as.data.frame(ensemble.pred)

ensemble.roc <- roc(testing_11$Delq.90.days, ensemble.pred.df[,2])
auc(ensemble.roc)
#0.8068

lines(ensemble.roc, col = 'magenta')