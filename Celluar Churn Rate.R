getwd()
setwd("C:/Users/roger/Documents/R Code/Analysis of Markets")

dat <- read.csv("2017 CellularChurn Data forR.csv", header=TRUE)

summary(dat)

prop.table(table(dat$LowCreditScore))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(dat$RecurringCharges)

mod1 <- glm(Churn ~ Age + Children + LowCreditScore + RetentionCall
            + RecurringCharges + MinutesOverage, data = dat, family = binomial(link = 'logit'))

summary(mod1)

modpred <- predict(mod1, type = 'response')
dat$prob <- modpred

dat$fit <- ifelse(modpred>.5,1,0)

table(dat$Churn,dat$fit)

##Value that increase probability are Children, Retentional Call, and Minutes Overage

new <- data.frame(45, 0, 0, 0, 49.99, 0)
names(new) <- c("Age", 'Children', 'LowCreditScore', 'RetentionCall', 'RecurringCharges', 'MinutesOverage')
pred1 <- predict(mod1, new, type = 'response')

new <- data.frame(45, 0, 1, 0, 49.99, 0)
names(new) <- c("Age", 'Children', 'LowCreditScore', 'RetentionCall', 'RecurringCharges', 'MinutesOverage')
pred2 <- predict(mod1, new, type = 'response')

new <- data.frame(45, 0, 0, 1, 49.99, 0)
names(new) <- c("Age", 'Children', 'LowCreditScore', 'RetentionCall', 'RecurringCharges', 'MinutesOverage')
pred3 <- predict(mod1, new, type = 'response')

pred3 * 49.99


