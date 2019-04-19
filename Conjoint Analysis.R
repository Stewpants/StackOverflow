setwd('C:/Users/roger/Documents/R Code/Data Analytics & Dynamic Pricing')
.libPaths("C:/Users/roger/Documents/R/win-library/3.4")
getwd()

cj.design <- read.csv('conjoint_design.csv')
cj.re <- read.csv('DADP Project.csv')
summary(cj.re)

library(DataCombine)

####MNL Regression####
#http://r-marketing.r-forge.r-project.org/slides/chapter13-phillyR/ConjointR20150418.html#/12
library(mlogit)

cbc.mlogit <- 
  mlogit.data(data=cj.re, choice="picked", 
              shape="long", varying=5:ncol(cj.re), 
              alt.levels=paste("pos",1:5), 
              id.var="person")
head(cj.re)

m1 <- mlogit(picked ~ 0 + text.chat + video.chat + voice.chat + certified + 
               comments + save + notes + mindful + stream + price, data = cbc.mlogit)
summary(m1)

coef(m1)["text.chat"]/
  (-coef(m1)["price"])


