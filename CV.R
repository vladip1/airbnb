#################################################################################
######################################### Fine - tuning #########################
#################################################################################

if(.Platform$OS.type == "unix") {
  setwd("/bb/airbnb")
}else {
  setwd("C://bb//airbnb")
}

source('LoadDataframes.R')

library(randomForest)
library(doParallel)
library(mlbench)
library(caret)


cls = makeCluster(8) 
registerDoParallel(cls)


# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random", verboseIter = TRUE, allowParallel = TRUE)
set.seed(123)
metric <- "Accuracy"
mtry <- sqrt(ncol(train))
ptm3 <- proc.time()

rf_random <- train(occupied_cat~., data=train, method="rf", metric=metric, tuneLength=15, trControl=control)

ptm4 <- proc.time()

print(ptm4 - ptm3)
print(rf_random)

