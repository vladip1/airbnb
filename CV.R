#################################################################################
######################################### Fine - tuning #########################
#################################################################################

library(randomForest)
library(doParallel)
library(mlbench)
library(caret)

if(.Platform$OS.type == "unix") {
  setwd("/bb/airbnb")
}else {
  setwd("C://bb//airbnb")
}



source('./Modeling.R')


cls = makeCluster(4) 
registerDoParallel(cls)




#train<-train[1:10000,]

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
plot(rf_random)



mod <- randomForest(occupied_cat ~., data=train, mtry = 59, ntree=2000, do.trace=TRUE)
mod
