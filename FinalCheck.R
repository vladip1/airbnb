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



set.seed(12345)

mod <- randomForest(occupied_cat ~., data=train, mtry = 46, ntree=20000, do.trace=TRUE)
mod

############ predict on Dev ###############
pred_dev <- predict(mod,newdata=test[2:ncol(test)], type = "prob")

err_res <- rbind(err_res, data.frame(Name="RandomForest", Target="dev", 
                                     MultiLogLoss=MultiLogLoss(pred_dev, test$occupied_cat),
                                     Accuracy=accuracyFunc(as.data.frame(pred_dev), test$occupied_cat)))
############################################


############ predict on Production ###############
pred_production <- predict(mod,newdata=production[2:ncol(production)], type = "prob")

err_res <- rbind(err_res, data.frame(Name="RandomForest", Target="production", 
                                     MultiLogLoss=MultiLogLoss(pred_production, production$occupied_cat),
                                     Accuracy=accuracyFunc(as.data.frame(pred_production), production$occupied_cat)))
############################################

err_res
