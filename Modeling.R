if(.Platform$OS.type == "unix") {
  setwd("/bb/airbnb")
}else {
  setwd("C://bb//airbnb")
}

source('LoadDataframes.R')


library(tree)
library(rattle)

library(rpart)
library(dplyr)
library(caret)
library(rpart.plot)
library(rpart)
library(randomForest)
library(ranger)
library(xgboost)
library(liquidSVM)
library(e1071)
library(adabag)
library(gbm)
library(class)


dim(train)
dim(production)
dim(test)

occupied_cat<-train$occupied_cat

err_res<-NULL


#######################  Desicion trees ###################################################################
  
desicionTreeFunc<-function(train, test, err_res){
  
  mod3 <- tree(occupied_cat ~., data=train, method = "class")
  mod3
  
  pred3 <- predict(mod3,newdata=test[2:ncol(test)])
  
  
  err_res <- rbind(err_res, data.frame(Name="Decision Trees-tree", Model="mod3", 
                                       MultiLogLoss=MultiLogLoss(pred3, test$occupied_cat),
                                       Accuracy=accuracyFunc(as.data.frame(pred3), test$occupied_cat)))
  return(err_res)
}
  
#######################  Desicion trees rpart ###################################################################

desicionTreeRpartFunc<-function(train, test, err_res){

  mod4 <- rpart(occupied_cat ~., data=train, method = "class")
  mod4
  
  pred4 <- predict(mod4,newdata=test[2:ncol(test)])
  
  
  err_res <- rbind(err_res, data.frame(Name="Decision Trees-rpart", Model="mod4", 
                                       MultiLogLoss=MultiLogLoss(pred4, test$occupied_cat),
                                       Accuracy=accuracyFunc(as.data.frame(pred4), test$occupied_cat)))
  return(err_res)
}  

#######################  Random Forest ###################################################################

randomForestFunc<-function(train, test, err_res){
  set.seed(12345)
  
  mod5 <- randomForest(occupied_cat ~., data=train, ntree=2000, do.trace=TRUE)
  mod5
  
  pred5 <- predict(mod5,newdata=test[2:ncol(test)], type = "prob")
  
  
  err_res <- rbind(err_res, data.frame(Name="RandomForest", Model="mod5", 
                                       MultiLogLoss=MultiLogLoss(pred5, test$occupied_cat),
                                       Accuracy=accuracyFunc(as.data.frame(pred5), test$occupied_cat)))
  return(err_res)
}

#######################  Random Forest Ranger

randomForestRangerFunc<-function(train, test, err_res){
  
  mod6 <- ranger(occupied_cat ~., data=train, num.trees = 2000, verbose = TRUE, probability = TRUE)
  mod6
  
  pred6 <- predict(mod6,data=test[2:ncol(test)])
  
  pred6$predictions
  
  err_res <- rbind(err_res, data.frame(Name="RandomForest (ranger)", Model="mod6", 
                                       MultiLogLoss=MultiLogLoss(as.matrix(pred6$predictions), test$occupied_cat),
                                       Accuracy=accuracyFunc(as.data.frame(pred6$predictions), test$occupied_cat)))
  return(err_res)
}
  
#######################  XGBoost ###################################################################

xgBoostFunc<-function(train, test, err_res){
  

  # Convert the Species factor to an integer class starting at 0
  # This is picky, but it's a requirement for XGBoost
  #species = iris$Species
  
  #species<-df.dataset1$occupied_cat
  
  #label = as.integer(df.dataset1$occupied_cat)-1
  
  #iris$Species = NULL
  #df.dataset1$occupied_cat<-NULL
  
  train.data = as.matrix(train[2:ncol(train)])
  train.label = as.integer(train$occupied_cat)-1
  
  test.data = as.matrix(test[2:ncol(test)])
  test.label = as.integer(test$occupied_cat)-1
  
  
  # Transform the two data sets into xgb.Matrix
  xgb.train = xgb.DMatrix(data=train.data,label=train.label)
  xgb.test = xgb.DMatrix(data=test.data,label=test.label)
  
  # Define the parameters for multinomial classification
  num_class = length(levels(train$occupied_cat))
  params = list(
    booster="gbtree",
    eta=0.001,
    max_depth=5,
    gamma=3,
    subsample=0.75,
    colsample_bytree=1,
    objective="multi:softprob",
    eval_metric="mlogloss",
    num_class=num_class
  )
  
  # Train the XGBoost classifer
  xgb.fit=xgb.train(
    params=params,
    data=xgb.train,
    nrounds=2000,
    nthreads=8,
    early_stopping_rounds=10,
    watchlist=list(val1=xgb.train,val2=xgb.test),
    #watchlist=list(val1=xgb.train),
    verbose=1
  )
  
  # Review the final model and results
  xgb.fit
  
  
  
  # Predict outcomes with the test data
  xgb.pred = predict(xgb.fit,test.data,reshape=T)
  xgb.pred = as.data.frame(xgb.pred)
  colnames(xgb.pred) = levels(train$occupied_cat)
  
  err_res <- rbind(err_res, data.frame(Name="XGBoost", Model="mod7", 
                                       MultiLogLoss=MultiLogLoss(xgb.pred, test$occupied_cat),
                                       Accuracy=accuracyFunc(xgb.pred, test$occupied_cat)))
  

  return(err_res)
}

#######################  kNN ###################################################################

kNNFunc<-function(train, test, err_res){
  ### adaboost needs that values to be normalized
  min_max <- function(x) { (x -min(x))/(max(x)-min(x))   }
  
  train_norm <- sapply(data.frame(as.matrix(train[,2:ncol(train)])),min_max)
  test_norm <- sapply(data.frame(as.matrix(test[,2:ncol(test)])),min_max)
  
  #  summary(X_train)
  
  nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
  
  ##Run nomalization on first 4 coulumns of dataset because they are the predictors
  #train_norm <- as.data.frame(lapply(train[,2:ncol(train)], nor))
  
  #test_norm <- as.data.frame(lapply(test[,2:ncol(test)], nor))
  
  summary(train_norm)
  dim(train_norm)
  dim(test_norm)
  
  occupation_target_category<-train$occupied_cat
    
  mod8 <- class:knn(train_norm, test_norm, cl=occupation_target_category, k=3)
  
  #  str(mod8)
  
  #  pred8 <- as.numeric(as.character(mod8)
  
  #  rmse(test$cnt,pred8)
  #  rmsle(test$cnt,pred8)
  err_res <- rbind(err_res, data.frame(Name="kNN", Model="mod8", 
                                       MultiLogLoss=MultiLogLoss(pred8, test$occupied_cat),
                                       Accuracy=accuracyFunc(pred8, test$occupied_cat)))
  return(err_res)
}

#######################  SVM ###################################################################

svmFunc<-function(train, test, err_res){
  
  mod9 <- liquidSVM::svm(occupied_cat ~., train, predict.prob = TRUE)
  
  mod9$last_result
  
  mod9$predict.prob
  
  
  #test(mod9)
  
  pred9 <- predict(mod9, newdata=test[,2:ncol(test)])
  
  names(pred9)<-levels(test$occupied_cat)
  
  
  
  
  #rmse(test$cnt,pred9)
  #print(rmsle(test$cnt,pred9))
  #print(my.seed)
  
  err_res <- rbind(err_res, data.frame(Name="SVM", Model="mod9",
                                       MultiLogLoss=MultiLogLoss(as.matrix(pred9), test$occupied_cat),
                                       Accuracy=accuracyFunc(pred9, test$occupied_cat)))
  
  
  return(err_res)  
}

##########################AdaBoost##############################################
  

adabagFunc<-function(train, test, err_res){
  # set a unique seed number so you get the same results everytime you run the below model,
  # the number does not matter
  set.seed(14)
  # Run the standard version of the bagging model.
  
  
  model = adabag::boosting(occupied_cat~., data=train, boos=TRUE, mfinal=10)
  print(names(model))
  print(model$trees[1])
  
  pred10 = predict(model, test[,2:ncol(test)])
  
  #print(pred10$confusion)
  #print(pred10$error)
  
  #result = data.frame(test$occupied_cat, pred$prob, pred$class)
  #print(result)
  result<-as.data.frame(pred10$prob)
  names(result)<-levels(occupied_cat)
  
  err_res <- rbind(err_res, data.frame(Name="Adaboost", Model="mod10",
                                       MultiLogLoss=MultiLogLoss(pred10$prob, test$occupied_cat),
                                       Accuracy=accuracyFunc(result, test$occupied_cat)))
  return(err_res)  
}

####################################### gbm #############################################

gbmFunc<-function(train, test, err_res){  

  # set a unique seed number so you get the same results everytime you run the below model,
  # the number does not matter
  set.seed(17)
  ptm5 <- proc.time()
  
  #fit5 <- gbm(target ~ ., data=strain, distribution="multinomial", n.trees=2000, cv.folds=2)
  mod11 <- gbm(occupied_cat ~ ., data=train, distribution="multinomial", n.trees=20000, cv.folds=2, verbose = TRUE)
  
  mod.time <- proc.time() - ptm5
  
  mod.time
  
  trees <- gbm.perf(mod11)
  
  # Test the gbm model on the holdout test dataset
  pred11 <- predict(mod11, test[2:ncol(test)], n.trees=trees, type="response")
  pred11 <- as.data.frame(pred11)
  
  names(pred11) <- levels(train$occupied_cat)
  
  
  err_res <- rbind(err_res, data.frame(Name="gbm", Model="mod11", 
                                       MultiLogLoss=MultiLogLoss(pred11, test$occupied_cat),
                                       Accuracy=accuracyFunc(as.data.frame(pred11), test$occupied_cat)))
  
  
  return(err_res)
}
  
########################################################################
  
  

err_res<-desicionTreeFunc(train, test, err_res)
err_res

dim(train)
dim(test)
err_res<-desicionTreeRpartFunc(train, test, err_res)
err_res

err_res<-randomForestFunc(train, test, err_res)
err_res

err_res<-randomForestRangerFunc(train, test, err_res)
err_res

err_res<-xgBoostFunc(train, test, err_res)
err_res

err_res<-kNNFunc(train, test, err_res)
err_res

err_res<-svmFunc(train, test, err_res)
err_res

err_res<-adabagFunc(train, test, err_res)
err_res

err_res<-gbmFunc(train, test, err_res)
err_res                  

