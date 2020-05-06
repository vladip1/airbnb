library(tree)
library(rattle)
library(MLmetrics)

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

dataset1Variables<-c("week_num_1","week_num_10","week_num_11","week_num_12","week_num_13","week_num_14","week_num_15","week_num_16","week_num_17","week_num_18","week_num_19",
                     "week_num_2","week_num_20","week_num_21","week_num_22","week_num_23","week_num_24","week_num_25","week_num_26","week_num_27","week_num_28","week_num_29",
                     "week_num_3","week_num_30","week_num_31","week_num_32","week_num_33","week_num_34","week_num_35","week_num_36","week_num_37","week_num_38","week_num_39",
                     "week_num_4","week_num_40","week_num_41","week_num_42","week_num_43","week_num_44","week_num_45","week_num_46","week_num_47","week_num_48","week_num_49",
                     "week_num_5", "week_num_50", "week_num_51", "week_num_52", "week_num_53", "week_num_6", "week_num_7", "week_num_8", "week_num_9", 'avg_price', 'min_price', 'max_price', 'host_is_superhost_f',
                     'host_is_superhost_t', 'host_identity_verified_f',
                     'host_identity_verified_t', 'latitude', 'longitude',
                     'property_type_Boutique_hotel', 'property_type_Guest_suite',
                     'property_type_Hotel', 'property_type_Serviced_apartment',
                     'room_type_Entire_home_apt', 'accommodates', 'bedrooms_One',
                     'security_deposit_None', 'security_deposit_S', 'extra_people_None',
                     'minimum_nights_Day', 'minimum_nights_Week',
                     'minimum_nights_Month', 'maximum_nights_Few',
                     'maximum_nights_up3M', 'maximum_nights_up3Y',
                     'number_of_reviews_None', 'number_of_reviews_S',
                     'number_of_reviews_L', 'review_scores_accuracy_Perfect',
                     'review_scores_cleanliness_Perfect',
                     'review_scores_location_Perfect', 'review_scores_value_Good',
                     'review_scores_value_Perfect', 'instant_bookable_f',
                     'instant_bookable_t', 'cancellation_policy_flexible',
                     'cancellation_policy_moderate', 'cancellation_policy_strict',
                     'cancellation_policy_strict_14_with_grace_period',
                     'calculated_host_listings_count_Single',
                     'calculated_host_listings_count_upFive',
                     'calculated_host_listings_count_upFifty',
                     'calculated_host_listings_count_upTwoHundred',
                     'reviews_per_month_1', 'reviews_per_month_2', 'host_seniority',
                     'host_about_len', 'description_len', 'summary_len',
                     'total_amenities', 'price_per_person',
                     'amenities_Freestreetparking', 'amenities_Familykidfriendly',
                     'amenities_Internet', 'amenities_Firstaidkit',
                     'amenities_Laptopfriendlyworkspace', 'amenities_Fireextinguisher',
                     'amenities_Iron', 'amenities_Hairdryer', 'amenities_Washer',
                     'amenities_Wifi', 'amenities_Carbonmonoxidedetector',
                     'amenities_Hangers', 'amenities_Smokedetector')

dataset2Variables<-c("week_num_1","week_num_10","week_num_11","week_num_12","week_num_13","week_num_14","week_num_15","week_num_16","week_num_17","week_num_18","week_num_19",
                     "week_num_2","week_num_20","week_num_21","week_num_22","week_num_23","week_num_24","week_num_25","week_num_26","week_num_27","week_num_28","week_num_29",
                     "week_num_3","week_num_30","week_num_31","week_num_32","week_num_33","week_num_34","week_num_35","week_num_36","week_num_37","week_num_38","week_num_39",
                     "week_num_4","week_num_40","week_num_41","week_num_42","week_num_43","week_num_44","week_num_45","week_num_46","week_num_47","week_num_48","week_num_49",
                     "week_num_5", "week_num_50", "week_num_51", "week_num_52", "week_num_53", "week_num_6", "week_num_7", "week_num_8", "week_num_9", 'avg_price', 'min_price',
                     'max_price', 'host_is_superhost_f', 'host_is_superhost_t',
                     'host_identity_verified_f', 'host_identity_verified_t', 'latitude',
                     'longitude', 'property_type_Boutique_hotel', 'property_type_Hotel',
                     'property_type_Serviced_apartment', 'room_type_Entire_home_apt',
                     'accommodates', 'bedrooms', 'beds', 'security_deposit',
                     'cleaning_fee', 'guests_included', 'extra_people',
                     'minimum_nights', 'maximum_nights', 'number_of_reviews',
                     'review_scores_value_Perfect', 'instant_bookable_f',
                     'instant_bookable_t', 'cancellation_policy',
                     'calculated_host_listings_count', 'reviews_per_month_1',
                     'host_seniority', 'accommodates.norm', 'host_about_len',
                     'description_len', 'summary_len', 'total_amenities',
                     'price_per_person', 'amenities_Freestreetparking',
                     'amenities_Familykidfriendly', 'amenities_Internet',
                     'amenities_Firstaidkit', 'amenities_Laptopfriendlyworkspace',
                     'amenities_Fireextinguisher', 'amenities_Iron',
                     'amenities_Hairdryer', 'amenities_Wifi', 'amenities_Hangers',
                     'amenities_Smokedetector')


if(.Platform$OS.type == "unix") {
  path<-NULL
} else {
  path<-'C://bb//airbnb//data//'
}

if(.Platform$OS.type == "unix") {
  filename <- "/airbnb/FF_train_clean_enriched2.csv"
  df = read.table(filename, header=T, quote="\"", sep=",")
  train<-df[c('occupied_cat', dataset2Variables_small)]
}else {
  load(paste(path,"FF_train_clean_enriched2.RData", sep = ''))
  train<-df.dataset2[c('occupied_cat', dataset2Variables_small)]
}
if(.Platform$OS.type == "unix") {
  filename <- "/airbnb/FF_dev_clean_enriched2.csv"
  df = read.table(filename, header=T, quote="\"", sep=",")
  test<-df[c('occupied_cat', dataset2Variables_small)]
}else {
  load(paste(path,"FF_dev_clean_enriched2.RData", sep = ''))  
  test<-df.dataset2[c('occupied_cat', dataset2Variables_small)]
}

if(.Platform$OS.type == "unix") {
  filename <- "/airbnb/FF_test_clean_enriched2.csv"
  df <- read.table(filename, header=T, quote="\"", sep=",")
  production<-df[c('occupied_cat', dataset2Variables_small)]
}else {
  load(paste(path,"FF_test_clean_enriched2.RData", sep = ''))  
  production<-df.dataset2[c('occupied_cat', dataset2Variables_small)]
}

dim(train)
dim(production)
dim(test)

occupied_cat<-train$occupied_cat

err_res<-NULL

accuracyFunc<-function(prediction.prob, test.label, convert=FALSE){
  prediction.prob$prediction = apply(prediction.prob,1,function(x) colnames(prediction.prob)[which.max(x)])
  if (convert==TRUE){
    prediction.prob$label = levels(occupied_cat)[test.label+1]
  } else {
    prediction.prob$label = test.label
  }
  
  # Calculate the final accuracy
  result = sum(prediction.prob$prediction==prediction.prob$label)/nrow(prediction.prob)
  #print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))
  return(result*100)
}

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

