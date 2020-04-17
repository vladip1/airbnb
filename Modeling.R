#load("C://bb//airbnb//data//FF_train_clean.RData")

load("C://bb//airbnb//data//FF_train_clean_enriched1.RData")    


train<-df.dataset1

class(train$occupied_cat)

ll<-function (y, p, likelihood = FALSE, global = TRUE, eps = 1e-15) 
{
  if (length(p) != length(y)) 
    stop("Error: y and p are not the same length")
  if (min(p) < 0 | max(p) > 1) 
    stop("p is out of {0-1} probability range")
  if (!is.numeric(y)) 
    y <- as.numeric(as.character(y))
  if (min(y) < 0 | max(y) > 1) 
    stop("y is not binomial {0,1}")
  loglike.loss <- function(y, x) {
    if (class(y) == "factor" | class(y) == "character") 
      y <- as.numeric(as.character(y))
    x <- 1/(1 + exp(-x))
    grad <- x - y
    hess <- x * (y - x)
    return(hess)
  }
  if (likelihood == FALSE) {
    if (global == TRUE) {
      p <- matrix(sapply(p, function(x) max(eps, x)), nrow = length(p))
      p <- matrix(sapply(p, function(x) min(1 - eps, x)), 
                  nrow = length(p))
      ll <- sum(y * log(p) + (1 - y) * log(1 - p))
      ll <- ll * -1/length(y)
      class(ll) <- "global.log.loss"
    }
    else {
      ll <- vector()
      for (i in 1:length(p)) {
        ll <- append(ll, pmin(pmax(p[i], eps), 1 - eps) - 
                       (sum(y[i] * log(p[i]) + (1 - y[i]) * log(1 - 
                                                                  p[i])))/length(y[i]))
      }
      ll[is.nan(ll)] <- 0
      ll <- list(y = y, p = p, log.loss = ll)
      class(ll) <- "local.log.loss"
    }
  }
  else {
    ll <- loglike.loss(y, p)
  }
  return(ll)
}

err_res<-NULL

trainFunc<-function(data, err_res){

  #######################  Desicion trees ###################################################################
  
  library(tree)
  library(rpart)
  #install.packages('MLmetrics')
  library(MLmetrics)
  
  mod3 <- tree(occupied_cat ~., data=train, method = "class")
  mod3
  
  #pred3 <- predict(mod3,newdata=test)
  
  test<-train
  test$occupied_cat<-NULL
  pred3 <- predict(mod3,newdata=train)
  
  
  MultiLogLoss(pred3, train$occupied_cat)
  
  
  err_res <- rbind(err_res, data.frame(Name="Decision Trees-tree", Model="mod3", 
                                       MultiLogLoss=MultiLogLoss(pred3, train$occupied_cat)))
  
  mod4 <- rpart(occupied_cat ~., data=train)
  mod4
  
  pred4 <- predict(mod4,newdata=test)
  err_res <- rbind(err_res, data.frame(Name="Decision Trees-rpart", Model="mod4", 
                                       MultiLogLoss=MultiLogLoss(pred4, train$occupied_cat)))
  err_res
  
  #######################  Random Forest ###################################################################
  library(randomForest)
  library(ranger)
  
  mod5 <- randomForest(occupied_cat ~., data=train)
  mod5
  
  pred5 <- predict(mod5,newdata=test)
  
  
  
  err_res <- rbind(err_res, data.frame(Name="RandomForest", Model="mod5", 
                                       MultiLogLoss=MultiLogLoss(pred5, train$occupied_cat)))  
  
  mod6 <- ranger(occupied_cat ~., data=train)
  mod6
  
  pred6 <- predict(mod6,data=test)
  
  err_res <- rbind(err_res, data.frame(Name="RandomForest (ranger)", Model="mod6", 
                                       MultiLogLoss=MultiLogLoss(pred5, train$occupied_cat)))    
  
  
  #######################  XGBoost ###################################################################
  #install.packages("xgboost")
  library(xgboost)
  
  train1 <- Matrix::sparse.model.matrix(occupied_cat ~ .-1, data = train)
  test1 <- Matrix::sparse.model.matrix(occupied_cat ~ .-1, data = test)
  
  X_train <- train1
  y_train <- train$occupied_cat
  mod7 <- xgboost(data=X_train,label=y_train, nrounds=50,print_every_n = 10)
  
  X_test <- test1
  y_test <- test$cnt
  
  pred7 <- predict(mod7,newdata=X_test)
  rmse(y_test,pred7)
  rmsle(y_test,pred7)
  err_res <- rbind(err_res, data.frame(Name="XGBoost", Model="mod7", 
                                       RMSE=rmse(test$cnt,pred7), 
                                       RMSLE=rmsle(test$cnt,pred7)))
  
  #######################  kNN ###################################################################
  
  ### adaboost needs that values to be normalized
  min_max <- function(x) { (x -min(x))/(max(x)-min(x))   }
  
  #  X_train <- sapply(data.frame(as.matrix(train1)),min_max)
  #  X_test <- sapply(data.frame(as.matrix(test1)),min_max)
  
  #  summary(X_train)
  
    library(class)
    mod8 <- knn(train,test,cl=train$occupied_cat, prob = TRUE)
  
  #  str(mod8)
  
  #  pred8 <- as.numeric(as.character(mod8)
  
  #  rmse(test$cnt,pred8)
  #  rmsle(test$cnt,pred8)
  err_res <- rbind(err_res, data.frame(Name="kNN", Model="mod8", 
                                       MultiLogLoss=MultiLogLoss(pred8, train$occupied_cat)))  
  #######################  SVM ###################################################################
  
  #install.packages("liquidSVM")
  library(liquidSVM)
  
  mod9 <- svm(occupied_cat ~., train, , probability = TRUE)
  
  mod9$last_result
  
  #test(mod9)
  
  pred9 <- predict(mod9, newdata=test)
  
  rmse(test$cnt,pred9)
  print(rmsle(test$cnt,pred9))
  print(my.seed)
  
  err_res <- rbind(err_res, data.frame(Name="SVM", Model="mod9", , 
                                       MultiLogLoss=MultiLogLoss(pred9, train$occupied_cat)))  
  #plot(test$cnt, pred9, col=c('blue', 'red'))
  
  
  ########################################################################
  
  
  library(adabag)
  
  model = boosting(occupied_cat~., data=train, boos=TRUE, mfinal=50)
  print(names(model))
  print(model$trees[1])
  
  pred10 = predict(model, test)
  print(pred10$confusion)
  print(pred10$error)
  
  result = data.frame(test$occupied_cat, pred$prob, pred$class)
  print(result)
  
  err_res <- rbind(err_res, data.frame(Name="Adaboost", Model="mod10", , 
                                       MultiLogLoss=MultiLogLoss(pred10$prob, train$occupied_cat)))  
  
  ########################################################################
  
  dim(test)
  
  
  
  plot(1:nrow(test), test$cnt)
  points(1:nrow(test), pred2, col="red", pch=16)
  points(1:nrow(test), pred9, col="green", pch=16)
  points(1:nrow(test), pred7, col="blue", pch=16)
  
  plot(1:nrow(test), test$cnt)
  points(1:nrow(test), pred3, col="green", pch=16)
  points(1:nrow(test), pred4, col="red", pch=16)
  points(1:nrow(test), pred5, col="green", pch=16)
  
  plot(1:nrow(test), test$cnt)
  points(1:nrow(test), pred6$predictions, col="green", pch=16)
  
  
  
  err_res
  
}


############################################################
1. load both train data and both test data
2. train for each train data
3. test on dev.capabilities

string_features<-c('summary','space', 'description', 'neighborhood_overview','notes','transit','host_about','amenities')
big.factors<-c('neighbourhood_cleansed','zipcode', 'property_type')

features_to_leave<-setdiff(names(df), c(string_features, big.factors, "X", "listing_id"))


df<-df[ , features_to_leave]

#
#dataset with categories as dummies
df1<-df

#dataset with categories as numerics
df2<-df

df.cat.as.num<-df

train<-df[ , !(names(df) %in% drops)]
test<-df[ , !(names(df) %in% drops)]


str(train)



##########################################################################################
# Upload the real test
##########################################################################################


pred.res<-NULL
pred.res$id<-tdf4$id

tdf.pred<-tdf4[ , !(names(tdf4) %in% drops)]

pred.res$cnt<-round(predict(mod9, newdata=tdf.pred))

drops

file_name = sprintf('result_SVM_no_atemp_cluster_%f_%f.csv',  rmsle(test$cnt,pred9), my.seed)
file_name
write.csv(file = file_name, pred.res, row.names = FALSE)

dim(df4)
#------------------------------------------------------------------------------------------
pred.res<-NULL
pred.res$id<-tdf4$id

tdf.pred<-tdf4[ , !(names(tdf4) %in% drops)]

pred.res$cnt<-round(predict(mod2, newdata=tdf.pred))

write.csv(file = "result_LinearExtended_213_27.csv", pred.res, row.names = FALSE)

dim(df4)
#------------------------------------------------------------------------------------------
pred.res<-NULL
pred.res$id<-tdf4$id

tdf.pred<-tdf4[ , !(names(tdf4) %in% drops)]

pred.res$cnt<-round(predict(mod3, newdata=tdf.pred))

write.csv(file = "result_Tree_253_27.csv", pred.res, row.names = FALSE)

dim(df4)
#------------------------------------------------------------------------------------------
pred.res<-NULL
pred.res$id<-tdf4$id

tdf.pred<-tdf4[ , !(names(tdf4) %in% drops)]

pred.res$cnt<-round(predict(mod4, newdata=tdf.pred))

write.csv(file = "result_Rpart_255_27.csv", pred.res, row.names = FALSE)

dim(df4)
#------------------------------------------------------------------------------------------


dim(tdf4)

pred.res<-NULL
pred.res$id<-tdf4$id
tdf.pred<-tdf4[ , !(names(tdf4) %in% drops)]

pp<-predict(mod6, data=tdf.pred)
pred.res$cnt<-round(pp$predictions)


summary(test)
summary(tdf.pred)




write.csv(file = "result_Ranger_all_additions_201_27.csv", pred.res, row.names = FALSE)

#------------------------------------------------------------------------------------------

tdf4<-tdf

pred.res<-NULL
pred.res$id<-tdf4$id
tdf.pred<-tdf4[ , !(names(tdf4) %in% drops)]

dim(tdf.pred)

pred.res$cnt<-round(predict(mod5, newdata=tdf.pred))


write.csv(file = "result_RF__hcluster6_cluster6_184_30_99.csv", pred.res, row.names = FALSE)

#------------------------------------------------------------------------------------------

tdf4<-tdf

pred.res<-NULL
pred.res$id<-tdf4$id
tdf.pred<-tdf4[ , !(names(tdf4) %in% drops)]

pred.res$cnt<-round(predict(mod7, newdata=as.matrix(tdf.pred)))

write.csv(file = "result_XBoost_weathersit__hcluster_cluster_153_27.csv", pred.res, row.names = FALSE)