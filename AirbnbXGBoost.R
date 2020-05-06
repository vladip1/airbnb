library(xgboost)
data(iris)

load("C://bb//airbnb//data//FF_train_clean_enriched1.RData")    


# Convert the Species factor to an integer class starting at 0
# This is picky, but it's a requirement for XGBoost
#species = iris$Species
species<-df.dataset1$occupied_cat

label = as.integer(df.dataset1$occupied_cat)-1

#iris$Species = NULL
df.dataset1$occupied_cat<-NULL

n = nrow(df.dataset1)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(df.dataset1[train.index,])
train.label = label[train.index]
test.data = as.matrix(df.dataset1[-train.index,])
test.label = label[-train.index]


# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

# Define the parameters for multinomial classification
num_class = length(levels(species))
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
  nrounds=1000,
  nthreads=8,
  early_stopping_rounds=10,
#  watchlist=list(val1=xgb.train,val2=xgb.test),
  watchlist=list(val1=xgb.train),
  verbose=1
)

# Review the final model and results
xgb.fit



# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(species)


# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(species)[test.label+1]

# Calculate the final accuracy
result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))



