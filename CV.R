#################################################################################
######################################### Fine - tuning #########################
#################################################################################

library(randomForest)
library(doParallel)
library(mlbench)
library(caret)




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
  setwd("/bb/airbnb")
}else {
  setwd("C://bb//airbnb")
}


if(.Platform$OS.type == "unix") {
  filename <- "data/FF_train_clean_enriched2.csv"
  df = read.table(filename, header=T, quote="\"", sep=",")
  train<-df[c('occupied_cat', dataset2Variables)]
}else {
  load(paste(path,"FF_train_clean_enriched2.RData", sep = ''))
  train<-df.dataset2[c('occupied_cat', dataset2Variables)]
}
if(.Platform$OS.type == "unix") {
  filename <- "data/FF_dev_clean_enriched2.csv"
  df = read.table(filename, header=T, quote="\"", sep=",")
  test<-df[c('occupied_cat', dataset2Variables)]
}else {
  load(paste(path,"FF_dev_clean_enriched2.RData", sep = ''))  
  test<-df.dataset2[c('occupied_cat', dataset2Variables)]
}

if(.Platform$OS.type == "unix") {
  filename <- "data/FF_test_clean_enriched2.csv"
  df <- read.table(filename, header=T, quote="\"", sep=",")
  production<-df[c('occupied_cat', dataset2Variables)]
}else {
  load(paste(path,"FF_test_clean_enriched2.RData", sep = ''))  
  production<-df.dataset2[c('occupied_cat', dataset2Variables)]
}


cls = makeCluster(8) 
registerDoParallel(cls)


#train<-train[1:100,]

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



#mod <- randomForest(occupied_cat ~., data=train, mtry = 59, ntree=2000, do.trace=TRUE)
#mod
