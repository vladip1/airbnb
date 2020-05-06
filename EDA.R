library(RODBC)
library(DBI)
library(HelpersMG)
library(stringr)
library(dplyr)
library(ggplot2)
library(lattice)
library(mechkar)

library('fastcluster')
library(matrixStats)


#source('C://bb//mechkar/R/mechkar.R')


setwd("C://bb//airbnb")

#con = odbcConnect("airbnb")
#airbnb_ff<-sqlQuery(con, "select * from dbo.FF_V")
#save(airbnb_ff, file="C://bb//insideairbnb//airbnb_ff.RData")

load("C://bb//insideairbnb//airbnb_ff.RData")    

dim(airbnb_ff)

summary(airbnb_ff)

head(airbnb_ff)

str(airbnb_ff)

df<-airbnb_ff
df<-as.data.frame(df)

length(unique(df$listing_id))



########################
#remove technical fields
########################
tech_fileds<-c('calendar_updated_trim', 'period', 'calendar_updated', 'rownames', 'occur', 'listing_id', 'id',
               'last_scraped', 'host_since', 'cal_period', 'square_feet', 'year_num')
df<-df[,setdiff(names(df), tech_fileds)]




######################################
## create a protocol excel
######################################


pr.class<-as.data.frame(sapply(df, class))
pr.type<-as.data.frame(sapply(df, typeof))

prot<-data.frame('name' = names(df),
                 'type' = as.vector(pr.type[[1]]), 
                 'class' = as.vector(pr.class[[1]]))

rownames(prot) <- prot$name
prot$min<-NA
prot$max<-NA
prot$unique<-NA

for (v in rownames(prot)){
  if (prot[v, 'class'] == 'numeric' | prot[v, 'class'] == 'integer'){
    prot[v, 'min']<-min(df[v])
  }
}

for (v in rownames(prot)){
  if (prot[v, 'class'] == 'numeric' | prot[v, 'class'] == 'integer'){
    prot[v, 'max']<-max(df[v])
  }
}

for (v in rownames(prot)){
  if (prot[v, 'class'] == 'numeric' | prot[v, 'class'] == 'integer'){
    prot[v, 'unique']<-unique(df[v])
  }
}

options(scipen = 999)


write.csv(format(prot, digits = 0), 'protocol11.csv')






#run mechkar only on numeric and factor filoeds (where there are not too much levels)
remove_from_eda<-c('summary','space', 'description', 'neighborhood_overview','notes','transit','host_about','amenities')
df<-df[,setdiff(names(df), remove_from_eda)]


#fix zipcode
df$zipcode<-sub("CA ","", df$zipcode)
df$zipcode[df$zipcode == '94158\n94158']<-'94158'
df$zipcode[df$zipcode == '94107-1273']<-'94107'
df$zipcode[df$zipcode == '95202\n\n\n94158']<-'95202'

#turn character into factor
df$zipcode<-factor(df$zipcode)


#change Y value (occupation) accroding to the following rules:
# 0 -> 0
# 1-6 -> 1
# 7 -> 2
df <- df %>% mutate(occupied_cat = factor(ifelse(occupied >= 7, "Full", ifelse(occupied == 0, "None", "Partial"))))
df <- df %>% mutate(occupied_cat.num = ifelse(occupied >= 7, 2, ifelse(occupied == 0, 0, 1)))

####################################
## create a train set and store a test set
####################################

my.seed<-28

train_test(data = df, train_name = 'train', test_name = 'test', prop = 0.80, tableone = TRUE, seed = my.seed)

df<-train



####################################
## run exploration without factors, factorswil be analyzed as part of the EDA as described here https://docs.google.com/presentation/d/1SR2DsiWdyp3yf7goZJqR1ftGmQ2evkqXM97nK-QW3mA/edit#slide=id.p
####################################
df.no.big.factors<-df[,setdiff(names(df), names(Filter(is.factor, df)))]

mm <- mechkar::getMissingness(df.no.big.factors,getRows=T)

#TODO reduce the number of missing entires!!!!!
df <- df.no.big.factors[mm$rows,]

#exploreData(df.no.big.factors, y = 'occupied_cat.num')

data<-df


ggplot(data)+
  geom_density(aes(log(avg_price)))

ggplot(data)+
  geom_density(aes(log(avg_price), group=data$occupied_cat.num, color=data$occupied_cat.num))

# this should be checked in 3-dimensions grapg: avg_price, occurences, num-of-rooms



####################################


histogram(df$occupied_cat.num)

#pairs(df)

#####################################
#choose the right error measurment function (check here https://docs.google.com/presentation/d/1-pnhAeur3uIg-5vnhA9CUKNgjFipBWvCd2h6SFTMvus/edit#slide=id.p)
#####################################

mae <- function(y,yhat) {
  mean(abs(y-yhat),na.rm=T)
}

rmse <- function(y,yhat) {
  sqrt(mean((y-yhat)^2,na.rm=T))
}

options(repr.plot.width = 16 , repr.plot.height = 16)

mod1 <- lm(occupied_cat.num ~., data=df)

summary(mod1)

pred <- predict(mod1)
mae(df$occupied_cat.num, round(pred))
rmse(df$occupied_cat.num, round(pred))

pca<- prcomp(df)
#plot(pca)

#plot(pca$x)

memory.size()


hcl <-fastcluster::hclust(dist(as.matrix(df)))
#plot(hcl)

clusters <- cutree(hcl,3)
table(clusters)

#plot(pca$x, col=clusters)

######################
lapply(sapply(df[c('number_of_reviews', 'review_scores_rating','review_scores_accuracy')], unique), length)
summary(df[c('number_of_reviews', 'review_scores_rating','review_scores_accuracy')])


lapply(sapply(df[c('review_scores_cleanliness','review_scores_checkin','review_scores_communication','review_scores_location','review_scores_value')], unique), length)
       
summary(df[c('review_scores_cleanliness','review_scores_checkin','review_scores_communication','review_scores_location','review_scores_value')])
                   
                   
lapply(sapply(df[c('instant_bookable','cancellation_policy','require_guest_profile_picture','require_guest_phone_verification')], unique), length)

summary(df[c('instant_bookable','cancellation_policy','require_guest_profile_picture','require_guest_phone_verification')])
                   
lapply(sapply(df[c('reviews_per_month', 'host_listings_count','host_seniority')], unique), length)

summary(df[c('reviews_per_month', 'host_listings_count','host_seniority')])

names(df)


#######################games area
names(df)
head(df)

summary(df$accommodates)

lapply(sapply(df[c('security_deposit',                   'cleaning_fee',                   'guests_included',                   'extra_people',                   'minimum_nights',                   'maximum_nights')], unique), length)

length(levels(df$bed_type))

summary (df[c('security_deposit',                   'cleaning_fee',                   'guests_included',                   'extra_people',                   'minimum_nights',                   'maximum_nights')])

summary (df[c('bathrooms','bedrooms','beds')])

###################################################
########################## features engeneerihng
###################################################

load("C://bb//airbnb//data//FF_train_clean.RData")    

data<-df

data<-data[1:100,]
convert_amenities_to_dummy<-function(data) {
  
  print('convert_amenities_to_dummy')
  
  NUM_OF_AMENITIES<-20
  
  data$amenities <- as.character(data$amenities)
  
  data$amenities<-gsub('"', '', data$amenities)
  
  data$amenities<-gsub('\\{', '', data$amenities)
  
  data$amenities<-gsub('\\}', '', data$amenities)
  
  amenities_normilized <- gsub("(?!,)[^[:alnum:]]", "", data$amenities, perl=TRUE)
  
  amenities_normilized.split <- strsplit(amenities_normilized, ",")
  
  most.popluar.amenities<-tail(sort(table(unlist(amenities_normilized.split))), NUM_OF_AMENITIES)
  
  most.popluar.amenities.names<-names(most.popluar.amenities)
  
  m <- matrix(0, ncol = length(most.popluar.amenities.names), nrow = nrow(data))
  
  m <- data.frame(m)
  
  
  most.popluar.amenities.names.col<-paste("amenities_",most.popluar.amenities.names, sep = "")
  names(m)<-most.popluar.amenities.names.col
  
  data<-cbind(data, m)  
  
  
  for (v in 1:length(amenities_normilized)){
    for(j in most.popluar.amenities.names){
      if (length(grep(j, amenities_normilized[v])) != 0){
        data[v,paste("amenities_",j,sep = "")]<-1
      }
    }
    print(v)
  }
  
  return(data)
}


test<-convert_amenities_to_dummy(data[1:10,])

names(test)

paste("hhh", c('sdas', 'sdasdasd'), sep = "_")

dim(data)

review_scores_cleanliness_dummy <- dummy(data$review_scores_cleanliness, sep = "_")

dim(review_scores_cleanliness_dummy)

review_scores_cleanliness_dummy

names(review_scores_cleanliness_dummy)

head(review_scores_cleanliness_dummy)

dim(df)

###hclust generation
#remove string variables
string_features<-c('summary','space', 'description', 'neighborhood_overview','notes','transit','host_about','amenities')
big.factors<-c('neighbourhood_cleansed','zipcode', 'property_type')
no.big.factors<-setdiff(names(df), c(string_features, big.factors, "occupied.cat")) 
hcl <- hclust(dist(as.matrix(df[1:2])))

library(dbscan)

dft<-df[no.big.factors]

# convert all the factors into numberic
indx <- sapply(dft, is.factor)
dft[indx] <- lapply(dft[indx], function(x) as.numeric(x))



dd<-dbscan(dft, eps = 0.15, minPts = 2)


library("fpc")
db <- fpc::dbscan(dft, eps = 0.15, MinPts = 5)




dd


### And convert categorical values into dummy variables:
  
# neighbourhood_cleansed - we will replace non-alphanumeric characters with underscores to prevent code errors
# property_type - we will replace non-alphanumeric characters with underscores to prevent code errors
# room_type - we will replace non-alphanumeric characters with underscores to prevent code errors
# bed_type - we will replace non-alphanumeric characters with underscores to prevent code errors
# cancellation_policy
library(dummies)

data<-df

review_scores_accuracy_dummy <- dummy(data$review_scores_accuracy, sep = "_")
data <- cbind(data, review_scores_accuracy_dummy)



review_scores_cleanliness_dummy <- dummy(data$review_scores_cleanliness, sep = "_")
data <- cbind(data, review_scores_cleanliness_dummy)

review_scores_checkin_dummy <- dummy(data$review_scores_checkin, sep = "_")
data <- cbind(data, review_scores_checkin_dummy)

review_scores_communication_dummy <- dummy(data$review_scores_communication, sep = "_")
data <- cbind(data, review_scores_communication_dummy)

review_scores_location_dummy <- dummy(data$review_scores_location, sep = "_")
data <- cbind(data, review_scores_location_dummy)

review_scores_value_dummy <- dummy(data$review_scores_value, sep = "_")
data <- cbind(data, review_scores_value_dummy)

reviews_per_month_dummy <- dummy(data$reviews_per_month, sep = "_")
data <- cbind(data, reviews_per_month_dummy)

dim(data)

names(data)

convert_categorical_to_dummies<-function(data){

  data$neighbourhood_group_cleansed <- str_replace_all(data$neighbourhood_cleansed, "[^[:alnum:]]", "_")
  
  data$property_type <- str_replace_all(data$property_type, "[^[:alnum:]]", "_")
  
  data$room_type <- str_replace_all(data$room_type, "[^[:alnum:]]", "_")
  
  data$bed_type <- str_replace_all(data$bed_type, "[^[:alnum:]]", "_")
  
  nb_group_dummy <- dummy(data$neighbourhood_cleansed, sep = "_")
  data <- cbind(data, nb_group_dummy)
  
  property_type_dummy <- dummy(data$property_type, sep = "_")
  data <- cbind(data, property_type_dummy)
  
  room_type_dummy <- dummy(data$room_type, sep = "_")
  data <- cbind(data, room_type_dummy)
  
  bed_type_dummy <- dummy(data$bed_type, sep = "_")
  data <- cbind(data, bed_type_dummy)
  
  cancellation_policy_dummy <- dummy(data$cancellation_policy, sep = "_")
  data <- cbind(data, cancellation_policy_dummy)
  
  return(data)
}


### convert amenities into dummy
atest<-df

convert_amenities_to_dummy<-function(data) {

  data$amenities <- as.character(data$amenities)
  
  data$amenities<-gsub('"', '', data$amenities)
  
  data$amenities<-gsub('\\{', '', data$amenities)
  
  data$amenities<-gsub('\\}', '', data$amenities)
  
  amenities_normilized <- gsub("(?!,)[^[:alnum:]]", "", data$amenities, perl=TRUE)
  
  amenities_normilized.split <- strsplit(amenities_normilized, ",")
  
  most.popluar.amenities<-tail(sort(table(unlist(amenities_normilized.split))), 100)
  
  most.popluar.amenities.names<-names(most.popluar.amenities)
  
  m <- matrix(0, ncol = length(most.popluar.amenities.names), nrow = nrow(data))
  
  names(m)<-most.popluar.amenities.names
  
  m <- data.frame(m)
  
  data<-cbind(data, m)
  
  length(test)
  
  for (v in 1:length(amenities_normilized)){
    for(j in most.popluar.amenities.names){
      if (length(grep(j, amenities_normilized[v])) != 0){
        data[v,j]<-1
      }
    }
  }
  
  return(data)
}

### host_about_len = ifelse(is.na(host_about), 0, nchar(host_about)) - in this case NA will be counted as 0

### description_len = ifelse(is.na(description), 0, nchar(as.character(description)))

### summary_len = ifelse(is.na(summary), 0, nchar(as.character(summary)))

### total_amenities = count of listed amenities - since is comma delimited we will count number of separators + 1 where number of characters greater than 2 (listing brackets)

### price_per_person - (avg_price/accommodates)
library(dplyr)
library(stringr)

features_engeneering<-function(data){
  data <- data %>% 
    mutate(host_about_len = ifelse(is.na(host_about), 0, nchar(as.character(host_about))), 
           description_len = ifelse(is.na(description), 0, nchar(as.character(description))), 
           summary_len = ifelse(is.na(summary), 0, nchar(as.character(summary))), 
           total_amenities = ifelse(nchar(as.character(amenities))>2, str_count(as.character(amenities), ',')+1, 0),
           price_per_person = avg_price / accommodates)
  
  data<-convert_amenities_to_dummy(data)
  
  data<-convert_categorical_to_dummies(data)
  
  return(data)
}

