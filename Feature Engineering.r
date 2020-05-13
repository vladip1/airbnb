####################
# Features engeneering
###################

library(dummies)
library(dplyr)
library(stringr)


######################
#Set variables according to the dataset (as described in protocol excel(Transformations))
#####################
remove<-c('X','listing_id','summary','space','description','neighborhood_overview','notes','transit','host_about','amenities')
dummy_common <-c('week_num','host_is_superhost','host_identity_verified','neighbourhood_cleansed','zipcode','is_location_exact','property_type','room_type','bed_type','review_scores_accuracy','review_scores_cleanliness','review_scores_checkin','review_scores_communication','review_scores_location','review_scores_value','instant_bookable','require_guest_profile_picture','require_guest_phone_verification','reviews_per_month')
dummy_dataset1 <-c('bathrooms','bedrooms','beds','security_deposit','cleaning_fee','guests_included','extra_people','minimum_nights','maximum_nights','number_of_reviews','cancellation_policy','calculated_host_listings_count','accommodates.norm')
most.popular.amenities<-c("Freestreetparking","Familykidfriendly","CableTV","Internet","Firstaidkit","Laptopfriendlyworkspace","Fireextinguisher","Iron","Hairdryer","Dryer","Washer","Wifi","Carbonmonoxidedetector","Hangers","TV","Shampoo","Kitchen","Smokedetector","Essentials","Heating")

convert_common_categoricals_to_dummies<-function(data){
    
    print('convert_common_categoricals_to_dummies')
    
    nm<-dummy_common
  
    data[nm] <- lapply(data[nm], function(x) str_replace_all(x, "[^[:alnum:]]", "_"))
      
    data<-dummy.data.frame(data, names = nm, sep = '_')

    return(data)
}


convert_dataset1_categoricals_to_dummies<-function(data){
  
  print('convert_dataset1_categoricals_to_dummies')
  

  nm<-dummy_dataset1

  data<-dummy.data.frame(data, names = nm, sep = '_')
  
  return(data)
}


convert_dataset2_categoricals_to_numeric<-function(data){
  
  print('convert_dataset2_categoricals_to_numeric')
  
  
  nm<-dummy_dataset1
  
  
  data[nm] <- lapply(data[nm], function(x) as.numeric(x))
  
  
  return(data)
}


convert_amenities_to_dummy<-function(data, popluar.amenities) {
  
  print('convert_amenities_to_dummy')
  
  NUM_OF_AMENITIES<-20
  
  data$amenities <- as.character(data$amenities)
  
  data$amenities<-gsub('"', '', data$amenities)
  
  data$amenities<-gsub('\\{', '', data$amenities)
  
  data$amenities<-gsub('\\}', '', data$amenities)
  
  amenities_normilized <- gsub("(?!,)[^[:alnum:]]", "", data$amenities, perl=TRUE)
  
  #amenities_normilized.split <- strsplit(amenities_normilized, ",")
  
  #most.popluar.amenities<-tail(sort(table(unlist(amenities_normilized.split))), NUM_OF_AMENITIES)
  
  #most.popluar.amenities.names<-names(most.popluar.amenities)
  
  m <- matrix(0, ncol = length(popluar.amenities), nrow = nrow(data))
  
  m <- data.frame(m)
  
  
  most.popluar.amenities.names.col<-paste("amenities_",popluar.amenities, sep = "")
  names(m)<-most.popluar.amenities.names.col
  
  data<-cbind(data, m)  
  
  
  for (v in 1:length(amenities_normilized)){
    for(j in popluar.amenities){
      if (length(grep(j, amenities_normilized[v])) != 0){
        data[v,paste("amenities_",j,sep = "")]<-1
      }
    }
      
    if(v %% 1000==0) {
      # Print on the screen some message
     cat(paste0("iteration: ", v, "\n"))
    }
  }
  
  return(data)
}



add_new_variables<-function(data){
    
  print('add_new_variables')
    
  data <- data %>% 
      mutate(host_about_len = ifelse(is.na(host_about), 0, nchar(as.character(host_about))), 
         description_len = ifelse(is.na(description), 0, nchar(as.character(description))), 
         summary_len = ifelse(is.na(summary), 0, nchar(as.character(summary))), 
         total_amenities = ifelse(nchar(as.character(amenities))>2, str_count(as.character(amenities), ',')+1, 0),
         price_per_person = avg_price / accommodates)
  
  return(data)
}

remove_columns_dataset1<-function(data){
  
  remain<-setdiff(names(data), c(remove, dummy_common, dummy_dataset1))
  
  return(data[remain])
}

remove_columns_dataset2<-function(data){
  
  remain<-setdiff(names(data), c(remove, dummy_common))
  
  return(data[remain])
}

####################
# Feature engeneering
###################

#load("C://bb//airbnb//data//FF_train_clean.RData")

#load("C://bb//airbnb//data//FF_dev_clean.RData")

load("C://bb//airbnb//data//FF_test_clean.RData")

setwd("C://bb//airbnb")  

df<-add_new_variables(df)

df<-convert_common_categoricals_to_dummies(df)

df<-convert_amenities_to_dummy(df, most.popular.amenities)

dim(df)

df.dataset1<-convert_dataset1_categoricals_to_dummies(df)

dim(df.dataset1)

df.dataset1<-remove_columns_dataset1(df.dataset1)

dim(df.dataset1)

df.dataset2<-convert_dataset2_categoricals_to_numeric(df)

dim(df.dataset2)

df.dataset2<-remove_columns_dataset2(df.dataset2)

dim(df.dataset2)



#filename_clean_enriched1<-"./data/FF_train_clean_enriched1.csv"
#filename_clean_enriched2<-"./data/FF_train_clean_enriched2.csv"

#filename_clean_enriched1<-"./data/FF_dev_clean_enriched1.csv"
#filename_clean_enriched2<-"./data/FF_dev_clean_enriched2.csv"

filename_clean_enriched1<-"./data/FF_test_clean_enriched1.csv"
filename_clean_enriched2<-"./data/FF_test_clean_enriched2.csv"


con1<-file(filename_clean_enriched1,encoding="UTF-8")
con2<-file(filename_clean_enriched2,encoding="UTF-8")

write.csv(df.dataset1, file=con1, row.names = FALSE)
write.csv(df.dataset2, file=con2, row.names = FALSE)


save(df.dataset1, file=gsub(".csv", ".RData", filename_clean_enriched1))
save(df.dataset2, file=gsub(".csv", ".RData", filename_clean_enriched2))

dim(df.dataset1)

