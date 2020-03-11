library(RODBC)
library(DBI)
library(HelpersMG)
library(stringr)
library(dplyr)


setwd("C://bb//airbnb")

#con = odbcConnect("airbnb")
#airbnb_ff<-sqlQuery(con, "select * from dbo.FF_V")
#save(airbnb_ff, file="C://bb//insideairbnb//airbnb_ff.RData")

load("C://bb//insideairbnb//airbnb_ff.RData")    

summary(airbnb_ff)

head(airbnb_ff)

str(airbnb_ff)

#remove not needed factors, keep only the factors with limited numver of levels
fct<-names(Filter(is.factor, airbnb_ff))

fct.final<-fct

fct.final<-fct.final[-grep('require_guest_profile_picture', fct.final)]

fct.final<-fct.final[-grep('require_guest_phone_verification', fct.final)]

fct.final<-fct.final[-grep('cancellation_policy', fct.final)]

fct.final<-fct.final[-grep('instant_bookable', fct.final)]

#convert to numerics

$ price                           : Factor w/ 526 levels "$0.00 ","$1,000.00 ",..: 94 129 304 195 188 383 64 509 522 113 ...
$ security_deposit                : Factor w/ 97 levels "","$0.00 ","$1,000.00 ",..: 77 85 1 45 77 3 41 41 77 2 ...
$ cleaning_fee                    : Factor w/ 195 levels "","$0.00 ","$10.00 ",..: 4 185 191 4 134 40 53 101 15 2 ...
$ extra_people 


fct.final<-fct.final[-grep('require_guest_profile_picture', fct.final)]

fct.final<-fct.final[-grep('require_guest_phone_verification', fct.final)]

fct.final<-fct.final[-grep('cancellation_policy', fct.final)]

fct.final<-fct.final[-grep('instant_bookable', fct.final)]



fct.final<-fct.final[-grep('host_response_time', fct.final)]

fct.final<-fct.final[-grep('host_is_superhost', fct.final)]

fct.final<-fct.final[-grep('cancellation_policy', fct.final)]

fct.final<-fct.final[-grep('instant_bookable', fct.final)]

[neighbourhood_cleansed]
[is_location_exact]
[property_type]
room_type


#install.packages('mechkar')


library(mechkar)

exploreData(airbnb_ff, y = 'occupied')
