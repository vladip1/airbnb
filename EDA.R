library(RODBC)
library(DBI)
library(HelpersMG)
library(stringr)
library(dplyr)
library(ggplot2)
library(lattice)
#library(mechkar)

library('fastcluster')
library(matrixStats)


source('C://bb//mechkar/R/mechkar.R')


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

summary(data)

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


 