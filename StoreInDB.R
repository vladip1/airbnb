#install.packages("RODBC")


# 1.  Take all the links frin isideairbnb
# 2.  Filter only sf calendars
#       grep san-fra insideairbnb.txt |grep calen > sf.calendars
# 3.  Clean it from html 
#       cat sf.calendars | cut -d \" -f2 > sf.calendars.cleaned


#work
setwd("C://bb//airbnb")

library(RODBC)
library(HelpersMG)
library(stringr)
library(dplyr)


#install.packages('HelpersMG')

air_conn = odbcConnect("airbnb")




############
# listings #
############

  #remove column Y (host_about)
  listings.1 <- read.csv("C:/bb/airbnb/data/big/listings.1.csv")
  listings<-as.data.frame(listings.1)
  sqlSave(air_conn, listings)

#calendar

  calendars <- read.csv("C:/bb/airbnb/sf.calendars.cleaned.csv", header = FALSE)

  df<-as.data.frame(calendars[1])
    
  #cc<-str_locate(i, "20")
  #61
  #cc<-str_locate(i, "data/calendar")
  #72
  
  cal<-data.frame()
  cal$date<-NULL
  cal$file_before<-NULL
  cal$file_after<-NULL
  
  row_num<-1
  for (i in df$V1) {
    date<-substr(i, 61, 70)
    
    file_before<-paste('calendar.', date, '.csv.gz', sep = '')
    
    file_after<-paste('calendar.', date, '.csv', sep = '')
    
    cal[row_num, 'date']<-date
    cal[row_num, 'file_before']<-paste('calendar.', date, '.csv.gz', sep = '')
    cal[row_num, 'file_after']<-paste('calendar.', date, '.csv', sep = '')
    
    row_num<-row_num + 1
    
    
    ###################################################
    
    wget(i)
    file.rename('calendar.csv.gz', file_before)
  } 

  
################## run through the calendars, extract the interval, attach to the previos one and upload to SQL

  
  cal.to.save<-NULL
  df<-cal
  
  #extract the latest period anbd take the first month
  period<-read.csv(paste("C:/bb/airbnb/data/calendars/", df[1,'file_after'],"/calendar.csv",sep = ''))
  period$date<-as.Date(period$date)
  cal.to.save<-period %>% filter(date < as.Date('2020-01-04')) %>% select(listing_id, date, available, price)
  

  for (i in 2:nrow(df))
  {
    to_date<-df[i-1, "date"]

    period<-read.csv(paste("C:/bb/airbnb/data/calendars/", df[i,'file_after'],"/calendar.csv",sep = ''))
    period$date<-as.Date(period$date)
    period.to.save<-period %>% filter(date < as.Date(to_date)) %>% select(listing_id, date, available, price)
    
    cal.to.save <-rbind(cal.to.save, period.to.save)
    
  }
  
cal.to.save$str_date<-as.character.Date(cal.to.save$date)

cal.to.save$date<-NULL

sqlSave(air_conn, cal.to.save, tablename = 'calendar')

#reviews
reviews<-as.data.frame(reviews)
sqlSave(air_conn, reviews)

close(air_conn)




# 1  2019-12-04 calendar.2019-12-04.csv.gz calendar.2019-12-04.csv
# 2  2019-11-01 calendar.2019-11-01.csv.gz calendar.2019-11-01.csv
# 3  2019-10-14 calendar.2019-10-14.csv.gz calendar.2019-10-14.csv
# 4  2019-09-12 calendar.2019-09-12.csv.gz calendar.2019-09-12.csv
# 5  2019-08-06 calendar.2019-08-06.csv.gz calendar.2019-08-06.csv
# 6  2019-07-08 calendar.2019-07-08.csv.gz calendar.2019-07-08.csv
# 7  2019-06-02 calendar.2019-06-02.csv.gz calendar.2019-06-02.csv
# 8  2019-05-03 calendar.2019-05-03.csv.gz calendar.2019-05-03.csv
# 9  2019-04-03 calendar.2019-04-03.csv.gz calendar.2019-04-03.csv
# 10 2019-03-06 calendar.2019-03-06.csv.gz calendar.2019-03-06.csv
# 11 2019-02-01 calendar.2019-02-01.csv.gz calendar.2019-02-01.csv
# 12 2019-01-09 calendar.2019-01-09.csv.gz calendar.2019-01-09.csv
# 13 2018-12-06 calendar.2018-12-06.csv.gz calendar.2018-12-06.csv
# 14 2018-11-03 calendar.2018-11-03.csv.gz calendar.2018-11-03.csv
# 15 2018-10-03 calendar.2018-10-03.csv.gz calendar.2018-10-03.csv
# 16 2018-09-08 calendar.2018-09-08.csv.gz calendar.2018-09-08.csv
# 17 2018-08-06 calendar.2018-08-06.csv.gz calendar.2018-08-06.csv
# 18 2018-07-05 calendar.2018-07-05.csv.gz calendar.2018-07-05.csv
# 19 2018-05-09 calendar.2018-05-09.csv.gz calendar.2018-05-09.csv
# 20 2018-04-06 calendar.2018-04-06.csv.gz calendar.2018-04-06.csv
# 21 2018-03-04 calendar.2018-03-04.csv.gz calendar.2018-03-04.csv
# 22 2018-02-02 calendar.2018-02-02.csv.gz calendar.2018-02-02.csv
# 23 2018-01-17 calendar.2018-01-17.csv.gz calendar.2018-01-17.csv
# 24 2018-01-10 calendar.2018-01-10.csv.gz calendar.2018-01-10.csv
# 25 2017-12-07 calendar.2017-12-07.csv.gz calendar.2017-12-07.csv
# 26 2017-12-02 calendar.2017-12-02.csv.gz calendar.2017-12-02.csv
# 27 2017-11-08 calendar.2017-11-08.csv.gz calendar.2017-11-08.csv
# 28 2017-11-01 calendar.2017-11-01.csv.gz calendar.2017-11-01.csv
# 29 2017-10-02 calendar.2017-10-02.csv.gz calendar.2017-10-02.csv
# 30 2017-09-02 calendar.2017-09-02.csv.gz calendar.2017-09-02.csv
# 31 2017-08-02 calendar.2017-08-02.csv.gz calendar.2017-08-02.csv
# 32 2017-07-02 calendar.2017-07-02.csv.gz calendar.2017-07-02.csv
# 33 2017-06-02 calendar.2017-06-02.csv.gz calendar.2017-06-02.csv
# 34 2017-05-02 calendar.2017-05-02.csv.gz calendar.2017-05-02.csv
# 35 2017-04-02 calendar.2017-04-02.csv.gz calendar.2017-04-02.csv
# 36 2017-03-02 calendar.2017-03-02.csv.gz calendar.2017-03-02.csv
# 37 2017-02-02 calendar.2017-02-02.csv.gz calendar.2017-02-02.csv
# 38 2017-01-01 calendar.2017-01-01.csv.gz calendar.2017-01-01.csv
# 39 2016-12-03 calendar.2016-12-03.csv.gz calendar.2016-12-03.csv
# 40 2016-11-02 calendar.2016-11-02.csv.gz calendar.2016-11-02.csv
# 41 2016-10-01 calendar.2016-10-01.csv.gz calendar.2016-10-01.csv
# 42 2016-09-02 calendar.2016-09-02.csv.gz calendar.2016-09-02.csv
# 43 2016-08-02 calendar.2016-08-02.csv.gz calendar.2016-08-02.csv
# 44 2016-07-02 calendar.2016-07-02.csv.gz calendar.2016-07-02.csv
# 45 2016-06-02 calendar.2016-06-02.csv.gz calendar.2016-06-02.csv
# 46 2016-05-02 calendar.2016-05-02.csv.gz calendar.2016-05-02.csv
# 47 2016-04-03 calendar.2016-04-03.csv.gz calendar.2016-04-03.csv
# 48 2016-02-02 calendar.2016-02-02.csv.gz calendar.2016-02-02.csv
# 49 2015-12-02 calendar.2015-12-02.csv.gz calendar.2015-12-02.csv
# 50 2015-11-01 calendar.2015-11-01.csv.gz calendar.2015-11-01.csv
# 51 2015-09-02 calendar.2015-09-02.csv.gz calendar.2015-09-02.csv
# 52 2015-05-04 calendar.2015-05-04.csv.gz calendar.2015-05-04.csv
