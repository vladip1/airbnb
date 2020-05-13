USE airbnb; 

go 

SELECT dc. Count(*) oc 
FROM   dbo.calendar dc 
GROUP  BY dc.listing_id, 
          dc.str_date 
HAVING Count(*) > 1 

SELECT dc.listing_id               id, 
       Datepart(week, dc.str_date) week_num, 
       Datepart(year, dc.str_date) year_num, 
       Count(*)                    occur 
FROM   dbo.calendar dc 
GROUP  BY dc.listing_id, 
          week_num, 
          year_num 

SELECT year_num, 
       occur, 
       Count(*) occur_occur 
FROM   (SELECT t.id, 
               Count(*) occur, 
               t.week_num, 
               t.year_num 
        FROM   (SELECT dc.listing_id               id, 
                       Datepart(week, dc.str_date) week_num, 
                       Datepart(year, dc.str_date) year_num 
                FROM   dbo.calendar dc) AS t 
        GROUP  BY t.id, 
                  t.week_num, 
                  t.year_num) AS tt 
--where occur !=7 
GROUP  BY year_num, 
          occur 
ORDER  BY year_num DESC, 
          occur_occur DESC 

DROP VIEW dbo.calendar_weeks_v 

SELECT TOP 100 * 
FROM   (SELECT t.id, 
               Count(*) occur, 
               t.week_num, 
               t.year_num 
        FROM   ff_ext_v t 
        GROUP  BY t.id, 
                  t.week_num, 
                  t.year_num) AS tt 
WHERE  occur < 7 
       AND year_num < 2020 
       AND year_num >= 2016 

SELECT Count(*) 
FROM   (SELECT t.id, 
               Count(*) occur, 
               t.week_num, 
               t.year_num 
        FROM   ff_ext_v t 
        GROUP  BY t.id, 
                  t.week_num, 
                  t.year_num) AS tt 
WHERE  occur < 7 
       AND year_num < 2020 
       AND year_num >= 2016 

--28066 
SELECT Count(*) 
FROM   (SELECT t.id, 
               Count(*) occur, 
               t.week_num, 
               t.year_num 
        FROM   ff_ext_v t 
        GROUP  BY t.id, 
                  t.week_num, 
                  t.year_num) AS tt 
WHERE  occur = 8 
       AND year_num < 2020 
       AND year_num >= 2016 

--4008 
SELECT Count(*) 
FROM   (SELECT t.id, 
               Count(*) occur, 
               t.week_num, 
               t.year_num 
        FROM   ff_ext_v t 
        GROUP  BY t.id, 
                  t.week_num, 
                  t.year_num) AS tt 
WHERE  year_num < 2020 
       AND year_num >= 2016 

--790332 
SELECT week_num, 
       year_num 
FROM   ff_ext_v 
WHERE  id = 37713870 

SELECT tt.occur, 
       tt.id, 
       tt.week_num, 
       tt.year_num 
FROM   (SELECT t.id, 
               Count(*) occur, 
               t.week_num, 
               t.year_num 
        FROM   ff_ext_v t 
        GROUP  BY t.id, 
                  t.week_num, 
                  t.year_num) AS tt 
WHERE  year_num < 2020 
       AND year_num >= 2016 
       AND id = 37713870 

--insert all the entris with of first week 2020 to be also week 53 2019 
INSERT INTO calendar 
            (rownames, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT rownames, 
       listing_id, 
       available, 
       price, 
       '2019-12-31', 
       1 
FROM   ff_ext_v 
WHERE  year_num = 2020 
       AND week_num = 1 
       AND is_migrated IS NULL 

--insert all the entris with of first week 2019 to be also week 53 2018 
INSERT INTO calendar 
            (rownames, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT rownames, 
       listing_id, 
       available, 
       price, 
       '2018-12-31', 
       1 
FROM   ff_ext_v 
WHERE  year_num = 2019 
       AND week_num = 1 
       AND is_migrated IS NULL 

--insert all the entris with of week 53 2018 to be also in first week 2019  
INSERT INTO calendar 
            (rownames, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT rownames, 
       listing_id, 
       available, 
       price, 
       '2019-01-01', 
       1 
FROM   ff_ext_v 
WHERE  year_num = 2018 
       AND week_num = 53 
       AND is_migrated IS NULL 

--insert all the entris with of first week 2018 to be also week 53 2017 
INSERT INTO calendar 
            (rownames, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT rownames, 
       listing_id, 
       available, 
       price, 
       '2017-12-31', 
       1 
FROM   ff_ext_v 
WHERE  year_num = 2018 
       AND week_num = 1 
       AND is_migrated IS NULL 

--insert all the entris with of week 53 2018 to be also in first week 2019  
INSERT INTO calendar 
            (rownames, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT rownames, 
       listing_id, 
       available, 
       price, 
       '2018-01-01', 
       1 
FROM   ff_ext_v 
WHERE  year_num = 2017 
       AND week_num = 53 
       AND is_migrated IS NULL 

SELECT Count(*) 
FROM   (SELECT t.id, 
               Count(*) occur, 
               t.week_num, 
               t.year_num 
        FROM   ff_ext_v t 
        GROUP  BY t.id, 
                  t.week_num, 
                  t.year_num) AS tt 
WHERE  year_num < 2020 
       AND year_num >= 2016 