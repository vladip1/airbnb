USE airbnb; 

go 

ALTER TABLE dbo.calendar 
  ALTER COLUMN price MONEY; 

ALTER TABLE dbo.calendar 
  ALTER COLUMN price DECIMAL(10, 2); 

ALTER TABLE dbo.listings 
  ALTER COLUMN price MONEY; 

ALTER TABLE dbo.listings 
  ALTER COLUMN price DECIMAL(10, 2); 

ALTER TABLE dbo.listings 
  ALTER COLUMN weekly_price MONEY; 

ALTER TABLE dbo.listings 
  ALTER COLUMN weekly_price DECIMAL(10, 2); 

ALTER TABLE dbo.listings 
  ALTER COLUMN monthly_price MONEY; 

ALTER TABLE dbo.listings 
  ALTER COLUMN monthly_price DECIMAL(10, 2); 

ALTER TABLE dbo.listings 
  ALTER COLUMN security_deposit MONEY; 

ALTER TABLE dbo.listings 
  ALTER COLUMN security_deposit DECIMAL(10, 2); 

ALTER TABLE dbo.listings 
  ALTER COLUMN cleaning_fee MONEY; 

ALTER TABLE dbo.listings 
  ALTER COLUMN cleaning_fee DECIMAL(10, 2); 

ALTER TABLE dbo.listings 
  ALTER COLUMN extra_people MONEY; 

ALTER TABLE dbo.listings 
  ALTER COLUMN extra_people DECIMAL(10, 2); 

ALTER TABLE dbo.calendar 
  ADD is_migrated INT; 

DROP VIEW dbo.cal_v 

DROP VIEW dbo.ff_v 

--set monday to be the first day of the week 
--set datefirst 1 
DROP VIEW calendar_ext_v; 

--  extend calendar with with date fields (day, week, year) 
CREATE VIEW calendar_ext_v 
AS 
  SELECT cc.available, 
         cc.listing_id, 
         cc.price, 
         cc.str_date, 
         cc.is_migrated, 
         cc.period, 
         Datepart(year, str_date) AS year_num, 
         Datepart(week, str_date) AS week_num, 
         Datepart(day, str_date)  AS day_num 
  FROM   dbo.calendar AS cc 

--------------------------------------------------------------------------------------------------------------------------- 
---------------------------------FIX EDGES 
--insert all the entris with of first week 2020 to be also week 53 2019 
INSERT INTO calendar 
            (period, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT period, 
       listing_id, 
       available, 
       price, 
       '2019-12-31', 
       1 
FROM   calendar_ext_v 
WHERE  year_num = 2020 
       AND week_num = 1 
       AND is_migrated IS NULL 

INSERT INTO calendar 
            (period, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT period, 
       listing_id, 
       available, 
       price, 
       '2020-01-01', 
       1 
FROM   calendar_ext_v 
WHERE  year_num = 2019 
       AND week_num = 53 
       AND is_migrated IS NULL 

--insert all the entris with of first week 2019 to be also week 53 2018 
INSERT INTO calendar 
            (period, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT period, 
       listing_id, 
       available, 
       price, 
       '2018-12-31', 
       1 
FROM   calendar_ext_v 
WHERE  year_num = 2019 
       AND week_num = 1 
       AND is_migrated IS NULL 

--insert all the entris with of week 53 2018 to be also in first week 2019  
INSERT INTO calendar 
            (period, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT period, 
       listing_id, 
       available, 
       price, 
       '2019-01-01', 
       1 
FROM   calendar_ext_v 
WHERE  year_num = 2018 
       AND week_num = 53 
       AND is_migrated IS NULL 

--insert all the entris with of first week 2018 to be also week 53 2017 
INSERT INTO calendar 
            (period, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT period, 
       listing_id, 
       available, 
       price, 
       '2017-12-31', 
       1 
FROM   calendar_ext_v 
WHERE  year_num = 2018 
       AND week_num = 1 
       AND is_migrated IS NULL 

--insert all the entris with of week 53 2018 to be also in first week 2019  
INSERT INTO calendar 
            (period, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT period, 
       listing_id, 
       available, 
       price, 
       '2018-01-01', 
       1 
FROM   calendar_ext_v 
WHERE  year_num = 2017 
       AND week_num = 53 
       AND is_migrated IS NULL 

--insert all the entris with of first week 2018 to be also week 53 2017 
INSERT INTO calendar 
            (period, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT period, 
       listing_id, 
       available, 
       price, 
       '2015-12-31', 
       1 
FROM   calendar_ext_v 
WHERE  year_num = 2016 
       AND week_num = 1 
       AND is_migrated IS NULL 

--insert all the entris with of week 53 2018 to be also in first week 2019  
INSERT INTO calendar 
            (period, 
             listing_id, 
             available, 
             price, 
             str_date, 
             is_migrated) 
SELECT period, 
       listing_id, 
       available, 
       price, 
       '2016-01-01', 
       1 
FROM   calendar_ext_v 
WHERE  year_num = 2015 
       AND week_num = 53 
       AND is_migrated IS NULL 

--------------------------------------------------------------------------------------------------------------------------- 
DROP VIEW cal_v 

--  aggregate calendar and move to week resolution 
CREATE VIEW cal_v 
AS 
  SELECT t.listing_id, 
         t.week_num, 
         t.year_num, 
         Max(t.period) AS cal_period, 
         Avg(t.price)  AS avg_price, 
         Min(t.price)  AS min_price, 
         Max(t.price)  AS max_price, 
         Sum(CASE 
               WHEN available = 'f' THEN 1 
               ELSE 0 
             END)      AS occupied, 
         Count(*)      AS occur 
  FROM   (SELECT Datepart(year, str_date) AS year_num, 
                 Datepart(week, str_date) AS week_num, 
                 * 
          FROM   dbo.calendar) AS t 
  GROUP  BY listing_id, 
            week_num, 
            year_num 

DROP VIEW ff_v 

--  create FF with week resolution 
CREATE VIEW ff_v 
AS 
  SELECT *, 
         year_num - Datepart(year, host_since) AS host_seniority 
  FROM   cal_v c 
         INNER JOIN dbo.listings lb 
                 ON c.listing_id = lb.id 
                    AND c.cal_period = lb.period 
                    AND occur >= 7 