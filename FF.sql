USE airbnb;
GO


ALTER TABLE dbo.calendar
ALTER COLUMN price money;

ALTER TABLE dbo.calendar
ALTER COLUMN price decimal(10,2);

ALTER TABLE dbo.listings
ALTER COLUMN security_deposit money;

ALTER TABLE dbo.listings
ALTER COLUMN security_deposit decimal(10,2);

ALTER TABLE dbo.listings
ALTER COLUMN cleaning_fee money;

ALTER TABLE dbo.listings
ALTER COLUMN cleaning_fee decimal(10,2);

ALTER TABLE dbo.listings
ALTER COLUMN extra_people decimal(10,2);


ALTER TABLE dbo.calendar
ADD is_migrated int;

drop view dbo.cal_v
drop view dbo.FF_V



drop view calendar_ext_v;

--	extend calendar with with date fields (day, week, year)
create view calendar_ext_v as
SELECT 
cc.available,
cc.listing_id,
cc.price as cal_price,
cc.str_date,
cc.is_migrated,
DATEPART(year, str_date) AS year_num,
DATEPART(week, str_date) AS week_num,
DATEPART(day, str_date) AS day_num
FROM dbo.calendar as cc

select top 100 * from calendar_ext_v

drop view ff_ext_v

--	create FF view with day resolution
create VIEW ff_ext_v AS
select * from 
calendar_ext_v c
inner join dbo.listings lb
on c.listing_id = lb.id

---------------------------------------------------------------------------------------------------------------------------
---------------------------------FIX EDGES


--insert all the entris with of first week 2020 to be also week 53 2019
INSERT INTO calendar(rownames, listing_id, available, price, str_date, is_migrated)
SELECT rownames, listing_id, available, price, '2019-12-31', 1 FROM ff_ext_v where year_num=2020 and week_num=1 and is_migrated IS NULL

--insert all the entris with of first week 2019 to be also week 53 2018
INSERT INTO calendar(rownames, listing_id, available, price, str_date, is_migrated)
SELECT rownames, listing_id, available, price, '2018-12-31', 1 FROM ff_ext_v where year_num=2019 and week_num=1 and is_migrated IS NULL

--insert all the entris with of week 53 2018 to be also in first week 2019 
INSERT INTO calendar(rownames, listing_id, available, price, str_date, is_migrated)
SELECT rownames, listing_id, available, price, '2019-01-01', 1 FROM ff_ext_v where year_num=2018 and week_num=53 and is_migrated IS NULL

--insert all the entris with of first week 2018 to be also week 53 2017
INSERT INTO calendar(rownames, listing_id, available, price, str_date, is_migrated)
SELECT rownames, listing_id, available, price, '2017-12-31', 1 FROM ff_ext_v where year_num=2018 and week_num=1 and is_migrated IS NULL

--insert all the entris with of week 53 2018 to be also in first week 2019 
INSERT INTO calendar(rownames, listing_id, available, price, str_date, is_migrated)
SELECT rownames, listing_id, available, price, '2018-01-01', 1 FROM ff_ext_v where year_num=2017 and week_num=53 and is_migrated IS NULL

---------------------------------------------------------------------------------------------------------------------------

--	aggregate calendar and move to week resolution
create view cal_v as
select
t.listing_id,
t.week_num,
t.year_num,
avg(t.price) as avg_price,
min(t.price) as min_price,
max(t.price) as max_price,
sum(case  when available = 'f' then 1 else 0 end) as occupied,
count(*) as occur
from
(
    SELECT 
	DATEPART(year, str_date) AS year_num,
    DATEPART(week, str_date) AS week_num,
	*
    FROM dbo.calendar
) as t
GROUP BY
listing_id,
week_num,
year_num

--	create FF with week resolution
create VIEW FF_V AS
select * from 
CAL_V c
inner join dbo.listings lb
on c.listing_id = lb.id


--	see all the counts
select count(*) from FF_V
where year_num < 2020 and year_num >= 2016 and occur >= 7

--	see the distribution of weeks
select occupied, count(*) cnt from FF_V
where year_num < 2020 and year_num >= 2016 and occur >= 7
group by occupied
order by cnt desc

