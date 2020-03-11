USE airbnb;
GO



select dc. count(*) oc from dbo.calendar dc
group by  dc.listing_id, dc.str_date
having count(*) > 1



select dc.listing_id id, 
DATEPART(week, dc.str_date) week_num,
DATEPART(year, dc.str_date) year_num, 
count(*) occur
from dbo.calendar dc
group by dc.listing_id, week_num, year_num



select 
	year_num,
	occur,
	count(*) occur_occur
from
	(select 
	t.id, 
	count(*) occur, 
	t.week_num, 
	t.year_num 
		from 
			(select dc.listing_id id, 
			DATEPART(week, dc.str_date) week_num,
			DATEPART(year, dc.str_date) year_num 
			from dbo.calendar dc) as t
			group by t.id, t.week_num, t.year_num) as tt
			--where occur !=7
			group by year_num, occur
			order by year_num desc, occur_occur desc


drop view dbo.calendar_weeks_v

select top 100 * from
(select 
	t.id, 
	count(*) occur, 
	t.week_num, 
	t.year_num
from ff_ext_v t
group by t.id, t.week_num, t.year_num) as tt
where occur <7 and year_num < 2020 and year_num >= 2016

select count(*) from
(select 
	t.id, 
	count(*) occur, 
	t.week_num, 
	t.year_num
from ff_ext_v t
group by t.id, t.week_num, t.year_num) as tt
where occur<7 and year_num < 2020 and year_num >= 2016

--28066


select count(*) from
(select 
	t.id, 
	count(*) occur, 
	t.week_num, 
	t.year_num
from ff_ext_v t
group by t.id, t.week_num, t.year_num) as tt
where occur=8 and year_num < 2020 and year_num >= 2016

--4008

select count(*) from
(select 
	t.id, 
	count(*) occur, 
	t.week_num, 
	t.year_num
from ff_ext_v t
group by t.id, t.week_num, t.year_num) as tt
where year_num < 2020 and year_num >= 2016

--790332

select week_num, year_num from ff_ext_v where id=37713870

select tt.occur, tt.id, tt.week_num, tt.year_num from
(select 
	t.id, 
	count(*) occur, 
	t.week_num, 
	t.year_num
from ff_ext_v t
group by t.id, t.week_num, t.year_num) as tt
where year_num < 2020 and year_num >= 2016 and id=37713870


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



select count(*) from
(select 
	t.id, 
	count(*) occur, 
	t.week_num, 
	t.year_num
from ff_ext_v t
group by t.id, t.week_num, t.year_num) as tt
where year_num < 2020 and year_num >= 2016
