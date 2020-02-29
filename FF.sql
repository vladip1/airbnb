USE airbnb;
GO

ALTER TABLE dbo.calendar
ALTER COLUMN price money;

drop view dbo.cal_v
drop view dbo.FF_V


CREATE VIEW CAL_V AS
Select 
t.listing_id,
t.week_num,
--t.price as price,
avg(t.price) as avg_price,
min(t.price) as min_price,
max(t.price) as max_price,
sum(case  when available = 'f' then 1 else 0 end) as occupied
from
(
    SELECT 
    DATEPART(week, str_date) AS week_num,
	*
    FROM dbo.calendar
) as t
GROUP BY
listing_id,
week_num



create VIEW FF_V AS
select * from 
CAL_V c
inner join dbo.listings lb
on c.listing_id = lb.id



