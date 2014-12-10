----------------
--DEMDEX_OUTPUT
--Raw data, features are traits
----------------
create external table if not exists demdex_output (
eventTime string
, uuid string
, containerId int
, realizedTraits array<int>
, realizedSegments array<int>
, requestParams map<string,string>
, referer string
, ip string)
partitioned by (day string, hour string)
stored as textfile
location '';
msck repair table demdex_output ;
--EXAMPLE:
--eventTime: 2014-11-25 00:08:32
--uuid: 00004607488609013800850154562513907797
--containerId: NULL
--realizedTraits: [786082,766276,928072,783538,766272,934770,928071,856733,957974,794385,934760,1345599,766289,794388,957777,691152,708095,766239,766269,737518]	
--realizedSegments: [73408,104155,103686,104090,104117,104113,104110,126411,104749,104750,104074,81627,104071,104160,104070,104132,104045]	
--requestParams: {"c_moatevent":"measurable","c_mmsite":"BlackFriday.fm","c_mmstrat":"549269","c_crsize":"undefinedxundefined","c_mmcamp":"168013","c_partner":"moat","c_mmaid":"788902097318219129","c_mmexch":"cri","c_moatsite":"blackfriday.fm"}
--referer: http://www.blackfriday.fm/adscan/walmart?page=3
--ip: 71.196.118.83
--day: 2014-11-25, hour: 00

----------------
--USER_EVENTS3
----------------
create external table if not exists user_events3(
uuid string
, varstrings array<string>
, varints array<int>)
partitioned by (datatype smallint, day string)
clustered by (uuid) into 16 buckets
stored as sequencefile
location '';

insert overwrite table user_events3 partition(datatype=4, day)
select uuid, combine_unique(cast_array(realizedTraits)), combine_unique(realizedTraits),day
from demdex_output
where uuid is not null group by uuid, day;


----------------
--USERATTRIBS2
----------------
INSERT overwrite table user_attribs2_temp partition(datatype=4) 
SELECT uuid, combine_unique(varints), count(*) cnt 
from user_events3 
where datatype = 4 group by uuid, datatype;

insert overwrite table user_attribs2 partition(datatype=4) select uuid, varints, cnt 
from user_attribs2_temp where datatype =4;

----------------
--SEGMENT_HISPANIC
--class 1 as "contains es" and class 0 as "contains en"
--class 1 ~2% of total uuids
----------------
create external table if not exists segment_hispanic( uuid string)
partitioned by (class smallint)
clustered by (uuid) into 256 buckets
stored as textfile
location '';

insert overwrite table segment_hispanic partition(class)
select uuid, max(case when datatype = 4 and 
(array_contains(varints,780664) or array_contains(varints,920203))
then 1 else 0 end ) class
from user_attribs2
where (
datatype = 4 and 
(array_contains(varints,780664) 
or array_contains(varints,783538) 
or array_contains(varints,920203) 
or array_contains(varints,920206)
))
group by uuid;

----------------
--SEGMENT_HISPANIC2
--increase weight of class=1 to ~19% of total uuids
----------------
create external table if not exists segment_hispanic2( uuid string)
partitioned by (class smallint)
clustered by (uuid) into 8 buckets
stored as sequencefile
location '';

insert overwrite table segment_hispanic2 partition(class)
select * from ( 
select * from segment_hispanic where class = 1 
union all 
select * from segment_hispanic tablesample(bucket 1 out of 10)
) s;

----------------
--USER_ATTRIBS3, USER_FLAGS, HASH_LOOKUP
----------------

insert overwrite table user_attribs3 select * from user_attribs3_temp;
insert overwrite table user_flags select * from user_flags_temp;

insert overwrite table hash_lookup_temp partition(datatype=7)
select varstring
,concat(datatype, '_', case when datatype in (4,5,13,14) then cast(varstring as int) else hash(varstring) end)
,case when datatype in (4,5,13,14) then cast(varstring as int) else hash(varstring) end
,count(distinct uuid)
from user_events3 lateral view explode(varstrings) exploded_table AS varstring
where datatype=7 group by datatype, varstring having count(distinct uuid) > 4;
insert overwrite table hash_lookup partition(datatype) select * from hash_lookup_temp;

----------------
--SEGMENT_TRAIN_HISPANIC
----------------
create external table if not exists segment_train_hispanic(class smallint, outvars string)
ROW FORMAT DELIMITED FIELDS TERMINATED BY '\t' collection items terminated by ','
stored as textfile 
location '';

SET hive.exec.compress.output=false;
set hive.optimize.skewjoin=false; 
insert overwrite table segment_train_hispanic
select class, collect_set(varstring) 
from  hash_lookup hlt  join ( select uuid, varstring, class from
(select a.uuid, a.outvars, c.class 
from user_attribs3 a
join user_flags b on a.uuid = b.uuid
--join segment_hispanic c on a.uuid = c.uuid
--join (select * from segment_hispanic tablesample(bucket 1 out of 8)) c on a.uuid = c.uuid
join segment_hispanic2 c on a.uuid = c.uuid
where b.bot = 0 and b.us = 1 and b.has_domains = 1
) uf lateral view explode(outvars) uf_exp as varstring ) uf_exp2 on (hlt.hashname = uf_exp2.varstring and split(uf_exp2.varstring, '_')[0] = hlt.datatype) left outer join demdex_traits2 ddt on (hlt.datatype = 4 and hlt.variable = ddt.traitid)
where ((ddt.folderid is null and hlt.datatype <> 4) or (ddt.fpath not like '%3rd-PartyData%' 
and ddt.fpath not like '%UUID%'
and ddt.fpath not like '%Universal-UserLevel/Samp%'
and ddt.fpath not like '%Universal-UserLevel/Accept%'
and ddt.fpath not like '%GreatSchools/Universal%'))
and datatype in (4, 7, 8, 9, 10, 11,12,13,14,15, 16,17,19,20,21,22,23)
group by uuid, class;

----------------
--SEGMENT_HISPANIC SAMPLE
--Divide data into 8 buckets, 7 used for training and 1 for testing
----------------
create external table if not exists segment_hispanic_sample (class smallint, outvars string)
clustered by (outvars) into 8 buckets
ROW FORMAT DELIMITED FIELDS TERMINATED BY '\t' collection items terminated by ','
stored as textfile 
location '';

insert overwrite table segment_hispanic_sample
select * from segment_train_hispanic;

--insert overwrite table segment_hispanic_sample partition(sample='test')
--select * from segment_train_hispanic tablesample(bucket 1 out of 8 on rand()) s;

--insert overwrite table segment_hispanic_sample partition(sample='train')
--select a.* from segment_train_hispanic a
--left outer join segment_hispanic_sample b on a.class = b.class and a.outvars = b.outvars
--where b.sample='test' and b.outvars is null;



