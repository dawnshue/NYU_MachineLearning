----------------
--DEMDEX_OUTPUT
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
location 's3://cmcdf/cafemom/';
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
location 's3://cmcdf/hive_tables/user_events3/';

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
----------------
insert overwrite table segment_hispanic partition(class)
select uuid, max(case when datatype = 4 and 
(
--contains es
array_contains(varints,780664) or array_contains(varints,920203)
 )
then 1
 else 0 end ) class
from user_attribs2
where 
--contains es or startswith en
(datatype = 4 and (array_contains(varints,780664) or array_contains(varints,783538)  or array_contains(varints,920203)  or array_contains(varints,920206) ))
group by uuid;


