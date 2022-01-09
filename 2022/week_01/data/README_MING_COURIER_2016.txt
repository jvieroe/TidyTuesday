MING COURIER ROUTES AND STATIONS

Version:  2

Publication Date:  2016-11-16

Editor:  Lex Berman

Original Editors and Compilers:   Lex Berman, Wei Zhang (Nankai Univ) and Said Douai

Encoding:  UTF8

Projection:  WGS84  (EPSG 4326)

SOURCES:
Mingdai Yizhankao (MDYZK) by YANG Zhengtai
1903 Postal Atlas

ATTRIBUTES:
SRC:  MDYZK, PA_1903  (as above)
STATUS:  Editing notes
YZ_ID:  Unique ID of the station (note some out of sequence, removed from earlier drafts)
YZ_LABEL:  Pinyin Name of Station, Modern District, (Chinese Name of Station)
YZNM_PY:  Pinyin Name of Station
YZNM_CH:  Chinese Simplified Name of Station
YZNM_FT:  Chinese Traditional Name of Station
CHGIS_ID:  Corresponding Unique ID in CHGIS
NOTE:  Editing Notes about location
10CNTY_CH:  Chinese name of 2010 Census County that the station is located in
10CNTY_PY:  Pinyin name of 2010 Census County that the station is located in
10PREF_CH:  Chinese name of 2010 Census District that the station is located in
10PREF_PY:  Pinyin name of 2010 Census District that the station is located in
10PROV_CH:  Chinese name of 2010 Census Province that the station is located in
10PROV_PY:  Pinyin name of 2010 Census Province that the station is located in
GB_CODE:  Guobio Code number of the County Level Unit that the station is located in 
YZ_LAT: Calculated Latitude 
YZ_LONG: Calculated Longitude

Editing Notes:

This version contains numerous corrections to the original draft, published in 2014.  The corrections include moving the draft positions of the station points to their corresponding points found in CHGIS datasets, and updating the correct SYS_ID numbers from CHGIS.

In some cases the names were updated based on those shown in the MDYZK.   One missing route (from Chongqing heading Northeast to Wan Xian) was added, along with eleven new stations on the route.

The routes themselves were adjusted to connect at the station points.   This includes a number of corrections where the original draft had dangling nodes that had not been connected.

Wherever possible the station names are those reflected in the source material, MDYZK.    When finding relationships from station names to point locations in CHGIS, there are cases where the names are identical, or very similar.   A similar case is shown in these examples:

ZHUJIA'AOYI Zhejiang (朱家奥驿)    geocoded to CHGIS  ->  sys id 140078  朱奥市
 
One case was found where the Station name matched to a CHGIS location that is far away (probably an error in CHGIS).  For example:

SHIGAO Yunnan (石膏)   the placename occurs in CHGIS ->  sys id 22685  石膏箐井

In this case NO MATCH was made to the CHGIS SYS_ID because the x, y coordinates are so different.

In another case, the historical station is located in location now submerged by a modern reservoir.
SHIXIAYI Beijing  石匣驿 is under the Miyun Reservoir as noted in this reference: 
http://blog.sina.com.cn/s/blog_4c4d5bc60100joku.html

There were numerous matches  based on location, even though the station names do not match the CHGIS point location names.   Many of these are corroborated on the original MDYZK source maps.    Other cases of finding station locations where no CHGIS points are available involved searching the area on modern basemaps such as Google Maps or Open Street Map to find a possible location .   An example of this type of match is:

POXI Yunnan (婆兮)  location matched to OpenStreetMap ->  Pan Xi Zhen   盘溪镇  
PUQIAN Guangdong (埔前)  location matched to OpenStreetMap ->  Pu Qian Cun  埔前村

Summary of matching Courier Stations to CHGIS Points:

974 Station locations found with matching CHGIS points and SYS_IDs  (97.4%)
26 Station locations could not be matched with CHGIS SYS_IDs  (2.6%)

Out of the 974 matches:
358 Station locations retained the matching CHGIS SYS_IDs from version 1 (35.8%)
616 Station locations had CHGIS SYS_IDs corrected or added in version 2 (61.6%)







