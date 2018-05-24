%% ---------------------------------------------------------------------------------------------------------------------
%% Geoname record format for each country file
%%
%% geonameid         : integer id of record in geonames database
%% name              : name of geographical point (utf8) varchar(200)
%% asciiname         : name of geographical point in plain ascii characters,  varchar(200)
%% alternatenames    : alternatenames, comma separated, ascii names automatically transliterated, convenience attribute
%%                     from alternatename table, varchar(10000)
%% latitude          : latitude in decimal degrees (wgs84)
%% longitude         : longitude in decimal degrees (wgs84)
%% feature class     : see http://www.geonames.org/export/codes.html, char(1)
%% feature code      : see http://www.geonames.org/export/codes.html, varchar(10)
%% country code      : ISO-3166 2-letter country code, 2 characters
%% cc2               : alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
%% admin1 code       : fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for
%%                     display names of this code; varchar(20)
%% admin2 code       : code for the second administrative division, a county in the US, see file admin2Codes.txt;
%%                     varchar(80) 
%% admin3 code       : code for third level administrative division, varchar(20)
%% admin4 code       : code for fourth level administrative division, varchar(20)
%% population        : bigint (8 byte int) 
%% elevation         : in meters, integer
%% dem               : digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30''
%%                     (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
%% timezone          : the iana timezone id (see file timeZone.txt) varchar(40)
%% modification date : date of last modification in yyyy-MM-dd format
%%
%% Not all of the above fields are needed however. A reduced, internal version of this record is used
%%
%%  1 #geoname.id             => #geoname_int.id
%%  2 #geoname.name           => #geoname_int.name
%%  3 #geoname.asciiname      => Don't care
%%  4 #geoname.alternatenames => Don't care
%%  5 #geoname.latitude       => #geoname_int.latitude
%%  6 #geoname.longitude      => #geoname_int.longitude
%%  7 #geoname.feature_class  => #geoname_int.feature_class
%%  8 #geoname.feature_code   => #geoname_int.feature_code
%%  9 #geoname.country_code   => #geoname_int.country_code
%% 10 #geoname.cc2            => Don't care
%% 11 #geoname.admin1         => #geoname_int.admin1
%% 12 #geoname.admin2         => #geoname_int.admin2
%% 13 #geoname.admin3         => #geoname_int.admin3
%% 14 #geoname.admin4         => #geoname_int.admin4
%% 15 #geoname.population     => #geoname_int.population
%% 16 #geoname.elevation      => Don't care
%% 17 #geoname.dem            => Don't care
%% 18 #geoname.timezone       => #geoname_int.timezone
%% 19 #geoname.modified       => Don't care

-record(geoname_int, {id, name, latitude, longitude, feature_class, feature_code, country_code,
                      admin1, admin2, admin3, admin4, population, timezone, admin1_txt, admin2_txt, admin3_txt, admin4_txt}).
