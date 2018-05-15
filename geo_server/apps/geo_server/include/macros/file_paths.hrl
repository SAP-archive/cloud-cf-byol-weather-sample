
-define(GEONAMES_HOST, "download.geonames.org").
-define(GEONAMES_PORT, 80).
-define(GEONAMES_URL,  lists:append(["http://", ?GEONAMES_HOST, "/export/dump/"])).

-define(TARGET_DIR, "../../priv/").

-define(COUNTRY_FILE_FULL(CC), lists:append([?TARGET_DIR, CC, "/", CC, ".txt"])).
-define(COUNTRY_FILE_ETAG(CC), lists:append([?TARGET_DIR, CC, "/", CC, ".etag"])).
-define(COUNTRY_FILE_FCP(CC),  lists:append([?TARGET_DIR, CC, "/", CC, "_fcp.txt"])).
