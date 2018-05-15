-module(import_files).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/02/08 11:15:07").
-created_by("chris.whealy@sap.com").

-export([
    import_country_info/2
  , read_country_file/2
  , http_get_request/3
  , move_file/4
  , handle_zip_file/4
]).

%% Records
-include("../include/records/geoname.hrl").

%% Macros
-include("../include/macros/trace.hrl").
-include("../include/macros/file_paths.hrl").
-include("../include/macros/now.hrl").

%% Files become stale after 24 hours
-define(ONE_DAY,     60 * 60 * 24).
-define(STALE_AFTER, ?ONE_DAY).

%% Wait 5 seconds before retrying a file download.
%% Make no more than 3 download attempts before reporting an error
-define(RETRY_WAIT,  5000).
-define(RETRY_LIMIT, 3).

%% Report file import progress in 1% increments
-define(PROGRESS_FRACTION, 0.01).

%% Feature class P records (Population centres) having a population below this limit will not be included in a
%% country's FCP file
-define(MIN_POPULATION, 500).

%% Utilities
-include("../include/utils/http_status_codes.hrl").



%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Import the country info file to create a list of {country_code, country_name, continent}
%% Send this list back to the top level application
import_country_info(ApplicationPid, {ProxyHost, ProxyPort}) ->
  %% Debug trace output
  put(trace, true),

  %% Store proxy information in the process dictionary
  put(proxy_host, ProxyHost),
  put(proxy_port, ProxyPort),

  {ok, PWD} = file:get_cwd(),
  ?TRACE("Present working directory is ~s", [PWD]),

  %% Download the country info file
  spawn(?MODULE, http_get_request, [self(), "countryInfo", ".txt"]),
  retry(wait_for_resources(1, text), text, 1),
  
  ApplicationPid ! case parse_countries_file("countryInfo", ".txt") of
    {ok, Countries} -> {country_list, Countries};
    {error, Reason} -> exit({parse_error, Reason})
  end.


%% ---------------------------------------------------------------------------------------------------------------------
%% Import the internal FCP file if it exists, else import the full country text file.
read_country_file(CC, CountryServerPid) when is_pid(CountryServerPid) ->
  {registered_name, CountryServerName} = erlang:process_info(CountryServerPid, registered_name),
  
  put(my_name, CountryServerName),
  put(trace, process_tools:read_process_dictionary(CountryServerPid, trace)),
  
  %% Get the proxy information from the process dictionary of the country_manager
  put(proxy_host, process_tools:read_process_dictionary(whereis(country_manager), proxy_host)),
  put(proxy_port, process_tools:read_process_dictionary(whereis(country_manager), proxy_port)),

  check_for_update(CC),
  read_country_file_int(CC, filelib:file_size(?COUNTRY_FILE_FCP(CC)), CountryServerPid).


%% ---------------------------------------------------------------------------------------------------------------------
%% Always perform a conditional HTTP GET request
%%
http_get_request(CallerPid, Filename, Ext) ->
  Url = ?GEONAMES_URL ++ Filename ++ Ext,

  %% Read the debug mode setting from the process dictionary of the calling process
  case process_tools:read_process_dictionary(CallerPid, trace) of
    true -> put(trace, true);
    _    -> put(trace, false)
  end,

  %% Define HTTP GET request options
  Options = define_http_options(CallerPid),

  %% Check to see if we have an ETag for this file
  Response = case read_etag_file(Filename) of
    missing -> 
      ?TRACE("ETag file missing, sending normal GET request for ~s",[Url]),
      ibrowse:send_req(Url, [], get, [], Options);

    Etag ->
      ?TRACE("Sending conditional GET request for ~s",[Url]),
      ibrowse:send_req(Url, [{"If-None-Match", Etag}], get, [], Options)
  end,
    
  CallerPid ! case Response of
    {ok, "200", Hdrs, {file, TempFilename}} ->
      ?TRACE("HTTP 200 for ~s",[Url]),
      {ok, Filename, Ext, get_etag(Hdrs), TempFilename};

    {ok, "304", _Hdrs, _Body} ->
      ?TRACE("HTTP 304 for ~s",[Url]),
      {not_modified, Filename, Ext};
      
    {ok, StatusCode, _Hdrs, _Body} ->
      ?TRACE("HTTP ~s for ~s",[StatusCode, Url]),
      {error, {status_code, string:to_integer(StatusCode)}, Filename, Ext};

    {error, Reason} ->
      ?TRACE("Error ~p for ~s",[Reason, Url]),
      {error, {other, Reason}, Filename, Ext}
  end.
    

%% ---------------------------------------------------------------------------------------------------------------------
%% Unzip only the data file from a zipped country file, then throw away the ZIP file.
handle_zip_file(Dir, File, _Ext, ZipFileName) ->
  TxtFileName = Dir ++ File ++ ".txt",

  ?TRACE("Unzipping ~s to create ~s",[ZipFileName, TxtFileName]),
  case zip:unzip(ZipFileName, [{file_list, [File ++ ".txt"]}, {cwd, Dir}]) of
    {ok, [TxtFileName]} -> ok;
    {error, Reason}     -> exit({country_zip_file_error, ZipFileName, Reason})
  end,

  %% Delete the ZIP file
  file:delete(ZipFileName).



%% =====================================================================================================================
%%
%%                                               P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Based on the age of the eTag file, check whether or not the text file for this country needs to be updated.
%%
%% If the eTag file is still fresh, only the eTag and internal FCP files should be present in the country directory
%% However, if the eTag file is stale, then:
%% 1) Delete the existing internal FCP file
%% 2) Download a new copy of the country's ZIP file and extract the text file
%% 3) Delete the ZIP file
%% 5) Leave the text file in the country directory for subsequent processing
check_for_update(CountryCode) ->
  case is_stale(?COUNTRY_FILE_ETAG(CountryCode)) of
    true ->
      ?TRACE("Checking if data for country ~s has been updated",[CountryCode]),
      country_manager ! {starting, checking_for_update, CountryCode},
      spawn(?MODULE, http_get_request, [self(), CountryCode, ".zip"]),
      retry(wait_for_resources(1, zip), zip, 1);

    false -> done
  end.


%% ---------------------------------------------------------------------------------------------------------------------
%% Retry download of files that previously failed
retry([], _, _) -> done;

retry(RetryList, _, RetryCount) when RetryCount >= ?RETRY_LIMIT ->
  exit({retry_limit_exceeded, RetryList});

retry(RetryList, Ext, RetryCount) ->
  Parent = self(),
  
  receive
    after ?RETRY_WAIT ->
      lists:foreach(fun({F,Ext1}) ->
        io:format("~w~s retry attempt to download ~s~s~n",[RetryCount, format:as_ordinal(RetryCount), F, Ext1]),
        spawn(?MODULE, http_get_request, [Parent, F, Ext1])
      end,
      RetryList),
    retry(wait_for_resources(length(RetryList), Ext), Ext, RetryCount + 1)
  end.


%% ---------------------------------------------------------------------------------------------------------------------
%% Determine whether or not a local file has gone stale
is_stale(Filename) ->
  case file_age(Filename) of
    N when N > ?STALE_AFTER -> true;
    _                       -> false
  end.

%% ---------------------------------------------------------------------------------------------------------------------
%% Determine age of file in seconds.
%% If the file does not exist, then assume its modification date is midnight on Jan 1st, 1970
file_age(Filename) ->
  {Date,{H,M,S}} = case filelib:last_modified(Filename) of
    0 -> {{1970,1,1}, {0,0,0}};
    T -> T
  end,

  %% Find time difference between the file's last modified time and now
  %% Convert standard DateTime to custom DateTime format by adding microseconds
  time:time_diff(?NOW, {Date,{H,M,S,0}}).

%% ---------------------------------------------------------------------------------------------------------------------
%% Wait for 'Count' HTTP resource to be returned
wait_for_resources(Count, text) -> wait_for_resources(Count, move_file,       []);
wait_for_resources(Count, zip)  -> wait_for_resources(Count, handle_zip_file, []).

wait_for_resources(0,    _Fun, RetryList) -> RetryList;
wait_for_resources(Count, Fun, RetryList) ->
  RetryList1 = receive
    %% New version of the file has been receievd
    {ok, Filename, Ext, Etag, TempFilename} ->
      %% Each country file is written to its own directory
      TargetDir = ?TARGET_DIR ++ Filename ++ "/",
      filelib:ensure_dir(TargetDir),

      ?TRACE("Received ~s with ETag ~s",[Filename ++ Ext, Etag]),
      
      %% If an ETag is included in the HTTP response, then write it to disc
      case Etag of
        missing -> done;
        _       -> write_file(?COUNTRY_FILE_ETAG(Filename), Etag)
      end,

      %% Call the handler for this file type
      ?MODULE:Fun(TargetDir, Filename, Ext, TempFilename),
      RetryList;
    
    %% This file is unchanged since the last refresh
    {not_modified, _Filename, _Ext} ->
      ?TRACE("~s is unchanged since last request", [_Filename ++ _Ext]),
      RetryList;
  
    %% Various error conditions
    {error, SomeReason, Filename, Ext} ->
      case SomeReason of
        {status_code, StatusCode} ->
          {_, Desc} = http_status_code(StatusCode),
          io:format("HTTP ~w \"~s\": ~s~n", [StatusCode, Desc, ?GEONAMES_URL ++ Filename ++ Ext]);

        {other, req_timedout} ->
          io:format("Error: Request timed out for ~s~s~n", [?GEONAMES_URL ++ Filename, Ext]);
      
        {other, {conn_failed, {error, _Reason}}} ->
          io:format("Error: Connection to ~s~s failed.  Host is down or unreachable.~n", [?GEONAMES_URL ++ Filename, Ext]);
      
        {other, Reason} ->
          io:format("Error: ~w requesting ~s~s~n", [Reason, ?GEONAMES_URL ++ Filename, Ext])
      end,

      RetryList ++ [{Filename, Ext}]
  end,

  wait_for_resources(Count-1, Fun, RetryList1).


%% ---------------------------------------------------------------------------------------------------------------------
%% Read the countryInfo.txt file and fetch each individual country file
parse_countries_file(Filename, Ext) ->
  parse_countries_file(file:open(?TARGET_DIR ++ Filename ++ "/" ++ Filename ++ Ext, [read])).
  
% Generate a list of country codes
parse_countries_file({ok, IoDevice})  -> {ok, read_countries_file(IoDevice, [])};
parse_countries_file({error, Reason}) -> {error, Reason}.


%% ---------------------------------------------------------------------------------------------------------------------
%% Read the countries file and create a list of country codes skipping any lines that start with a hash character
read_countries_file(IoDevice, []) ->
  read_countries_file(IoDevice, io:get_line(IoDevice,""), []).

read_countries_file(IoDevice, eof, Acc) ->
  file:close(IoDevice),
  Acc;

read_countries_file(IoDevice, DataLine, Acc) ->
  LineTokens = string:split(DataLine,"\t",all),
  [[Char1 | _] | _] = LineTokens,
  read_countries_file(IoDevice, io:get_line(IoDevice,""), get_country_info(Char1, LineTokens, Acc)).

%% Extract ISO country code, country name and continent code
%% These are the 1st, 5th and 9th columns of countryInfo.txt
get_country_info($#,_Tokens, Acc) -> Acc;
get_country_info(_,  Tokens, Acc) -> lists:append(Acc, [{lists:nth(1,Tokens)
                                                       , lists:nth(5,Tokens)
                                                       , lists:nth(9,Tokens)}]).


%% ---------------------------------------------------------------------------------------------------------------------
%% Read local eTag file
read_etag_file({ok, IoDevice}) -> read_etag_file(IoDevice, io:get_line(IoDevice,""), "");
read_etag_file({error, _})     -> missing;
read_etag_file(Filename)       -> read_etag_file(file:open(lists:append([?COUNTRY_FILE_ETAG(Filename)]), [read])).

read_etag_file(IoDevice, eof, "")   -> file:close(IoDevice), missing;
read_etag_file(IoDevice, eof, Etag) -> file:close(IoDevice), Etag;
read_etag_file(IoDevice, Etag, "")  -> file:close(IoDevice), Etag.

%% ---------------------------------------------------------------------------------------------------------------------
%% Extract eTag from HTTP headers
get_etag([])                    -> missing;
get_etag([{"ETag", Etag} | _])  -> Etag;
get_etag([{"etag", Etag} | _])  -> Etag;
get_etag([{_Hdr, _Val} | Rest]) -> get_etag(Rest).


%% ---------------------------------------------------------------------------------------------------------------------
%% Write file to disc
write_file(FQFilename, Content) ->
  case file:write_file(FQFilename, Content) of
    ok              -> ok;
    {error, Reason} -> io:format("Writing file ~s failed. ~p~n",[FQFilename, Reason])
  end.


%% ---------------------------------------------------------------------------------------------------------------------
%% Move file
move_file(Dir, Filename, Ext, From) ->
  To = Dir ++ Filename ++ Ext,

  case file:copy(From, To) of
    {ok, _BytesCopied} ->
      file:delete(From),
      ok;

    {error, Reason} ->
      io:format("Copying file from ~s to ~s failed: ~p~n",[From, To, Reason])
  end.

%% ---------------------------------------------------------------------------------------------------------------------
%% read_country_file_int/3
%%
%% The full country text file is being read
read_country_file_int(CC, {ok, IoDevice}, Filesize) ->
  read_country_file_int(CC, IoDevice, {[],[]}, Filesize, trunc(Filesize * ?PROGRESS_FRACTION));

%% Some error occurred trying to read the full country text file
read_country_file_int(CC, {error, Reason}, _) ->
  {error, io_lib:format("File ~s~s.txt: ~p",[?TARGET_DIR, CC, Reason])};

%% This country's internal FCP file is missing (I.E. has zero size), so open the country's text file and generate a new
%% internal FCP file
read_country_file_int(CC, 0, CountryServerPid) when is_pid(CountryServerPid) ->
  Filesize = filelib:file_size(?COUNTRY_FILE_FULL(CC)),
  ?TRACE("Internal FCP file does not exist. Importing ~s from full country file ~s", [format:as_binary_units(Filesize), ?COUNTRY_FILE_FULL(CC)]),
  CountryServerPid ! read_country_file_int(CC, file:open(?COUNTRY_FILE_FULL(CC), [read]), Filesize);

%% Import the internal <CC>_fcp.txt file
read_country_file_int(CC, FCP_Filesize, CountryServerPid) when is_pid(CountryServerPid) ->
  ?TRACE("Importing ~s from internal FCP country file ~s",[format:as_binary_units(FCP_Filesize), ?COUNTRY_FILE_FCP(CC)]),
  {ok, [FCP_Data | _]} = file:consult(?COUNTRY_FILE_FCP(CC)),
  CountryServerPid ! FCP_Data.


%% ---------------------------------------------------------------------------------------------------------------------
%% read_country_file_int/5
%%
%% Read a country text file and create a list of geoname records
read_country_file_int(CC, IoDevice, ListPair, Filesize, Stepsize) ->
  read_country_file_int(CC, IoDevice, io:get_line(IoDevice,""), ListPair, Filesize, Stepsize, 0).
  
%% ---------------------------------------------------------------------------------------------------------------------
%% read_country_file_int/7
%%
%% Reached EOF, so close and delete the input text file, supplement the FCP records with FCA region data, then dump the
%% FCP records to disc
read_country_file_int(CC, IoDevice, eof, {FeatureClassA, FeatureClassP}, _, _, _) ->
  %% Close and delete this country's text file as it is no longer needed
  file:close(IoDevice),
  file:delete(?COUNTRY_FILE_FULL(CC)),

  %% Now that we have a list of FCA and FCP records, start the country hierarchy server in order to supplement the FCP
  %% data with admin text from the FCA records.
  %% The hirerarchy server is only needed whilst the FCP file is being created; after that, both the hiererachy_server
  %% and the FCA data can be deleted
  HierarchyServer = list_to_atom("country_hierarchy_" ++ string:lowercase(CC)),
  country_hierarchy:init(HierarchyServer, FeatureClassA),

  %% Remember the hierarchy server's pid
  put(hierarchy_server_pid, whereis(HierarchyServer)),

  FeatureClassP1 = supplement_fcp_admin_text(HierarchyServer, FeatureClassP),

  %% Stop the hierarchy server because it is no longer needed
  HierarchyServer ! {cmd, stop},

  %% Since the feature code lists are Erlang terms, they must always be written to file with a terminating "."
  %% otherwise, when being read back in again, they will generate a syntax error in the term parser
  file:write_file(?COUNTRY_FILE_FCP(CC), io_lib:format("~p.",[FeatureClassP1])),

  country_manager ! {starting, file_import, get(my_name), progress, complete},
  FeatureClassP1;

%% Read country file when not eof
read_country_file_int(CC, IoDevice, DataLine, {FeatureClassA, FeatureClassP}, Filesize, Stepsize, Progress) ->
  {FCA_Filesize, Progress1} = report_progress(Filesize, Stepsize, length(DataLine), Progress),
  Rec = make_geoname_record(string:split(DataLine,"\t"), 1, #geoname_int{}),

  %% Do we want to keep this record?
  ListPair = case keep_geoname_record(Rec) of
    false     -> {FeatureClassA,          FeatureClassP};
    {true, a} -> {FeatureClassA ++ [Rec], FeatureClassP};
    {true, p} -> {FeatureClassA,          FeatureClassP ++ [Rec]}
  end,

  read_country_file_int(CC, IoDevice, io:get_line(IoDevice,""), ListPair, FCA_Filesize, Stepsize, Progress1).


%% ---------------------------------------------------------------------------------------------------------------------
%% Send a message to the country_manager for each unit of progress
report_progress(Filesize, Stepsize, Linesize, Progress) ->
  Chunk = Linesize + Progress,

  % Have we read enough data to report more progress?
  case (Chunk div Stepsize) > 0 of
    true -> country_manager ! {starting, file_import, get(my_name), progress, (Chunk div Stepsize)};
    _    -> ok
  end,

  {Filesize - Linesize, Chunk rem Stepsize}.
  


%% ---------------------------------------------------------------------------------------------------------------------
%% Transform one line from a country file into a geoname record
%% Various fields are skipped to minimise the record size
make_geoname_record([[]], _, Acc) -> Acc;

%% #geoname.id             => #geoname_int.id
%% #geoname.name           => #geoname_int.name
%% #geoname.asciiname      => Don't care
%% #geoname.alternatenames => Don't care
%% #geoname.latitude       => #geoname_int.latitude
%% #geoname.longitude      => #geoname_int.longitude
%% #geoname.feature_class  => #geoname_int.feature_class
%% #geoname.feature_code   => #geoname_int.feature_code
%% #geoname.country_code   => #geoname_int.country_code
%% #geoname.cc2            => Don't care
%% #geoname.admin1         => #geoname_int.admin1
%% #geoname.admin2         => #geoname_int.admin2
%% #geoname.admin3         => #geoname_int.admin3
%% #geoname.admin4         => #geoname_int.admin4
%% #geoname.population     => #geoname_int.population
%% #geoname.elevation      => Don't care
%% #geoname.dem            => Don't care
%% #geoname.timezone       => #geoname_int.timezone
%% #geoname.modified       => Don't care

make_geoname_record([V  | Rest],  1, Acc) -> make_geoname_record(string:split(Rest,"\t"),  2, Acc#geoname_int{id   = bin_or_undef(V)});
make_geoname_record([V  | Rest],  2, Acc) -> make_geoname_record(string:split(Rest,"\t"),  3, Acc#geoname_int{name = bin_or_undef(V)});
make_geoname_record([_V | Rest],  3, Acc) -> make_geoname_record(string:split(Rest,"\t"),  4, Acc);
make_geoname_record([_V | Rest],  4, Acc) -> make_geoname_record(string:split(Rest,"\t"),  5, Acc);
make_geoname_record([V  | Rest],  5, Acc) -> make_geoname_record(string:split(Rest,"\t"),  6, Acc#geoname_int{latitude       = bin_or_undef(V)});
make_geoname_record([V  | Rest],  6, Acc) -> make_geoname_record(string:split(Rest,"\t"),  7, Acc#geoname_int{longitude      = bin_or_undef(V)});
make_geoname_record([V  | Rest],  7, Acc) -> make_geoname_record(string:split(Rest,"\t"),  8, Acc#geoname_int{feature_class  = bin_or_undef(V)});
make_geoname_record([V  | Rest],  8, Acc) -> make_geoname_record(string:split(Rest,"\t"),  9, Acc#geoname_int{feature_code   = bin_or_undef(V)});
make_geoname_record([V  | Rest],  9, Acc) -> make_geoname_record(string:split(Rest,"\t"), 10, Acc#geoname_int{country_code   = bin_or_undef(V)});
make_geoname_record([_V | Rest], 10, Acc) -> make_geoname_record(string:split(Rest,"\t"), 11, Acc);
make_geoname_record([V  | Rest], 11, Acc) -> make_geoname_record(string:split(Rest,"\t"), 12, Acc#geoname_int{admin1         = bin_or_undef(V)});
make_geoname_record([V  | Rest], 12, Acc) -> make_geoname_record(string:split(Rest,"\t"), 13, Acc#geoname_int{admin2         = bin_or_undef(V)});
make_geoname_record([V  | Rest], 13, Acc) -> make_geoname_record(string:split(Rest,"\t"), 14, Acc#geoname_int{admin3         = bin_or_undef(V)});
make_geoname_record([V  | Rest], 14, Acc) -> make_geoname_record(string:split(Rest,"\t"), 15, Acc#geoname_int{admin4         = bin_or_undef(V)});
make_geoname_record([V  | Rest], 15, Acc) -> make_geoname_record(string:split(Rest,"\t"), 16, Acc#geoname_int{population     = bin_or_undef(V)});
make_geoname_record([_V | Rest], 16, Acc) -> make_geoname_record(string:split(Rest,"\t"), 17, Acc);
make_geoname_record([_V | Rest], 17, Acc) -> make_geoname_record(string:split(Rest,"\t"), 18, Acc);
make_geoname_record([V  | Rest], 18, Acc) -> make_geoname_record(string:split(Rest,"\t"), 19, Acc#geoname_int{timezone       = bin_or_undef(V)});
make_geoname_record([_V | Rest], 19, Acc) -> make_geoname_record(string:split(Rest,"\t"),  0, Acc).

%% ---------------------------------------------------------------------------------------------------------------------
%% Transform string data to binary for use in the geoname_int records
bin_or_undef([]) -> undefined;
bin_or_undef(V)  -> list_to_binary(V).


%% ---------------------------------------------------------------------------------------------------------------------
%% Filter out geoname records that not related to countries or administrative areas
keep_geoname_record(Rec) ->
  keep_feature_codes_for_class(Rec#geoname_int.feature_class, Rec, binary_to_integer(Rec#geoname_int.population)).


%% ---------------------------------------------------------------------------------------------------------------------
%% Administrative areas
keep_feature_codes_for_class(<<"A">>, Rec, _Pop) ->
  case Rec#geoname_int.feature_code of
    <<"ADM1">>  -> {true, a};
    <<"ADM2">>  -> {true, a};
    <<"ADM3">>  -> {true, a};
    <<"ADM4">>  -> {true, a};
    <<"ADM5">>  -> {true, a};
    <<"ADMD">>  -> {true, a};
    <<"PCL">>   -> {true, a};
    <<"PCLD">>  -> {true, a};
    <<"PCLF">>  -> {true, a};
    <<"PCLI">>  -> {true, a};
    <<"PCLS">>  -> {true, a};
    _           -> false
  end;

%% Only keep population centres having a population greater than the limit defined in ?MIN_POPULATION
%% For smaller countries, the situation might exist in which the administrative centres have a population above the
%% threshold, but all the individual population centres within it are below the threshold.  This will result in zero
%% population centres being extracted
keep_feature_codes_for_class(<<"P">>, Rec, Pop) when Pop >= ?MIN_POPULATION ->
  case Rec#geoname_int.feature_code of
    <<"PPL">>   -> {true, p};
    <<"PPLA">>  -> {true, p};
    <<"PPLA2">> -> {true, p};
    <<"PPLA3">> -> {true, p};
    <<"PPLA4">> -> {true, p};
    <<"PPLC">>  -> {true, p};
    <<"PPLG">>  -> {true, p};
    <<"PPLS">>  -> {true, p};
    <<"PPLX">>  -> {true, p};
    _           -> false
  end;

keep_feature_codes_for_class(_, _, _) -> false.


%% ---------------------------------------------------------------------------------------------------------------------
%% Supplement FCP records with additional admin text
supplement_fcp_admin_text(HierarchyServer, FCP) ->
  [ HierarchyServer ! {name_lookup, FCPRec, self()} || FCPRec <- FCP ],
  wait_for_results(length(FCP), []).



%% ---------------------------------------------------------------------------------------------------------------------
%% Wait for responses from country hierarchy server
wait_for_results(0, Acc) -> Acc;
wait_for_results(N, Acc) ->
  Acc1 = Acc ++ receive
FCPRec when is_record(FCPRec, geoname_int) -> [FCPRec];
_Whatever                                  -> []
end,

wait_for_results(N-1, Acc1).


%% ---------------------------------------------------------------------------------------------------------------------
%% Define HTTP options
define_http_options(CallerPid) ->
  %% Ensure HTTP response is written directly to file
  Options1 = [{save_response_to_file, true}],

  %% Get the proxy information from the process dictionary of the calling process
  %%
  %% The proxy host value must be a tuple of {proxy_host, HostName}
  %% The proxy port value must be a tuple of {proxy_port, PortNumber}
  Options2 = Options1 ++
    case process_tools:read_process_dictionary(CallerPid, proxy_host) of
      undefined -> [];

      {proxy_host, ProxyHost} ->
        case ProxyHost of
          undefined -> [];
          _         -> [{proxy_host, ProxyHost}]
        end
    end,

  Options3 = Options2 ++
    case process_tools:read_process_dictionary(CallerPid, proxy_port) of
      undefined -> [];

      {proxy_port, ProxyPort} ->
        case ProxyPort of
          undefined -> [];
          _         -> [{proxy_port, ProxyPort}]
        end
    end,

  Options3.
