-module(geo_server_app).
-behaviour(application).

-include("../include/macros/revision.hrl").
-revision(?REVISION).

-author("Chris Whealy <chris.whealy@sap.com>").
-created("Date: 2018/02/02 13:15:19").
-created_by("chris.whealy@sap.com").

-export([
    start/2
	, stop/1
]).

%% Macros
-include("../include/macros/file_paths.hrl").
-include("../include/macros/trace.hrl").

-define(DEFAULT_HTTP_PORT,  8080).
-define(DEFAULT_PROXY_PORT, 8080).

%% Limit the number of parallel HTTP connections used by iBrowse to 10, and the number of queued requests to 25
-define(HTTP_SESSIONS, 10).
-define(HTTP_PIPELINE, 25).

%% Define Cowboy routes and their respective handlers
-define(ROUTE_DEFINITIONS, [
  %% Browser API
   {"/",                    handle_root,                []}
  ,{"/server_status",       handle_server_status,       []}
  ,{"/client_info",         handle_client_info,         []}
  ,{"/search",              handle_search,              []}

  %% Command API
  ,{"/country_manager_cmd", handle_country_manager_cmd, []}
  ,{"/country_server_cmd",  handle_country_server_cmd,  []}

  %% Handle static files
  ,{"/server_info",         cowboy_static, {priv_file, geo_server, "html/server_info.html"}}
  ,{"/js/server_info.js",   cowboy_static, {priv_file, geo_server, "js/server_info.js"}}
  ,{"/css/server_info.css", cowboy_static, {priv_file, geo_server, "css/server_info.css"}}
  ,{"/img/[...]",           cowboy_static, {priv_dir,  geo_server, "img"}}
]).

%% MongoDB instance name
-define(MONGODB_NAME, <<"geo_server_db">>).


%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Start the geo_server application
start(_Type, _Args) ->
  process_flag(trap_exit, true),

  %%?LOG("Application start. Type = ~p, Args =~p",[_Type, _Args]),

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  %% Determine if external communication needs to take place through a proxy server
  ProxyInfo = get_proxy_info(),
  ?LOG("Proxy information = ~p",[ProxyInfo]),

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  %% Start iBrowse and set connection parameters
  ?LOG("Starting iBrowse"),
  ?LOG("Configuring iBrowse:"),
  ?LOG("  Destination                    = ~s:~w",[?GEONAMES_HOST, ?GEONAMES_PORT]),
  ?LOG("  Parallel HTTP connection limit = ~w",[?HTTP_SESSIONS]),
  ?LOG("  HTTP request queue length      = ~w",[?HTTP_PIPELINE]),
  
  ibrowse:start(),
  ibrowse:set_dest(?GEONAMES_HOST, ?GEONAMES_PORT, []),
  ibrowse:set_max_sessions(?GEONAMES_HOST, ?GEONAMES_PORT, ?HTTP_SESSIONS),
  ibrowse:set_max_pipeline_size(?GEONAMES_HOST, ?GEONAMES_PORT, ?HTTP_PIPELINE),

  %% Don't switch iBrowse trace on because it swamps the log files...
  % case get(trace) of
  %   true -> ibrowse:trace_on(?GEONAMES_HOST, ?GEONAMES_PORT);
  %   _    -> ok
  % end,

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	%% Get countryInfo.txt from GeoNames.org and parse it to create a list of {country_code, country_name, continent_code}
  ?LOG("Importing countryInfo.txt from GeoNames.org"),
  spawn_link(import_files, import_country_info, [self(), ProxyInfo]),

  Countries = wait_for_countries(),
  Port      = get_port_info(),

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  %% Start Cowboy webserver
  ?LOG("Starting Cowboy server on port ~w",[Port]),
  cowboy:start_clear(
    my_http_listener
  , [{port, Port}]
  , #{env => #{dispatch => cowboy_router:compile([{'_', ?ROUTE_DEFINITIONS}])}}
  ),

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  %% Connect to MongoDB instance
  % ?LOG("Connecting to MongoDB"),

  % case mc_worker_api:connect([{database, ?MONGODB_NAME}]) of
  %   {error, Reason} ->
  %     ?LOG("Unable to connect to MongoDB instance: ~p", [Reason]);

  %   {ok, SomePid} ->
  %     ?LOG("Connected to MongoDB via pid ~p",[SomePid]),
  %     put(mongo_pid, SomePid)
  % end,

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  %% Start geo_server supervisor
  ?LOG("Starting geo_server supervisor"),
	geo_server_sup:start(Countries, ProxyInfo, get(mongo_pid)).



%% ---------------------------------------------------------------------------------------------------------------------
%% Stop the geo_server application
stop(_State) ->
  ?LOG("geo_server application shutting down"),

  %% Disconnect from MongoDB instance
  case get(mongo_pid) of
    undefined ->
      ok;

    SomePid ->
      mc_worker_api:disconnect(SomePid),
      ?LOG("Disconnected from MongoDB")
  end,

  geo_server_sup:stop(_State).



%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% If available, obtain HTTP proxy information from the environment.
%% Returns a tuple of {Host, Port} or {undefined, undefined}
get_proxy_info() ->
  ProxyUpper = os:getenv("HTTP_PROXY"),
  ProxyLower = os:getenv("http_proxy"),
  
  %% Has the uppercase version of this variable been set?
  {Host, Port} = case ProxyUpper of
    false ->
      %% Nope, so check for the lowercase version of the same variable
      case ProxyLower of
        %% Nope, so we assume that no proxy has been set
        false ->
          {undefined, undefined};
          
        %% Parse lowercase proxy string to find host and port number
        _ ->
          split_proxy_str(ProxyLower)
      end;
      
      %% Parse uppercase proxy string to find host and post number
    _ ->
      split_proxy_str(ProxyUpper)
  end,
  
  {{proxy_host, Host}, {proxy_port, Port}}.

%% ---------------------------------------------------------------------------------------------------------------------
split_proxy_str(ProxyStr) ->
  Parts = string:split(ProxyStr, ":", all),

  {Host, PortStr} = case length(Parts) of
    3 ->
      [_Prot, Hst | Prt ] = Parts,
      {_, Hst1} = lists:split(2,Hst),
      {Hst1, Prt};

    2 ->
      [Hst | Prt] = Parts,
      {Hst, Prt}
  end,
  
  Port = case PortStr of
    [] -> ?DEFAULT_PROXY_PORT;
    _  -> list_to_integer(hd(PortStr))
  end,

  {Host, Port}.


%% ---------------------------------------------------------------------------------------------------------------------
%% Get port number from the environment for incoming HTTP requests
get_port_info() ->
  case os:getenv("PORT") of
    false ->
      ?LOG("Environment variable PORT not set.  Defaulting to ~w",[?DEFAULT_HTTP_PORT]),
      ?DEFAULT_HTTP_PORT;

    P ->
      {Int,_} = string:to_integer(P),
      Int
  end.


%% ---------------------------------------------------------------------------------------------------------------------
wait_for_countries() ->
  receive
    {'EXIT', ChildPid, Reason} ->
      case Reason of
        {retry_limit_exceeded, RetryList} -> ?LOG("Retry limit exceeded downloading ~p",[RetryList]);
        {parse_error, Reason1}            -> ?LOG("Error ~p parsing countryInfo.txt",[Reason1]);
        _                                 -> ?LOG("Error ~p received from child process ~p",[Reason, ChildPid])
      end,

      exit({error, Reason});

    {country_list, L} -> L
  end.

