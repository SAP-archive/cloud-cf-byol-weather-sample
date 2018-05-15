-module(geo_server_app).
-behaviour(application).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/02/02 13:15:19").
-created_by("chris.whealy@sap.com").

-export([
    start/2
	, stop/1
]).

-define(DEFAULT_HTTP_PORT, 8080).

-include("../include/macros/file_paths.hrl").
-include("../include/macros/trace.hrl").

%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Start the geo_server application
start(_Type, _Args) ->
  process_flag(trap_exit, true),

  %% Keep trace on in order to log server startup
  put(trace, true),

  ?TRACE("Application start. Type = ~p, Args =~p",[_Type, _Args]),
  
	%% Get port number from the environment for incoming HTTP requests
  Port = case os:getenv("PORT") of
    false ->
      ?TRACE("Environment variable PORT not set.  Defaulting to ~w",[?DEFAULT_HTTP_PORT]),
      ?DEFAULT_HTTP_PORT;

    P ->
      {Int,_} = string:to_integer(P),
      Int
  end,

  %% Determine if external communication needs to take place through a proxy server
  ProxyInfo = get_proxy_info(),
  ?TRACE("Proxy information = ~p",[ProxyInfo]),

  %% Start iBrowse and set connection parameters
  ?TRACE("Starting iBrowse"),
  ibrowse:start(),
  ibrowse:set_dest(?GEONAMES_HOST, ?GEONAMES_PORT, []),
  ibrowse:set_max_sessions(?GEONAMES_HOST, ?GEONAMES_PORT, 10),
  ibrowse:set_max_pipeline_size(?GEONAMES_HOST, ?GEONAMES_PORT, 25),

  %% Don't switch iBrowse trace on because it swamps the log files...
  % case get(trace) of
  %   true -> ibrowse:trace_on(?GEONAMES_HOST, ?GEONAMES_PORT);
  %   _    -> ok
  % end,

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	%% Get countryInfo.txt from GeoNames.org and parse it to create a list of {country_code, country_name, continent_code}
  ?TRACE("Importing country information"),
  spawn_link(import_files, import_country_info, [self(), ProxyInfo]),

  Countries =
		receive
      {'EXIT', ChildPid, Reason} ->
        case Reason of
          {retry_limit_exceeded, RetryList} ->
            io:format("Retry limit exceeded downloading ~p~n",[RetryList]);

          {parse_error, Reason} ->
            io:format("Error ~p parsing countryInfo.txt~n",[Reason]);

          _ ->
            io:format("Error ~p received from child proces ~p~n",[Reason, ChildPid])
        end,
        
        exit({error, Reason});

			{country_list, L} -> L
    end,

  %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  %% Define routes
  Dispatch = cowboy_router:compile([
		{'_', [
      %% Browser paths
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
		]}
	]),

  ?TRACE("Starting Cowboy server on port ~w",[Port]),
  cowboy:start_clear(my_http_listener, [{port, Port}], #{env => #{dispatch => Dispatch}}),

  ?TRACE("Starting supervisor"),
	geo_server_sup:start(Countries, ProxyInfo).



%% ---------------------------------------------------------------------------------------------------------------------
%% Stop the geo_server application
stop(_State) -> geo_server_sup:stop(_State).



%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% If available, obtain HTTP proxy information from the environment.
%% Returns a tuple of {Host, Port} or {undefined, undefined}
get_proxy_info() ->
  Proxy1 = os:getenv("HTTP_PROXY"),
  Proxy2 = os:getenv("http_proxy"),
  
  %% Has the uppercase version of this variable been set?
  {Host, Port} = case Proxy1 of
    false ->
      %% Nope, so check for the lowercase version of the same variable
      case Proxy2 of
        %% Nope, so we assume that no proxy has been set
        false ->
          {undefined, undefined};
          
          %% Parse lowercase proxy string to find host and port number
        _ ->
          split_proxy_str(Proxy2)
      end;
      
      %% Parse uppercase proxy string to find host and post number
    _ ->
      split_proxy_str(Proxy1)
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
    [] -> 8080;
    _  -> list_to_integer(hd(PortStr))
  end,

  {Host, Port}.

    
