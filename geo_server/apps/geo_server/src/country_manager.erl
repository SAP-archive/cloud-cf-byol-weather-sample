-module(country_manager).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/03/02 09:22:03").
-created_by("chris.whealy@sap.com").

-export([
    init/2
  , start/2
]).

%% Put -defines before -includes because COUNTRY_SERVER_NAME is needed in manage_server_status.hrl
-define(COUNTRY_SERVER_NAME(CountryCode), list_to_atom("country_server_" ++ string:lowercase(CountryCode))).
-define(RETRY_LIMIT, 3).
-define(RETRY_WAIT,  5000).

%% Record definitions
-include("../include/records/cmd_response.hrl").
-include("../include/records/country_server.hrl").

%% Macro definitions
-include("../include/macros/trace.hrl").
-include("../include/macros/now.hrl").

%% Utilities
-include("../include/utils/server_status.hrl").


%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Initialise the country manager
init(CountryList, ProxyInfo) ->
  %% Is the country_manager process already registered?
  case whereis(?MODULE) of
    undefined -> register(?MODULE, spawn(?MODULE, start, [CountryList, ProxyInfo]));
    _         -> already_registered
end,

{ok, whereis(?MODULE), []}.


%% ---------------------------------------------------------------------------------------------------------------------
%% Start the country manager
%%
%% This process is responsible for starting and then managing each of the individual country servers
start(CountryList, {ProxyHost, ProxyPort}) ->
  process_flag(trap_exit, true),

  %% Keep the debug trace flag switched off by default.  Can be switched on from the admin screen
  put(trace, false),

  %% Store the proxy information in the process dictionary
  put(proxy_host, ProxyHost),
  put(proxy_port, ProxyPort),

  %% The default sort order is by ascending continent name
  wait_for_msgs(lists:sort(
    fun(A,B) ->
      %% Swap parameters around for sort ascending
      sort_servers_by(continent, B, A)
    end,
    initialise_country_server_list(CountryList))
  ).



%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Country manager receive loop
wait_for_msgs(CountryServerList) ->
  %% The CountryServerList could be emmpty for two reasons.  Either:
  %% - none of the country servers have started yet, or
  %% - the whole country_manager is shutting down
  case CountryServerList of
    [] ->
      %% Are we shutting down?
      case get(shutdown) of
        true -> exit(normal);
        _    -> ok
      end;

    _ -> ok
  end,

  ServerStatusList1 = receive
    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Messages from processes that have crashed
    %%
    {'EXIT', CountryServerPid, Reason} ->
      {NameOrPid, Status, Substatus} = case Reason of
        {stopped, DeadServerName} ->
          io:format("Country server ~p was stopped~n", [DeadServerName]),
          {DeadServerName, stopped, undefined};

        {no_cities, DeadServerName} ->
          io:format("Country server ~p terminated: no_cities~n", [DeadServerName]),
          {DeadServerName, stopped, no_cities};

        {country_file_error, Reason} ->
          io:format("Error reading country file ~s~n", [Reason]),
          {CountryServerPid, crashed, country_file_error};

        {fcp_country_file_error, Reason} ->
          io:format("Error reading internal FCP file. ~p~n", [Reason]),
          {CountryServerPid, crashed, fcp_country_file_error};

        {country_zip_file_error, ZipFile, Reason} ->
          io:format("Error unzipping file ~s: ~p~n", [ZipFile, Reason]),
          {CountryServerPid, crashed, country_zip_file_error};

        {retry_limit_exceeded, {CC, Ext}} ->
          io:format("Retry limit exceeded attempting to download ~s~s~n", [CC, Ext]),
          {CountryServerPid, crashed, retry_limit_exceeded};

        {error, SomeReason} ->
          DeadServerName = get_server_name_from_pid(CountryServerPid, CountryServerList),
          io:format("Country server ~p (~p) terminated for reason '~p'~n", [DeadServerName, CountryServerPid, SomeReason]),
          {CountryServerPid, crashed, SomeReason}
      end,
      
      set_server_status(CountryServerList, NameOrPid, Status, Substatus, 0, [], undefined);


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Messages from country servers reporting the how their startup sequence is progressing
    %%
    %% Initialisation
    {starting, checking_for_update, CountryCode} ->
      set_server_status(CountryServerList, ?COUNTRY_SERVER_NAME(CountryCode), starting, checking_for_update, 0, [], undefined);
    
    {starting, country_file_download, CountryCode} ->
      set_server_status(CountryServerList, ?COUNTRY_SERVER_NAME(CountryCode), starting, country_file_download, 0, [], undefined);
    
    {starting, Substatus, CountryServer} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, complete, [], undefined);

    {starting, init, CountryServer, StartTime} ->
      set_server_status(CountryServerList, CountryServer, starting, init, init, [], StartTime);

    {starting, Substatus, CountryServer, Id} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, complete, Id, undefined);
  
    {starting, Substatus, CountryServer, progress, Progress} ->
      set_server_status(CountryServerList, CountryServer, starting, Substatus, Progress, [], undefined);


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Sort messages
    %%
    {sort, Dir, ColName, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("Sorting server status records by ~p ~p",[ColName, Dir]),
      CountryServerList1 = lists:sort(fun(A,B) ->
          case Dir of
            ascending -> sort_servers_by(ColName, B, A);
            _         -> sort_servers_by(ColName, A, B)
          end
        end,
        CountryServerList),
      RequestHandlerPid ! CountryServerList1,
      CountryServerList1;


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Start up complete, server now running
    %%
    {started, running, CountryServer, CityCount, StartupComplete} ->
      ?TRACE("Country server ~p is up and running",[CountryServer]),
      set_server_running(CountryServerList, CountryServer, CityCount, StartupComplete);


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% STATUS commands
    %%
    %% Status commmands from some request handler
    {cmd, status, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("Server status requested from request handler ~p",[RequestHandlerPid]),
      RequestHandlerPid ! {country_server_list, CountryServerList, trace_on, get(trace)},
      CountryServerList;

    {cmd, status, started, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("List of started servers requested by ~p",[RequestHandlerPid]),
      RequestHandlerPid ! {started_servers, started_servers(CountryServerList)},
      CountryServerList;


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% SHUTDOWN messages
    %%
    %% Shutdown all country servers, but keep the country manager up
    {cmd, shutdown_all, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("Shutdown all country servers but keep country_manager running"),
      put(shutdown, false),
      CountryServerList1 = stop_all_country_servers(CountryServerList),
      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = shutdown_all, status = ok, payload = CountryServerList1},
      CountryServerList1;

    %% Shutdown all country servers and then shutdown the country manager
    {cmd, terminate, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      %% Set process dictionary flag to indicate country_manager shutdown
      put(shutdown, true),
      ?TRACE("Shutdown all country servers then shutdown the country manager"),
      CountryServerList1 = stop_all_country_servers(CountryServerList),
      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = terminate, status = goodbye},
      CountryServerList1;

    %% Shutdown a specific country server
    {cmd, shutdown, CountryCode, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServer = ?COUNTRY_SERVER_NAME(CountryCode),
      ?TRACE("Shutdown country server ~p",[CountryServer]),

      case whereis(CountryServer) of
        undefined ->
          io:format("~p not started~n",[CountryServer]),
          CountryServerList;

        _Pid ->
          CountryServer ! {cmd, shutdown},
          StoppedSvr = set_server_stopped(lists:keyfind(CountryServer, #country_server.name, CountryServerList)),
          RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = shutdown, status = ok, payload = StoppedSvr},
          lists:keyreplace(CountryServer, #country_server.name, CountryServerList, StoppedSvr)
      end;


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% START messages
    %%
    %% (Re)start a specific country server
    {cmd, start, CountryCode, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServer = ?COUNTRY_SERVER_NAME(CountryCode),

      {CountryServerList1, ResponseRec} = case whereis(CountryServer) of
        undefined ->
          T = lists:keyfind(CountryServer, #country_server.name, CountryServerList),

          %% Did the server lookup work?
          case T of
            false ->
              io:format("Error: Lookup of ~p in CountryServerList failed~n",[CountryServer]),
              {CountryServerList,
               #cmd_response{from_server = country_manager, cmd = start, status = error, reason = country_server_not_found}};
            _ ->
              T1 = start_country_server(T),
              {lists:keyreplace(CountryServer, #country_server.name, CountryServerList, T1),
               #cmd_response{from_server = country_manager, cmd = start, status = ok, payload = T1}}
          end;

        _Pid ->
          {CountryServerList,
           #cmd_response{from_server = country_manager, cmd = start, status = error, reason = already_started}}
      end,

      RequestHandlerPid ! ResponseRec,
      CountryServerList1;

    %% Start all the country servers at once
    {cmd, start_all, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      ?TRACE("Starting all country servers"),
      CountryServerList1 = start_all_country_servers(CountryServerList),
      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = start_all, status = ok, payload = CountryServerList1},
      CountryServerList1;


    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Reset a single country server that has crashed
    %%
    {cmd, reset, CC, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServerName = ?COUNTRY_SERVER_NAME(CC),
      S = lists:keyfind(CountryServerName, #country_server.name, CountryServerList),

      case S#country_server.status of
        crashed ->
          S1 = reset_crashed_server(S),
          RequestHandlerPid ! #cmd_response{from_server = CountryServerName, cmd = reset, status = ok, payload = S1},
          lists:keyreplace(CountryServerName, #country_server.name, CountryServerList, S1);

        _ ->
          RequestHandlerPid ! #cmd_response{from_server = CountryServerName, cmd = reset, status = error, reason = server_not_crashed},
          CountryServerList
      end;

  

    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Reset all crashed country servers
    %%
    {cmd, reset_all, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServerList1 = [
        (fun(S) ->
          case S#country_server.status of
            crashed -> reset_crashed_server(S);
            _       -> S
          end
        end)(Svr)
        || Svr <- CountryServerList
        ],
      
      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = reset_all, status = ok, payload = CountryServerList1},
      CountryServerList1;

  

    %% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    %% Debug trace on/off commands
    %%
    %% Turn trace on/off for the country manager
    {cmd, trace, TraceState, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      case TraceState of
        on  -> put(trace, true);
        off -> put(trace, false)
      end,

      RequestHandlerPid ! #cmd_response{from_server = country_manager, cmd = trace, status = ok, payload = TraceState},
      CountryServerList;

    %% Turn trace on/off for an individual country server
    %% The value bound to Trace must be either of the atoms 'trace_on' or 'trace_off'
    {cmd, Trace, CC, RequestHandlerPid} when is_pid(RequestHandlerPid) ->
      CountryServerName = ?COUNTRY_SERVER_NAME(CC),
      S = lists:keyfind(CountryServerName, #country_server.name, CountryServerList),

      {Status, Reason} = case S of
        false -> {error, no_such_country_server};
        _     -> case S#country_server.substatus of
                   running -> S#country_server.name ! {cmd, Trace};
                   _       -> ok
                 end,

                 {ok, undefined}
      end,

      RequestHandlerPid ! #cmd_response{from_server = CountryServerName, cmd = Trace, status = Status, reason = Reason},

      %% Only update the CountryServerList if the trace state has changed
      case S#country_server.trace == trace_state_to_boolean(Trace) of
        true  -> CountryServerList;
        false -> lists:keyreplace(CountryServerName, #country_server.name, CountryServerList, update_trace(S, Trace))
      end
  end,

  wait_for_msgs(ServerStatusList1).



%% ---------------------------------------------------------------------------------------------------------------------
%% List servers by status
started_servers(CountryServerList) -> filter_by_status(CountryServerList, started).

%% ---------------------------------------------------------------------------------------------------------------------
%% Filter server list by status
filter_by_status(CountryServerList, Status) ->
  filter_by_status(CountryServerList, Status, []).

filter_by_status([], _Status, Acc)        -> Acc;
filter_by_status([S | Rest], Status, Acc) ->
  filter_by_status(Rest, Status, Acc ++ case S#country_server.status of Status -> [S]; _ -> [] end).
  
%% ---------------------------------------------------------------------------------------------------------------------
%% Get country server name from pid.  Returns an atom
get_server_name_from_pid(CountryServerPid, CountryServerList) ->
  case lists:keyfind(CountryServerPid, #country_server.pid, CountryServerList) of
    false -> unknown_pid;
    Rec   -> Rec#country_server.name
end.


%% ---------------------------------------------------------------------------------------------------------------------
%% Convert the initial list of countries into a list of initialised country_server records with status stopped
initialise_country_server_list(CountryList) ->
  [ set_server_init(CC, Name, Cont) || {CC, Name, Cont} <- CountryList ].

%% ---------------------------------------------------------------------------------------------------------------------
%% Start all country servers
start_all_country_servers(CountryServerList) ->
  [ start_country_server(Svr) || Svr <- CountryServerList ].

%% Start a country server
start_country_server(Svr) when Svr#country_server.status == stopped ->
  CountryServer = ?COUNTRY_SERVER_NAME(Svr#country_server.country_code),
  ?TRACE("Starting country server ~s",[CountryServer]),

  case whereis(CountryServer) of
    undefined -> Svr#country_server{pid = country_server:init(CountryServer, Svr#country_server.trace), status = starting};
    _Pid      -> Svr
  end;

%% Ignore any country server that has a status other than 'stopped'
start_country_server(Svr) -> Svr.

%% ---------------------------------------------------------------------------------------------------------------------
%% Stop all country servers
stop_all_country_servers(CountryServerList) ->
  [ stop_country_server(Svr) || Svr <- CountryServerList ].

%% Stop a country server
stop_country_server(Svr) when Svr#country_server.status == started ->
  ?TRACE("Stopping ~p",[Svr#country_server.name]),
  Svr#country_server.name ! {cmd, shutdown},
  set_server_stopped(Svr);

%% Ignore any country server that has a status other than 'started'
stop_country_server(Svr) -> Svr.



%% ---------------------------------------------------------------------------------------------------------------------
%% Convert trace state atom to Boolean atom
trace_state_to_boolean(trace_on)  -> true;
trace_state_to_boolean(trace_off) -> false.



%% =====================================================================================================================
%%
%%                                    S O R T   S E R V E R   S T A T U S   L I S T
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Sort server status records by various column headings
%%
%% Sorting by continent implies sorting by country_name within continent
sort_servers_by(continent, A, B) -> 
  case A#country_server.continent == B#country_server.continent of
    true  -> A#country_server.country_name > B#country_server.country_name;
    false -> A#country_server.continent    > B#country_server.continent
  end;

sort_servers_by(country_name, A, B) -> simple_sort(A#country_server.country_name, B#country_server.country_name);
sort_servers_by(country_code, A, B) -> simple_sort(A#country_server.country_code, B#country_server.country_code);
sort_servers_by(city_count,   A, B) -> simple_sort(A#country_server.city_count,   B#country_server.city_count);
sort_servers_by(mem_usage,    A, B) -> simple_sort(A#country_server.mem_usage,    B#country_server.mem_usage);
sort_servers_by(startup_time, A, B) -> simple_sort(A#country_server.startup_time, B#country_server.startup_time).

%% By default, all atoms are greater than integers. So, since some fields might contain the atom 'undefined' we need to
%% override the default sort order
simple_sort(A, _) when is_atom(A) -> false;
simple_sort(_, B) when is_atom(B) -> true;
simple_sort(A, B)                 -> A > B.


