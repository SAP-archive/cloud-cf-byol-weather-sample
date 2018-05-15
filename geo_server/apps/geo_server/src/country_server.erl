-module(country_server).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/02/16 13:24:21").
-created_by("chris.whealy@sap.com").

-export([
    init/2
  , start/3
]).

%% Records
-include("../include/records/geoname.hrl").

%% Macros
-include("../include/macros/trace.hrl").
-include("../include/macros/file_paths.hrl").
-include("../include/macros/now.hrl").

%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================


%% ---------------------------------------------------------------------------------------------------------------------
%% Initialise the country server
%% We could be passed either the two character country code as a string, or the name of server as an atom depending on
%% where the start command came from
init(CountryCode, Trace) when is_list(CountryCode) -> do_init(CountryCode, Trace);
init(ServerName,  Trace) when is_atom(ServerName)  -> do_init(extract_cc(ServerName), Trace).


%% ---------------------------------------------------------------------------------------------------------------------
%% Start the country server
start(CC, ServerName, Trace) ->
  process_flag(trap_exit, true),
  
  %% Store various values in the process dictionary
  put(my_name, ServerName),
  put(country_manager_pid, whereis(country_manager)),
  put(cc, CC),
  put(city_count, unknown),

  %% Trace flag supplied by the country manager
  put(trace, Trace),

  %% Inform country manager that this server is starting up
  country_manager ! {starting, init, ServerName, ?NOW},

  %% Ensure that the country directory exists, then check if the country file needs to be updated
  TargetDir = ?TARGET_DIR ++ CC ++ "/",
  filelib:ensure_dir(TargetDir),
  ?TRACE("Starting country server ~s in ~s",[CC, TargetDir]),

  %% Read the country file data
  spawn_link(import_files, read_country_file, [CC, self()]),

  CityServerList = receive
    {'EXIT', _, Reason} ->
      case Reason of
        {retry_limit_exceeded, [FileDetails | _]} ->
          exit({retry_limit_exceeded, FileDetails});

        SomethingElse ->
          exit({error, SomethingElse})
      end;

    {error, Reason} ->
      exit({country_file_error, Reason}),
      [];

    FCP_Data ->
      distribute_cities(FCP_Data)
  end,

  %% Inform country manager that start up is complete
  country_manager ! {started, running, ServerName, get(city_count), ?NOW},

  wait_for_msg(CityServerList).



%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================


%% ---------------------------------------------------------------------------------------------------------------------
%% Internal startup function
do_init(CC, Trace) ->
  ServerName = list_to_atom("country_server_" ++ string:lowercase(CC)),

  %% Has this country server already been registered?
  case whereis(ServerName) of
    undefined ->
      CountryServerPid = spawn_link(?MODULE, start, [CC, ServerName, Trace]),
      register(ServerName, CountryServerPid),
      CountryServerPid;

    SomePid ->
      SomePid
  end.

%% ---------------------------------------------------------------------------------------------------------------------
%% If the CityServerList is empty, then either this country has no cities with populations large enough to appear in a
%% search, or all the city servers have shut down.  Either way, the country server should also shut down
wait_for_msg([]) ->
  Reason = case get(shutdown) of
    undefined -> no_cities;
    _         -> stopped
  end,

  ?TRACE("~p stopping.  Reason: ~p",[get(my_name), Reason]),
  exit({Reason, get(my_name)});

wait_for_msg(CityServerList) ->
  CityServerList1 = receive
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Termination messages

    %% Handle process exit
    {'EXIT', SomePid, Reason} ->
      handle_exit(SomePid, Reason, CityServerList);

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Server commands
    
    %% Commands sent to all city servers
    {cmd, CmdOrQuery} ->
      case CmdOrQuery of
        city_list  -> send_to_all(CityServerList, city_list);
        city_stats -> send_to_all(CityServerList, city_stats);

        shutdown   ->
          ?TRACE("~p sending shutdown command to all city servers",[get(my_name)]),
          put(shutdown, true),
          send_to_all(CityServerList, shutdown);

        trace_on   -> put(trace, true),  send_to_all(CityServerList, trace_on);
        trace_off  -> send_to_all(CityServerList, trace_off), put(trace, false);

        child_list -> io:format("Country server ~s uses child processes~n~s~n",[get(cc), format_proc_list(CityServerList)]);
        _          -> io:format("~s received unknown command ~p~n",[get(my_name), CmdOrQuery])
      end,
      
      CityServerList;
    
    %% Commands sent to a specific city server
    {cmd, city_list, Id} ->
      Id1 = string:uppercase(Id),

      case lists:keyfind(Id1, 4, CityServerList) of
        {pid, Pid, id, Id1} -> Pid ! {cmd, city_list};
        _                   -> io:format("Country ~s has no cities starting with ~s~n",[get(cc), Id])
      end,

      CityServerList;

    %% Commands from the server_info handler
    {cmd, city_count, RequestHandlerPid} ->
      ?TRACE("City count requested by ~p.  Returning ~w",[RequestHandlerPid, get(city_count)]),
      RequestHandlerPid ! {city_count, get(city_count)},
      CityServerList;

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Query
    {query, Ref, {search_term, SearchTerm, whole_word, _, starts_with, _} = QS, RequestHandlerPid} ->
      ?TRACE("Country server ~s (~p) received query \"~s\" from request handler ~p", [get(cc), self(), SearchTerm, RequestHandlerPid]),
      RequestHandlerPid ! handle_query(Ref, CityServerList, QS),
      CityServerList

  end,

  wait_for_msg(CityServerList1).

%% ---------------------------------------------------------------------------------------------------------------------
%% Wait for responses from city servers

%% Response from last city server
wait_for_results(Ref, 0, ResultList) ->
  ?TRACE("Country ~s search complete: found ~w results", [get(cc), length(ResultList)]),
  {results, Ref, ResultList};

%% Responses from city servers
wait_for_results(Ref, N, ResultList) ->
  ?TRACE("Country server ~s waiting for ~w more results", [get(cc), N]),

  ResultList1 = ResultList ++ receive
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Response from city server - which might be an empty list
    {results, Ref, _Id, SearchResults} ->
      case length(SearchResults) of
        0 -> ?TRACE("No match in ~s for letter ~c",[get(cc), _Id]);
        L -> ?TRACE("Found ~w results in ~s",[L, get(cc)])
      end,

      SearchResults
  end,

  wait_for_results(Ref, N-1, ResultList1).



%% ---------------------------------------------------------------------------------------------------------------------
%% Handle query from client

%% If the starts_with parameter is false, then the "contains" search option has been selected.  Therefore, the query
%% must be sent to all city servers for this country
handle_query(Ref, CityServerList, {search_term, SearchTerm, whole_word, _, starts_with, false} = QS) ->
  ?TRACE("Sending query to all city servers"),
  send_to_all(CityServerList, {Ref, QS, get(cc), SearchTerm, self()}),
  wait_for_results(Ref, length(CityServerList), []);


%% If the starts_with parameter is true, then query need only be sent to the city server handling cities starting with
%% the first letter of the search term
handle_query(Ref, CityServerList, {search_term, [Char1 | _] = SearchTerm, whole_word, _, starts_with, true} = QS) ->
  Id = string:uppercase([Char1]),

  %% Do we have a child process for the first letter of this query?
  case lists:keyfind(Id, 4, CityServerList) of
    %% Yup, so pass query down to the relevant child process
    {pid, ChildPid, id, Id} ->
      ?TRACE("Sending query to ~s city server ~p",[get(cc), Id]),
      ChildPid ! {Ref, QS, get(cc), SearchTerm, self()},
      wait_for_results(Ref, 1, []);

    %% Nope, so ignore query
    false ->
      ?TRACE("Country ~s has no server for cities starting with ~p", [get(cc), Id]),
      {results, Ref, []}

  end.


%% ---------------------------------------------------------------------------------------------------------------------
%% Handle EXIT messages.
%% This function must return some version of the CityServerList
handle_exit(SomePid, Reason, CityServerList) ->
  %% Has the country_manager shut down?
  case pids_are_equal(get(country_manager_pid), SomePid) of
    %% Yup, so shut down all the city servers (which in turn, will cause this country_server to shut down with reason
    %% 'no_cities')
    true ->
      send_to_all(CityServerList, shutdown),
      CityServerList;

    %% Nope, the country_manager is still alive
    false ->
      %% Has the hierarchy server shutdown?
      case pids_are_equal(get(hierarchy_server_pid), SomePid) of
        %% Yup.  That's fine, this does not need to be reported
        true ->
          CityServerList;

        false ->
          %% Nope, so it must be a city server that's died
          CityServerId = get_child_id(SomePid, CityServerList),

          case Reason of
            normal -> ok;
            _      -> io:format("City server ~p (~p) terminated with reason ~p~n", [CityServerId, SomePid, Reason])
          end,
    
          lists:keydelete(SomePid, 2, CityServerList)
      end
  end.

pids_are_equal(_Pid, _Pid) -> true;
pids_are_equal(_Pid, _)   -> false.

%% ---------------------------------------------------------------------------------------------------------------------
%% Create a child process to handle the cities belonging to successive letters of the alphabet
%%
distribute_cities(FCP) ->
  country_manager ! {starting, distribute_cities, get(my_name)},

  %% Remember the number of cities in the process dictionary
  put(city_count, length(FCP)),

  distribute_cities(FCP, []).

%% CityServerList is a list of {pid, <0.1.0>, id, "A"}
distribute_cities([], CityServerList) -> CityServerList;

%% Distribute FCP records amongst city servers based on the first character of the city name
distribute_cities([FCP_Rec | Rest], CityServerList) ->
  [Char1 | _] = string:uppercase(binary_to_list(FCP_Rec#geoname_int.name)),

  %% Extend CityServerList each time a city name starting with new letter of the alphabet is encountered
  NewCityServer = case lists:keyfind([Char1], 4, CityServerList) of
    %% No child process exists yet for city names starting with this letter
    false ->
      % ?TRACE("~p starting new city server for letter ~c",[get(my_name), Char1]),
      country_manager ! {starting, distribute_cities, get(my_name), Char1},
      [{pid, spawn_link(city_server, init, [self(), Char1, [FCP_Rec]]), id, [Char1]}];

    %% A child process already exists to handle cities starting with this letter, so send the curemnt record to that
    %% process.  We do not expect a reply
    {pid, CityServerPid, id, _} ->
      % ?TRACE("Adding record for letter ~c",[Char1]),
      CityServerPid ! {add, FCP_Rec},
      []
  end,

  distribute_cities(Rest, lists:append(CityServerList, NewCityServer)).


%% ---------------------------------------------------------------------------------------------------------------------
%% Send either a command or a query to all city servers
send_to_all(CityServerList, CmdOrQuery) ->
  Msg = case is_atom(CmdOrQuery) of
    true  -> {cmd, CmdOrQuery};
    false -> CmdOrQuery 
  end,

  lists:foreach(
    fun({pid, Pid, id, _Id}) ->
      ?TRACE("Sending ~p to ~p city server ~p",[CmdOrQuery, get(cc), _Id]),
      Pid ! Msg
    end,
    CityServerList
  ).


%% ---------------------------------------------------------------------------------------------------------------------
%% Format process list
format_proc_list(CityServerList) -> format_proc_list(CityServerList, "").

format_proc_list([], Acc)                          -> lists:flatten(Acc);
format_proc_list([{pid, Pid, id, Id} | Rest], Acc) -> format_proc_list(Rest, lists:append(Acc, [io_lib:format("~p ~s~n",[Pid,Id])])).



%% ---------------------------------------------------------------------------------------------------------------------
%% Get child process id.  Returns an atom
get_child_id(SomePid, CityServerList) ->
  case lists:keyfind(SomePid, 2, CityServerList) of
    {pid, _, id, Id} -> list_to_atom(Id);
    false            -> unknown_city_server_pid
  end.


%% ---------------------------------------------------------------------------------------------------------------------
%% Extract country code from server name
extract_cc(ServerName) ->
  Name = atom_to_list(ServerName),
  string:uppercase(lists:nthtail(length(Name)-2,Name)).

