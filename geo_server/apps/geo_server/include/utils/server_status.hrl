%% =====================================================================================================================
%%
%%              H A N D L E   C H A N G E S   T O   C O U N T R Y   S E R V E R   S T A T U S   R E C O R D S
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Reset a crashed server back to its initial conditions
reset_crashed_server(Rec) ->
  io:format("Resetting crashed server ~p~n",[Rec#country_server.name]),

  %% Ensure that the country server process really has terminated
  case whereis(Rec#country_server.name) of
    undefined -> ok;
    Pid       -> exit(Pid, reset)
  end,

  %% Set server's state back to initial
  set_server_init(Rec#country_server.country_code, Rec#country_server.country_name, Rec#country_server.continent).


%% ---------------------------------------------------------------------------------------------------------------------
%% Update trace flag in a country server status record
update_trace(Rec, TraceState) -> Rec#country_server{ trace = trace_state_to_boolean(TraceState) }.


%% ---------------------------------------------------------------------------------------------------------------------
%% Create a new server status record with status 'stopped' and trace 'false'
set_server_init(CountryCode, CountryName, Continent) ->
  #country_server{
      name         = ?COUNTRY_SERVER_NAME(CountryCode)
    , country_name = CountryName
    , continent    = Continent
    , country_code = CountryCode
    , status       = stopped
    , trace        = false
  }.


%% ---------------------------------------------------------------------------------------------------------------------
%% Set server status to stopped
set_server_stopped(Rec) ->
  Rec#country_server{
    pid          = undefined
  , status       = stopped
  , substatus    = undefined
  , progress     = undefined
  , children     = undefined
  , started_at   = undefined
  , startup_time = undefined
  , mem_usage    = undefined
  }.


%% ---------------------------------------------------------------------------------------------------------------------
%% Update country server status record to "running"
set_server_running(CountryServerList, Name, CityCount, StartComplete) ->
  Rec = lists:keyfind(Name, #country_server.name, CountryServerList),

  NewStatus = Rec#country_server{
    status       = started
  , substatus    = running
  , progress     = 100
  , city_count   = CityCount
  , startup_time = time:time_diff(StartComplete, Rec#country_server.started_at)
  , mem_usage    = process_tools:memory_usage(Rec#country_server.name)
  },

  lists:keyreplace(Name, #country_server.name, CountryServerList, NewStatus).


%% ---------------------------------------------------------------------------------------------------------------------
%% Update status of a given server without time stamp
%% When a server crashes, we only get the Pid that used to exist
set_server_status(CountryServerList, Pid, crashed, Substatus, _, _, _) when is_pid(Pid) ->
  Rec = lists:keyfind(Pid, #country_server.pid, CountryServerList),

  NewStatus = Rec#country_server{
    status    = crashed
  , substatus = Substatus
  , trace     = false
  , mem_usage = 0
  },

  lists:keyreplace(Pid, #country_server.pid, CountryServerList, NewStatus);

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% When a server is stopped (either due to no cities, or manual command, we still have its name available
set_server_status(CountryServerList, Name, stopped, Substatus, _, _, _) ->
  Rec = lists:keyfind(Name, #country_server.name, CountryServerList),

  NewStatus = Rec#country_server{
    status    = stopped
  , substatus = Substatus
  , trace     = false
  , mem_usage = 0
  },

  lists:keyreplace(Name, #country_server.name, CountryServerList, NewStatus);

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Server identified by its name
set_server_status(CountryServerList, Name, Status, Substatus, Progress, Children, Time) ->
  Rec = lists:keyfind(Name, #country_server.name, CountryServerList),

  NewStatus = Rec#country_server{
    pid          = whereis(Name)
  , status       = Status
  , substatus    = Substatus

  , progress = case Progress of
      init     -> 0;
      complete -> 100;
      P        -> Rec#country_server.progress + P
    end 

  , children = case is_list(Rec#country_server.children) of
      true  -> case Children of
                 [] -> Rec#country_server.children;
                 Id -> Rec#country_server.children ++ [Id]
               end;
      false -> case Children of
                [] -> [];
                Id -> [Id]
              end
    end

  , started_at = case Substatus of
      init -> Time;
      _    -> Rec#country_server.started_at
    end

  , mem_usage = case Status of
      started -> process_tools:memory_usage(Rec#country_server.name);
      _       -> 0
    end
  },

  lists:keyreplace(Name, #country_server.name, CountryServerList, NewStatus).


