
%% ---------------------------------------------------------------------------------------------------------------------
%% Recod to track status of a country server
%% ---------------------------------------------------------------------------------------------------------------------
-record(country_server, {
  name                   %% Server name    :: atom()
, country_name           %% Country name   :: string()
, continent              %% Continent code :: string()
, country_code           %% Country code   :: string()
, pid                    %% Server pid     :: pid()
, status                 %% Main status    :: atom()
, substatus              %% Substatus      :: atom()
, progress               %% Progress %     :: integer
, city_count             %% Number of cities managed by this server :: integer
, children               %% List of child processes :: List[char()]
, started_at             %% Custom timestamp for server start time  :: string()
, startup_time           %% Server startup duration in ms :: integer()
, trace                  %% Trace active   :: Boolean atom()
, mem_usage              %% Server's memory usage     :: integer()
}).

