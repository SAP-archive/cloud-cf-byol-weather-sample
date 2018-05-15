-module(time).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/05/09 16:07:45").
-created_by("chris.whealy@sap.com").

-export([
    time_diff/2
  , make_millis/1
  , to_timestamp/1
]).



%% =====================================================================================================================
%%
%%                                           T I M E   C A L C U L A T I O N S
%%
%% =====================================================================================================================

%% Calculate the difference between two times where the times are in various formats:
%%  o  Two standard Erlang DateTimes
%%  o  Two standard Erlang Timestamps
%%  o  Two custom Erlang DateTimes with additional microseconds part
%%
%% The first parameter is always assumed to be later (bigger) than the second and the result is truncated to the nearest
%% millisecond

time_diff(_, undefined) -> undefined;
time_diff(undefined, _) -> undefined;

%% Two standard Erlang timestamps
time_diff({Mega1, Sec1, Micro1}, {Mega2, Sec2, Micro2}) -> (Mega1 - Mega2) + (Sec1 - Sec2) + make_millis(Micro1 - Micro2);

%% Two custom datetimes with additional microseconds part
time_diff({Date1,{H1,M1,S1,Micro1}}, {Date2,{H2,M2,S2,Micro2}}) ->
  SecDiff = calendar:datetime_to_gregorian_seconds({Date1,{H1,M1,S1}}) -
            calendar:datetime_to_gregorian_seconds({Date2,{H2,M2,S2}}),

  SecDiff + make_millis(Micro1 - Micro2);

%% Two standard Erlang datetimes
time_diff({Date1, Time1}, {Date2, Time2}) ->
  calendar:datetime_to_gregorian_seconds({Date1, Time1}) - calendar:datetime_to_gregorian_seconds({Date2, Time2}).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Truncate microseconds to millesconds and return a decimal fraction
make_millis(Micro) -> (Micro div 1000) / 1000.

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Convert standard DateTime to Timestamp
to_timestamp({{YYYY,MM,DD},{H,M,S}}) ->
  TS = calendar:datetime_to_gregorian_seconds({{YYYY,MM,DD},{H,M,S}}) - 62167219200,
  {(TS div 1000000), (TS rem 1000000), 0};

%% Convert custom DateTime to Timestamp
to_timestamp({{YYYY,MM,DD},{H,M,S,Micro}}) ->
  TS = calendar:datetime_to_gregorian_seconds({{YYYY,MM,DD},{H,M,S}}) - 62167219200,
  {(TS div 1000000), (TS rem 1000000), Micro}.


