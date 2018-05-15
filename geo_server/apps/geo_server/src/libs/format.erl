-module(format).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/05/09 18:08:22").
-created_by("chris.whealy@sap.com").

-export([
    as_binary_units/1
  , as_datetime/1
  , as_seconds/1
  , as_ordinal/1
]).

-define(KB, 1024).
-define(MB, ?KB * 1024).
-define(GB, ?MB * 1024).

%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Format integer as binary units of Kb, Mb or Gb truncated to three decimal places
as_binary_units(undefined)            -> "0 bytes";
as_binary_units(0)                    -> "0 bytes";
as_binary_units(N) when is_integer(N) -> lists:flatten(as_binary_units_int(N)).



%% =====================================================================================================================
%%                                F O R M A T   T I M E   V A L U E S   A S   S T R I N G S
%% =====================================================================================================================

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Transform Erlang local time or timestamp to string
as_datetime(undefined) -> undefined;

%% Standard Erlang timestamp format
as_datetime({Mega,Sec,Micro}) ->
  as_datetime(calendar:now_to_local_time({Mega,Sec,Micro}));

%% Standard Erlang DateTime format
as_datetime({{YYYY,MM,DD},{H,M,S}}) ->
  lists:flatten(io_lib:format("~w ~s ~s, ~2..0B:~2..0B:~2..0B", [YYYY, month_name(MM), format_day(DD), H, M, S]));

%% Custom DateTime format with additional microseconds value
as_datetime({{YYYY,MM,DD},{H,M,S,Micro}}) ->
  S1 = S + time:make_millis(Micro),
  lists:flatten(io_lib:format("~w ~s ~s, ~2..0B:~2..0B:~6.3.0fs", [YYYY, month_name(MM), format_day(DD), H, M, S1])).


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Convert a number of seconds to string
as_seconds(undefined) -> undefined;

as_seconds(0)   -> "0.0s";
as_seconds(0.0) -> "0.0s";

as_seconds(T) -> lists:flatten(as_seconds_int(T)).

as_seconds_int(T) when T >= 3600 ->
  H  = trunc(T) div 3600,
  T1 = trunc(T) rem 3600,
  S  = (T1 rem 60) + mantissa(T), 

  io_lib:format("~w:~2..0B:~6.3.0fs",[H, (T1 div 60), S]);

as_seconds_int(T) when T >= 60 ->
  M = trunc(T) div 60,
  S = (trunc(T) rem 60) + mantissa(T),
  io_lib:format("~w:~6.3.0fs",[M, S]);

as_seconds_int(T) when is_integer(T) -> io_lib:format("~ws",[T]);
as_seconds_int(T)                    -> io_lib:format("~.3fs",[T]).


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Convert an integer to its ordinal string
as_ordinal(D) when D == 1; D == 21; D == 31 -> "st";
as_ordinal(D) when D == 2; D == 22          -> "nd";
as_ordinal(D) when D == 3; D == 23          -> "rd";
as_ordinal(_)                               -> "th".


%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
as_binary_units_int(N) when N < ?KB -> io_lib:format("~w bytes",[N]);
as_binary_units_int(N) when N < ?MB -> as_binary_units_int(N, ?KB, "Kb");
as_binary_units_int(N) when N < ?GB -> as_binary_units_int(N, ?MB, "Mb");
as_binary_units_int(N)              -> as_binary_units_int(N, ?GB, "Gb").

as_binary_units_int(N, Unit, UnitStr) ->
  WholeUnits = N div Unit,
  Rem = (N - (WholeUnits * Unit)) / Unit,
  io_lib:format("~.2f ~s",[WholeUnits + Rem, UnitStr]).


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Month number to name conversion
month_name(MM) -> element(MM, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Add as_ordinal string to a day number
format_day(D) -> integer_to_list(D) ++ as_ordinal(D).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
mantissa(F) when is_float(F)   -> F - trunc(F);
mantissa(F) when is_integer(F) -> 0.0.

