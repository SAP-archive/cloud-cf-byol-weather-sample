-module(process_tools).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/05/09 17:39:12").
-created_by("chris.whealy@sap.com").

-export([
    memory_usage/1
  , read_process_dictionary/2
]).



%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Get memory usage of a particular process
memory_usage(undefined)   -> 0;
memory_usage("undefined") -> 0;
memory_usage("Undefined") -> 0;

memory_usage(Pid) when is_pid(Pid) ->
  case process_info(Pid, memory) of
    undefined   -> 0;
    {memory, N} -> N
  end;

memory_usage(ProcessName) ->
  memory_usage(whereis(ProcessName)).

%% ---------------------------------------------------------------------------------------------------------------------
%% Locate a value in the process dictionary of some other process
read_process_dictionary(Pid, Name) ->
  {dictionary, Dict} = erlang:process_info(Pid, dictionary),
  search_dictionary(Name, Dict).



%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Scan contents of process dictionary for some value
search_dictionary(_,    [])                     -> undefined;
search_dictionary(Name, [{Name, Value} | _])    -> Value;
search_dictionary(Name, [{_, _}        | Rest]) -> search_dictionary(Name, Rest).
