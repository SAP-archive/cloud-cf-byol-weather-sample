
%% ---------------------------------------------------------------------------------------------------------------------
%% Tracing and logging macros that require the use of a NOW macro to generate a timestamp
%% ---------------------------------------------------------------------------------------------------------------------

%% ---------------------------------------------------------------------------------------------------------------------
%% Since the NOW macro uses an anonymous function, its internal variables exist in the scope of the calling function;
%% therefore, the variable names used here are longer to reduce the risk of an invisible name clash which would then
%% lead to weird "no_match" exceptions at runtime...
%%
%% Switched from the use of erlang:now/0 to os:timestamp/0
%% erlang:now/0 returns a monotonically increasing timestamp guarunteed to be unique across the entire node.
%% The problem with this function however is that it requires the timer value to be locked/unlocked before/after each
%% call - and this simply doesn't scale!
%%
%% ---------------------------------------------------------------------------------------------------------------------
-define(NOW, (fun({_,_,Micro} = TS) ->
    {{YYYY_temp,MM_temp,DD_temp},{H_temp,M_temp,S_temp}} = calendar:now_to_local_time(TS),
    {{YYYY_temp,MM_temp,DD_temp},{H_temp,M_temp,S_temp,Micro}}
   end)
   (os:timestamp())
  ).


%% **********************************************************************************************************************
%% Trace macro switches on when the process dictionary flag 'trace' is set to true
%% This allows for a finer grain control over how much trace information is created compared ti the all-or-nothing 
%% approach of using the debug compile flag
%% **********************************************************************************************************************
-define(FUNCTION_SIG, io_lib:format("~s:~s/~w, line ~w ",[?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE])).

-define(TRACE(Str),
  case get(trace) of
    true -> (fun(S) -> io:fwrite("~s ~s~n", [?FUNCTION_SIG, S]) end)(Str);
    _    -> trace_off
  end
).

-define(TRACE(FStr,Params), 
  case get(trace) of
    true -> (fun(F, P) -> io:fwrite("~s " ++ F ++ "~n", [?FUNCTION_SIG] ++ P) end)(FStr, Params);
    _    -> trace_off
  end
).


%% **********************************************************************************************************************
%% Write logging output to the console
%% **********************************************************************************************************************
-define(LOG(FStr,Params), io:fwrite("~s: " ++ FStr ++ "~n", [format:as_datetime(?NOW)] ++ Params)).

