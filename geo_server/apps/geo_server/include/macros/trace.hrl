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
