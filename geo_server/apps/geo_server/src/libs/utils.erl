-module(utils).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/05/22 10:17:52").
-created_by("chris.whealy@sap.com").

-export([
    is_quote_delimited/1
  , datatype/1
]).

%% Records
-include("../../include/records/datatype.hrl").

%% Binary string values
-define(DBL_QUOTE, <<"\"">>).



%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Check if a printable character string is already delimited with double quotes
is_quote_delimited(V) when is_binary(V) ->
  is_quote_delimited(binary:bin_to_list(V));

is_quote_delimited([])       -> false;     % Empty string
is_quote_delimited([_ | []]) -> false;     % String length 1
is_quote_delimited("\"\"")   -> true;      % Empty quoted string

is_quote_delimited(Str) ->
  case length(Str) == 2 of
    true  ->
      false;
    
    false ->
      is_quote_delimited(string:slice(Str, 0, 1), string:slice(Str, length(Str) - 1))
  end.


%% ---------------------------------------------------------------------------------------------------------------------
%% Return a tagged tuple describing the value's apparent datatype, its represented datatype, and where possible, the
%% binary string representation of the value
%%
%% In cases of numeric strings, return the numeric value of the string
%% In cases of binary values, convert the value to a list and attempt to determine datatype via the string representation
%%

datatype(V) when is_boolean(V)   -> #datatype{type = boolean, value = list_to_binary(atom_to_list(V))};
datatype(V) when is_atom(V)      -> #datatype{type = atom,    value = list_to_binary(atom_to_list(V))};
datatype(V) when is_float(V)     -> #datatype{type = float,   value = list_to_binary(io_lib:format("~p",[V]))};
datatype(V) when is_integer(V)   -> #datatype{type = integer, value = list_to_binary(io_lib:format("~p",[V]))};

% Convert the binary value to a string, then attempt to work out its represented datatype
datatype(V) when is_binary(V)    -> 
  {RepresentedType, Value} = represented_data_type(binary_to_list(V)),
  #datatype{type = binary, represents = RepresentedType, value = Value};
  
% Assuming the list contains some sort of string, attempt to determine the datatype of the string representation
datatype(V) when is_list(V) ->
  {RepresentedType, Value} = represented_data_type(V),
  #datatype{type = list, represents = RepresentedType, value = Value};

datatype(V) when is_tuple(V)     -> {tuple,     list_to_binary(lists:flatten(io_lib:format("~p",[V])))};
datatype(V) when is_pid(V)       -> {pid,       list_to_binary(pid_to_list(V))};
datatype(V) when is_reference(V) -> {reference, list_to_binary(ref_to_list(V))};
datatype(V) when is_function(V)  -> {function,  list_to_binary(erlang:fun_to_list(V))};
datatype(V) when is_map(V)       -> {map, V};
datatype(V) when is_port(V)      -> {port, V};
datatype(V) when is_bitstring(V) -> {bitstring, V};

datatype(V) -> {indeterminate, V}.



%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% is_quote_delimited/2
is_quote_delimited("\"", "\"") -> true;
is_quote_delimited(_, _)       -> false.


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Determine the data type represented within a character string
represented_data_type(V) when is_list(V) ->
  case string:to_float(V) of                               % Is it a float?
    {error, _} ->                                          % Nope
      case string:to_integer(V) of                         % Is it an integer?
        {error, _} ->                                      % Nope
          case io_lib:printable_unicode_list(V) of         % Is it a printable unicode string?
            false ->                                       % Nope, so its some sort of generic list
              {undefined, V};
            true ->                                        % Yup, it has a printable representation
              {printable_unicode_list, list_to_binary(V)}
          end;

        {IntVal, _} ->                                     % Yup, its the string representation of an integer
          {numeric_str_int, IntVal}
      end;

    {FloatVal, _} ->                                       % Yup, its the string representation of a floating point
      {numeric_str_float, FloatVal}
  end.    

