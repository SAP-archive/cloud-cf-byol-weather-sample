%% =====================================================================================================================
%%
%%                                       J S O N   T R A N S F O R M A T I O N S
%%
%% =====================================================================================================================
-module(json).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/05/09 16:07:45").
-created_by("chris.whealy@sap.com").

-export([
    record_to_json/2
  , property/2
  , array/1
  , object/1
  , to_bin_string/1
]).

%% Record definitions
-include("../../include/records/cmd_response.hrl").
-include("../../include/records/country_server.hrl").
-include("../../include/records/geoname.hrl").
-include("../../include/records/json.hrl").

%% Macro definitions
-include("../../include/macros/trace.hrl").

-define(OPEN_BRACKET,  <<"[">>).
-define(CLOSE_BRACKET, <<"]">>).
-define(OPEN_CURLY,    <<"{">>).
-define(CLOSE_CURLY,   <<"}">>).
-define(COLON,         <<":">>).
-define(COMMA,         <<",">>).
-define(NULL,          <<"null">>).
-define(TRUE,          <<"true">>).
-define(FALSE,         <<"false">>).


%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================


%% ---------------------------------------------------------------------------------------------------------------------
%% Transform the cmd_response record
%%
%% The payload field contains a single country_server record
record_to_json(cmd_response, Rec) when is_record(Rec#cmd_response.payload, country_server) ->
  record_to_json_int(cmd_response, Rec#cmd_response{
    payload = record_to_json(country_server, Rec#cmd_response.payload)
  });


%% The payload field contains a list of country_server records
%% It is assumed that if the payload field contains a list, that it can only be a list of country_server records
record_to_json(cmd_response, Rec) when is_list(Rec#cmd_response.payload) ->
  record_to_json_int(cmd_response, Rec#cmd_response{
    payload = array([ record_to_json_int(country_server, Svr) || Svr <- Rec#cmd_response.payload ])
  });


%% Some other command response
record_to_json(cmd_response, Rec) ->
  record_to_json_int(cmd_response, Rec);


%% ---------------------------------------------------------------------------------------------------------------------
%% Transform a country_server record into a JSON object
record_to_json(country_server, Rec) when is_record(Rec, country_server) ->
  %% Format the Erlang date time field into a human readable form
  record_to_json_int(country_server, Rec#country_server{
    started_at = format:as_datetime(Rec#country_server.started_at)
  });


%% ---------------------------------------------------------------------------------------------------------------------
%% Transform either a single geoname_int record or a list of geoname_int records
record_to_json(geoname_int, Rec)  when is_record(Rec, geoname_int) -> record_to_json_int(geoname_int, Rec);
record_to_json(geoname_int, Recs) when is_list(Recs)               -> array([record_to_json_int(geoname_int, Rec) || Rec <- Recs]).


%% ---------------------------------------------------------------------------------------------------------------------
%% Construct a JSON property record from a name/value pair
%% The value passed to this function should be either:
%%   * A simple value in binary form
%%   * A JSON array
%%   * A JSON object

%% The property value is a tagged tuple representing another JavaScript entity
property(PropName, T)       when is_tuple(T)       -> kv_to_json_property_rec(PropName, T);

%% Specifically handle JavaScript reserved words.
%% All other atoms are converted to quoted binary strings
property(PropName, undefined)                      -> kv_to_json_property_rec(PropName, ?NULL);
property(PropName, true)                           -> kv_to_json_property_rec(PropName, ?TRUE);
property(PropName, false)                          -> kv_to_json_property_rec(PropName, ?FALSE);
property(PropName, AtomVal) when is_atom(AtomVal)  -> kv_to_json_property_rec(PropName, format:as_quoted_str(atom_to_list(AtomVal)));

%% Convert other values that need to be quoted strings
property(PropName, Pid)     when is_pid(Pid)       -> kv_to_json_property_rec(PropName, format:as_quoted_str(pid_to_list(Pid)));
property(PropName, StrVal)  when is_list(StrVal)   -> kv_to_json_property_rec(PropName, format:as_quoted_str(StrVal));

%% Do not convert numeric values to strings
property(PropName, NumVal)  when is_number(NumVal) -> kv_to_json_property_rec(PropName, list_to_binary(io_lib:format("~p",[NumVal])));

%% Binary values must be examined to see if they represented a value that needs double quotes around it
property(PropName, BinVal) when is_binary(BinVal) ->
  {datatype, _, RepresentedType, Value} = utils:datatype(BinVal),

  %% Should we use double quotes around the value?
  case RepresentedType of
    %% For integer numeric strings, transform the converted numeric value to binary not the string value.
    %% This is to avoid confusing JavaScript's tiny mind that thinks all numeric values with a leading zero are octal
    numeric_str_int        -> kv_to_json_property_rec(PropName, integer_to_binary(Value));
    printable_unicode_list -> kv_to_json_property_rec(PropName, format:as_quoted_str(Value));
    _                      -> kv_to_json_property_rec(PropName, BinVal)
  end.

%% ---------------------------------------------------------------------------------------------------------------------
%% Return a JSON array from a list of values.
%% It is assumed that each value in the list is a valid JSON array element member (I.E. not a naked json_property)
array([])                                 -> {json_array, []};
array(L) when is_list(L)                  -> {json_array, L};
array(V) when is_record(V, json_property) -> {json_array, [V]}.


%% ---------------------------------------------------------------------------------------------------------------------
%% Return a JSON object from a list of JSON properties.  Each value in the list must be a valid JSON property record.
%% Any values that are not JSON property records are omitted from the returned object
object([]) -> {json_object, []};

object(Properties) when is_list(Properties) ->
  object([], Properties);

object(Property) when is_record(Property, json_property) ->
  object([], [Property]).

%% ---------------------------------------------------------------------------------------------------------------------
%% Transform a JSON object, array or property into a binary string
to_bin_string({json_object, PropList}) ->
  list_to_binary([?OPEN_CURLY
                , comma_separate([to_bin_string(Prop) || Prop <- PropList])
                , ?CLOSE_CURLY]);

to_bin_string({json_array, Elements}) ->
  list_to_binary([?OPEN_BRACKET
                , comma_separate([to_bin_string(El) || El <- Elements])
                , ?CLOSE_BRACKET]);

to_bin_string({json_property, K, V}) -> list_to_binary([K, ?COLON, to_bin_string(V)]);

to_bin_string(Vals) when is_list(Vals) -> comma_separate([to_bin_string(Val) || Val <- Vals]);

to_bin_string(V) -> V.


%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
%% Transform a record into a JSON object
record_to_json_int(RecType, Rec) ->
  RecInfo = case RecType of
    cmd_response   -> record_info(fields, cmd_response);
    country_server -> record_info(fields, country_server);
    geoname_int    -> record_info(fields, geoname_int)
  end,

  object([ property(K,V) || {K, V} <- lists:zip(RecInfo, tl(tuple_to_list(Rec)))]).


%% ---------------------------------------------------------------------------------------------------------------------
%% Filter out any list elements that are not json_property records
object(Acc, [])                                          -> {json_object, Acc};
object(Acc, [P | Rest]) when is_record(P, json_property) -> object(Acc ++ [P], Rest);
object(Acc, [_ | Rest])                                  -> object(Acc, Rest).


%% ---------------------------------------------------------------------------------------------------------------------
%% Create a json_property record from a key and a value
%% Convert property names to a binary quoted string if they arrive as an atom
%% The conversion of the property value to a binary (possibly quoted) string happens at the time the property is created
kv_to_json_property_rec(K,V) when is_atom(K) -> #json_property{name = format:as_quoted_str(atom_to_list(K)), value = V};
kv_to_json_property_rec(K,V)                 -> #json_property{name = format:as_quoted_str(K),               value = V}.


%% ---------------------------------------------------------------------------------------------------------------------
%% Join a list of values into a comma separated string
comma_separate(Vs) -> lists:flatten(lists:join(?COMMA,Vs)).

