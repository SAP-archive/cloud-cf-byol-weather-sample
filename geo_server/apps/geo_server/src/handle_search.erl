-module(handle_search).
-behavior(cowboy_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/02/03 10:45:47").
-created_by("chris.whealy@sap.com").

-export([init/2]).

%% Records
-include("../include/records/geoname.hrl").
-include("../include/records/country_server.hrl").

%% Macros
-include("../include/macros/trace.hrl").


-define(SERVER_NAME(Cc), list_to_atom("country_server_" ++ string:lowercase(Cc))).
-define(QS_PARAMETERS, [search_term, whole_word, starts_with]).

%% ---------------------------------------------------------------------------------------------------------------------
%%                             P U B L I C   A P I
%% ---------------------------------------------------------------------------------------------------------------------

%% ---------------------------------------------------------------------------------------------------------------------
init(Req, _State) ->
	%% Debug trace
	put(trace, true),

	#{search_term := P1, whole_word := P2, starts_with := P3} = cowboy_req:match_qs(?QS_PARAMETERS, Req),

	QS = {search_term, binary_to_list(P1),
	      whole_word,  binary_to_atom(P2, latin1),
        starts_with, binary_to_atom(P3, latin1)},

  % Are the query string parameters valid?
  Response = case validate_qs_parms(QS) of
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    true ->
      % Get list of started country servers from the country_manager
      country_manager ! {cmd, status, started, self()},
	
      ServerList = receive
        {started_servers, S} -> S;
        _ -> []
      end,

      Ref = make_ref(),

      %% Send query to all started country servers
      [ Svr#country_server.name ! {query, Ref, QS, self()} || Svr <- ServerList ],

      ResultList = wait_for_results(Ref, length(ServerList)),

      cowboy_req:reply(200,
        #{<<"content-type">>                => <<"text/json">>,
          <<"access-control-allow-origin">> => cowboy_req:header(<<"origin">>, Req)},
        geoname_to_json(ResultList),
        Req);

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  false ->
      cowboy_req:reply(400,
        #{<<"content-type">>                => <<"text/json">>,
          <<"access-control-allow-origin">> => cowboy_req:header(<<"origin">>, Req)},
        format_bad_request(QS),
        Req)

  end,

	{ok, Response, []}.





%% ---------------------------------------------------------------------------------------------------------------------
%%                            P R I V A T E   A P I
%% ---------------------------------------------------------------------------------------------------------------------

%% ---------------------------------------------------------------------------------------------------------------------
wait_for_results(Ref, N) -> wait_for_results(Ref, N, []).

wait_for_results(_Ref, 0, Acc) ->
	?TRACE("Search complete. ~w results received",[length(Acc)]),
	Acc;

wait_for_results(Ref, N, Acc) ->
	receive
		{results, Ref, ResultList} -> wait_for_results(Ref, N-1, Acc ++ ResultList);
		{error,   Ref, Reason}     -> wait_for_results(Ref, N-1, Acc ++ Reason)
  end.

%% ---------------------------------------------------------------------------------------------------------------------
%% Convert a geoname_int record to a JSON string in binary format
geoname_to_json(GeonameList) -> geoname_to_json(GeonameList, []).

geoname_to_json([], Acc) ->
  << <<"[">>/binary, (list_to_binary(lists:flatten(lists:join(<<",">>, Acc))))/binary, <<"]">>/binary >>;

geoname_to_json([G | Rest], Acc) ->
	JsonBin =
  << <<"{ \"name\": \"">>/binary,       (G#geoname_int.name)/binary,                     <<"\", ">>/binary
	 , <<"\"lat\": ">>/binary,            (G#geoname_int.latitude)/binary,                 <<", ">>/binary
	 , <<"\"lng\": ">>/binary,            (G#geoname_int.longitude)/binary,                <<", ">>/binary
	 , <<"\"featureClass\": \"">>/binary, (G#geoname_int.feature_class)/binary,            <<"\", ">>/binary
	 , <<"\"featureCode\": \"">>/binary,  (G#geoname_int.feature_code)/binary,             <<"\", ">>/binary
	 , <<"\"countryCode\": \"">>/binary,  (G#geoname_int.country_code)/binary,             <<"\", ">>/binary
	 , <<"\"admin1Txt\": \"">>/binary,    (val_or_null(G#geoname_int.admin1_txt))/binary,  <<"\", ">>/binary
	 , <<"\"admin2Txt\": \"">>/binary,    (val_or_null(G#geoname_int.admin2_txt))/binary,  <<"\", ">>/binary
	 , <<"\"admin3Txt\": \"">>/binary,    (val_or_null(G#geoname_int.admin3_txt))/binary,  <<"\", ">>/binary
	 , <<"\"admin4Txt\": \"">>/binary,    (val_or_null(G#geoname_int.admin4_txt))/binary,  <<"\", ">>/binary
   , <<"\"timezone\": \"">>/binary,     (G#geoname_int.timezone)/binary,                 <<"\" }">>/binary
  >>,

	geoname_to_json(Rest, Acc ++ [JsonBin]).

val_or_null(undefined) -> <<"null">>;
val_or_null(Val)       -> Val.

%% ---------------------------------------------------------------------------------------------------------------------
%% Validate the query string parameter values
validate_qs_parms({search_term, _, whole_word, WW, starts_with, SW}) ->
  is_boolean(WW) and is_boolean(SW).

format_bad_request({search_term, _, whole_word, WW, starts_with, SW}) ->
  MsgStr = list_to_json_array([format_bad_boolean(whole_word, WW),
                               format_bad_boolean(starts_with, SW)]),

  << <<"{ \"error\": \"Bad request\", \"reason\": ">>/binary
   , (list_to_binary(MsgStr))/binary
   , <<" }">>/binary
  >>.

format_bad_boolean(_, V) when is_boolean(V) -> ok;
format_bad_boolean(K, V)                    -> io_lib:format("Parameter '~p' contains invalid Boolean value '~p'",[K, V]).

list_to_json_array(L) -> list_to_json_array(L,[]).

list_to_json_array([], Acc)            -> lists:flatten("[" ++ string:join(Acc, ",") ++ "]");
list_to_json_array([ok | Tail], Acc)   -> list_to_json_array(Tail, Acc);
list_to_json_array([Head | Tail], Acc) -> list_to_json_array(Tail, Acc ++ ["\"" ++ Head ++ "\""]).
