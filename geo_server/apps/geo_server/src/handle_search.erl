-module(handle_search).
-behavior(cowboy_handler).

-include("../include/macros/revision.hrl").
-revision(?REVISION).

-author("Chris Whealy <chris.whealy@sap.com>").
-created("Date: 2018/02/03 10:45:47").
-created_by("chris.whealy@sap.com").

-export([init/2]).

%% Records
-include("../include/records/geoname.hrl").
-include("../include/records/country_server.hrl").

-define(HTTP_STATUS_OK,          200).
-define(HTTP_STATUS_BAD_REQUEST, 400).

-define(SERVER_NAME(Cc), list_to_atom("country_server_" ++ string:lowercase(Cc))).
-define(QS_PARAMETERS, [search_term, whole_word, starts_with]).

%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
init(Req, _State) ->
	#{search_term := P1, whole_word := P2, starts_with := P3} = cowboy_req:match_qs(?QS_PARAMETERS, Req),

	QS = {search_term, binary_to_list(P1),
	      whole_word,  binary_to_atom(P2, latin1),
        starts_with, binary_to_atom(P3, latin1)},

  Hdrs = build_response_hdrs(cowboy_req:header(<<"origin">>, Req)),

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
      JsonResponse = json:record_to_json(geoname_int, ResultList),

      cowboy_req:reply(?HTTP_STATUS_OK, Hdrs, json:to_bin_string(JsonResponse), Req);

    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    false ->
      cowboy_req:reply(?HTTP_STATUS_BAD_REQUEST, Hdrs, json:to_bin_string(format_bad_request(QS)), Req)

  end,

	{ok, Response, []}.



%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================

%% ---------------------------------------------------------------------------------------------------------------------
wait_for_results(Ref, N) -> wait_for_results(Ref, N, []).

wait_for_results(_Ref, 0, Acc) ->	Acc;
wait_for_results( Ref, N, Acc) ->
	receive
		{results, Ref, ResultList} -> wait_for_results(Ref, N-1, Acc ++ ResultList);
		{error,   Ref, Reason}     -> wait_for_results(Ref, N-1, Acc ++ Reason)
  end.

%% ---------------------------------------------------------------------------------------------------------------------
%% Validate the query string parameter values
validate_qs_parms({search_term, _, whole_word, WW, starts_with, SW}) -> is_boolean(WW) and is_boolean(SW).

format_bad_request({search_term, _, whole_word, WW, starts_with, SW}) ->
  Reason = lists:flatten([format_bad_boolean(whole_word, WW)
                        , format_bad_boolean(starts_with, SW)]),

  json:object([
      json:property(error, "Bad request")
    , json:property(reason, Reason)
  ]).

%% ---------------------------------------------------------------------------------------------------------------------
%% Validate the query string parameter values
format_bad_boolean(K, _) when is_boolean(K) -> [];
format_bad_boolean(K,V)                     -> io_lib:format("Invalid Boolean value '~p' in parameter '~p'. ",[V,K]).

%% ---------------------------------------------------------------------------------------------------------------------
%% Build response headers
build_response_hdrs(undefined) ->
  #{<<"content-type">> => <<"text/json">>};

build_response_hdrs(Requester) ->
  #{<<"content-type">>                => <<"text/json">>,
    <<"access-control-allow-origin">> => Requester}.
