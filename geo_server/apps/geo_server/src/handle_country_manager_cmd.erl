-module(handle_country_manager_cmd).
-behavior(cowboy_handler).

-author("Chris Whealy <chris.whealy@sap.com>").
-revision("Revision: 1.0.0").
-created("Date: 2018/04/17 11:43:29").
-created_by("chris.whealy@sap.com").

-export([
    init/2
  ]).

%% Record definitions
-include("../include/records/cmd_response.hrl").
-include("../include/records/country_server.hrl").

%% Macros
-include("../include/macros/default_http_response.hrl").

-define(HTTP_GET, <<"GET">>).


%% =====================================================================================================================
%%
%%                                                 P U B L I C   A P I
%%
%% =====================================================================================================================

init(Req=#{method := ?HTTP_GET}, State) ->
	#{cmd := Cmd} = cowboy_req:match_qs([cmd], Req),

  %% Send the command to the country manager
  case Cmd of
    %% The name of the server to be started must also appear in the query string
    <<"set_debug">> ->
      #{param := Param} = cowboy_req:match_qs([param], Req),

      case Param of
        <<"true">>  -> country_manager ! {cmd, trace, on,  self()};
        <<"false">> -> country_manager ! {cmd, trace, off, self()}
      end;

    <<"sort_ascending">> ->
      #{param := Param} = cowboy_req:match_qs([param], Req),

      country_manager ! {sort, ascending, binary_to_atom(Param, utf8), self()};

    <<"sort_descending">> ->
      #{param := Param} = cowboy_req:match_qs([param], Req),

      country_manager ! {sort, descending, binary_to_atom(Param, utf8), self()};

    <<"start_all">>    -> country_manager ! {cmd, start_all,    self()};
    <<"shutdown_all">> -> country_manager ! {cmd, shutdown_all, self()};
    <<"reset_all">>    -> country_manager ! {cmd, reset_all,    self()}
  end,

  %% What response did we get?
  JsonResp = receive
    %% A general command response tuple
    CmdResponse when is_record(CmdResponse, cmd_response) ->
      json:record_to_json(cmd_response, CmdResponse);

    %% A country server list
    CountryServerList when is_list(CountryServerList) ->
      CmdResp = #cmd_response{
        from_server = country_manager
      , cmd         = binary_to_atom(Cmd, utf8)
      , status      = ok
      , payload     = json:make_json_array([ json:record_to_json(country_server, Svr) || Svr <- CountryServerList ])
      },
      json:record_to_json(cmd_response, CmdResp);

    %% Any unrecognised message is assumed to be an error...
    SomeVal ->
      CmdResp = #cmd_response{
        from_server = country_manager
      , cmd         = Cmd
      , status      = error
      , reason      = unrecognised_message
      , payload     = SomeVal
      },
      json:record_to_json(cmd_response, CmdResp)
  end,

  {ok, cowboy_req:reply(200, ?CONTENT_TYPE_JSON, JsonResp, Req), State};


init(Req, State) ->
  {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), State}.


%% =====================================================================================================================
%%
%%                                                P R I V A T E   A P I
%%
%% =====================================================================================================================


