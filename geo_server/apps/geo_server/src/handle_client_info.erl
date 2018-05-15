-module(handle_client_info).
-behavior(cowboy_handler).

-export([init/2]).

-include("../include/macros/default_http_response.hrl").

init(Req=#{method := <<"GET">>}, State) -> {ok, ?CLIENT_INFO_RESPONSE(Req), State};
init(Req, State)                        -> {ok, ?METHOD_NOT_ALLOWED_RESPONSE(Req), State}.


