-module(confirmation_handler).

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         from_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>],
   Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, to_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, from_json}], Req, State}.

from_json(Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  #{email := Email} = hp_json:decode(Body),

  case db_user:get_verification(Email) of
    {error, not_found} ->
      req_utils:error_response(403, <<"forbidden">>, Req2);
    {ok, #{verified := true}} ->
      req_utils:error_response(403, <<"forbidden">>, Req2);
    {ok, #{sent_seconds_ago := SecondsAgo}} when SecondsAgo < 30 ->
      req_utils:error_response(429, <<"don't push it">>, Req2);
    {ok, _} ->
      hp_auth:reset_verification(Email),
      {true, Req2, State}
  end.
