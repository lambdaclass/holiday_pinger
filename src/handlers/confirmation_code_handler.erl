-module(confirmation_code_handler).

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
  #{email := Email, code := Code} = hp_json:decode(Body),
  ExpirationSeconds = hp_config:get(verification_code_expiration),

  case db_user:get_verification(Email) of
    {error, not_found} ->
      req_utils:error_response(403, <<"forbidden">>, Req2);
    {ok, #{verified := true}} ->
      req_utils:error_response(403, <<"forbidden">>, Req2);
    {ok, #{verification_code := Code, sent_seconds_ago := SecondsAgo}}
      when SecondsAgo > ExpirationSeconds ->
      req_utils:error_response(401, <<"verification code expired">>, Req2);
    {ok, #{verification_code := Code}} ->
      db_user:set_verified(Email),
      {true, Req2, State};
    {ok, _UserWithAnotherCode} ->
      req_utils:error_response(403, <<"forbidden">>, Req2)
  end.
