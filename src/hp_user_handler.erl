-module(hp_user_handler).

-export([init/3,
         allowed_methods/2,
         content_types_accepted/2,
         from_json/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

from_json(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    %% TODO validate required fields, password strong enough
    #{<<"email">> := Email, <<"password">> := Password} = hp_json:decode(Body),
    {ok, _User} = hp_user_db:create(Email, Password, argentina),
    {true, Req2, State}.
