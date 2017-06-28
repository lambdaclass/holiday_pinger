-module(user_handler).

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
    %% FIXME validate required fields, password strong enough
    UserData = hp_json:decode(Body),
    %% FIXME validate if email already registered
    {ok, _User} = db_user:create(UserData),
    {true, Req2, State}.
