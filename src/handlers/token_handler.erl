-module(token_handler).

-export([init/3,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         to_json/2]).

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

    %% FIXME properly handle unauthorized requests
    {ok, User} = db_user:authenticate(Email, Password),
    {ok, Token} = hp_auth_tokens:encode(User),

    %% set the body so the access token is returned
    Response = hp_json:encode(#{access_token => Token}),
    Req3 = cowboy_req:set_resp_body(Response, Req2),
    {true, Req3, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State) ->
    %% does nothing, the body is set in from_json
    {Req, State}.
