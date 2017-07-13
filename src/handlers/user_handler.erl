-module(user_handler).

-export([init/3,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         resource_exists/2,
         from_json/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

resource_exists(Req, State) ->
    {false, Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

from_json(Req, _State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),

    %% FIXME add stronger validations (email is email, password strong enough, valid country, etc)
    case hp_json:decode(Body) of
        #{
           <<"email">> := Email,
           <<"name">> := Name,
           <<"password">> := Password,
           <<"country">> := Country
         } ->
            PasswordHash = hp_auth:password_hash(Password),
            %% FIXME validate if email already registered
            {ok, _User} = db_user:create(Email, Name, PasswordHash, Country),
            {{true, "/api/channels"}, Req2, []};
        _ -> req_utils:error_response(<<"Missing required fields">>, Req2)
    end.
