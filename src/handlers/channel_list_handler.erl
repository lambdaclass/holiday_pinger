-module(channel_list_handler).

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         is_authorized/2,
         to_json/2,
         from_json/2]).

%% TODO move to req_utils along with basic auth
is_authorized(Req, State) ->
    Fail = {false, <<"Bearer realm=\"holidayping\"">>},
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {ok, {<<"bearer">>, Token}, Req2} ->
            case hp_auth:token_decode(Token) of
                {ok, #{<<"email">> := User}} ->
                    {true, Req2, #{user => User}};
               _ -> {Fail, Req2, State}
            end;
        _ -> {Fail, Req, State}
    end.

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State = #{user := User}) ->
    {ok, Body} = db_channel:list(User),
    {hp_json:encode(Body), Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

from_json(Req, State = #{user := User}) ->
    {ok, Body, Req2} = cowboy_req:body(Req),

    % TODO validate input fields
    #{
       <<"name">> := Name,
       <<"type">> := Type,
       <<"configuration">> := Config
     } = hp_json:decode(Body),

    {ok, _} = db_channel:create(User, Name, Type, Config),
    {{true, <<"/api/channels/", Name/binary>>}, Req2, State}.
