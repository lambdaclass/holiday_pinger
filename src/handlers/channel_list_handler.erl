-module(channel_list_handler).

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

is_authorized(Req, _State) ->
    req_utils:is_authorized(bearer, Req, #{}).

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State = #{user := User}) ->
    {ok, Body} = db_channel:list(User),
    {hp_json:encode(Body), Req, State}.
