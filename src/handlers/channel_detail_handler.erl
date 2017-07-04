-module(channel_detail_handler).

-export([init/3,
         rest_init/2,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         content_types_accepted/2,
         is_authorized/2,
         to_json/2,
         delete_resource/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Name, Req2} = cowboy_req:binding(name, Req),
    State = #{name => Name},
    {ok, Req2, State}.

is_authorized(Req, State) ->
    req_utils:is_authorized(bearer, Req, State).

%% TODO add PUT. in fact creation should be primaily made via PUT instead of POST
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"DELETE">>],
     Req, State}.

resource_exists(Req, State = #{user := User, name := Name}) ->
    case db_channel:get(User, Name) of
        {ok, Channel} -> {true, Req, State#{channel => Channel}};
        _ -> {false, Req, State}
    end.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State = #{channel := Channel}) ->
    Body = hp_json:encode(Channel),
    {Body, Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

delete_resource(Req, State = #{user := User, name := Name}) ->
    ok = db_channel:delete(User, Name),
    {true, Req, State}.
