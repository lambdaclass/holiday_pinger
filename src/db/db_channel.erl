-module(db_channel).

-export([create/4,
         get/2,
         delete/2,
         list/1,
         channel_keys/0]).

channel_keys () -> [user, name, type, configuration].

create(User, Name, Type, Config) ->
    EncodedConfig = hp_json:encode(Config),
    Q = <<"INSERT INTO channels(\"user\", name, type, configuration) "
          "VALUES((SELECT id FROM users WHERE email = $1), $2, $3, $4) "
          "RETURNING name, type, configuration">>,
    {ok, [Result | []]} = db:query(Q, [User, Name, Type, EncodedConfig]),
    {ok, decode_config(Result)}.

get(User, ChannelName) ->
    Q = <<"SELECT name, type, configuration FROM channels "
          "WHERE \"user\" = (SELECT id FROM users WHERE email = $1) "
          "AND name = $2">>,
    case db:query(Q, [User, ChannelName]) of
        {ok, []} -> {error, not_found};
        {ok, [Channel | []]} -> {ok, decode_config(Channel)}
    end.

%% TODO add update

delete(User, ChannelName) ->
    Q = <<"DELETE FROM channels WHERE \"user\" = (SELECT id FROM users WHERE email = $1) "
          "AND name = $2">>,
    db:query(Q, [User, ChannelName]).

list(User) ->
    Q = <<"SELECT name, type, configuration FROM channels "
          "WHERE \"user\" = (SELECT id FROM users WHERE email = $1)">>,
    {ok, Results} = db:query(Q, [User]),
    {ok, lists:map(fun decode_config/1, Results)}.

%%% internal
%% TODO db:results_to_map could be smart enough to figure this decoding based on the column type
decode_config(Data = #{configuration := Config}) ->
    Data#{configuration := hp_json:decode(Config)}.
