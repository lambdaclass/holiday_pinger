-module(db_channel).

-export([create/4,
         get/2,
         update/3,
         delete/2,
         list/1,
         get_reminders/1,
         get_id/2,
         channel_keys/0]).

channel_keys () -> [user, name, type, configuration].

create(User, Name, Type, Config) ->
  EncodedConfig = hp_json:encode(Config),
  Q = <<"INSERT INTO channels(\"user\", name, type, configuration) "
        "VALUES((SELECT id FROM users WHERE email = $1), $2, $3, $4) "
        "RETURNING name, type, configuration">>,
  case db:query(Q, [User, Name, Type, EncodedConfig]) of
    {ok, [Result | []]} -> {ok, decode_channel(Result)};
    {error, unique_violation} -> {error, channel_already_exists}
  end.

get(User, ChannelName) ->
  Q = <<"SELECT name, type, configuration FROM channels "
        "WHERE \"user\" = (SELECT id FROM users WHERE email = $1) "
        "AND name = $2">>,
  case db:query(Q, [User, ChannelName]) of
    {ok, []} -> {error, not_found};
    {ok, [Channel | []]} -> {ok, decode_channel(Channel)}
  end.

update(User, ChannelName, Config) ->
  EncodedConfig = hp_json:encode(Config),
  Q = <<"UPDATE channels SET configuration = $1 "
        "WHERE \"user\" = (SELECT id FROM users WHERE email = $2) "
        "AND name = $3">>,
  db:query(Q, [EncodedConfig, User, ChannelName]).

delete(User, ChannelName) ->
  Q = <<"DELETE FROM channels WHERE \"user\" = (SELECT id FROM users WHERE email = $1) "
        "AND name = $2">>,
  db:query(Q, [User, ChannelName]).

list(User) ->
  Q = <<"SELECT name, type, configuration FROM channels "
        "WHERE \"user\" = (SELECT id FROM users WHERE email = $1)">>,
  {ok, Results} = db:query(Q, [User]),
  {ok, lists:map(fun decode_channel/1, Results)}.

get_id(Email, ChannelName) ->
  ChannelQ = <<"SELECT id FROM channels WHERE name = $1 ",
               "AND \"user\" = (SELECT id FROM users WHERE email = $2)">>,
  case db:query(ChannelQ, [ChannelName, Email]) of
    {ok, [#{id := ChannelId}]} -> {ok, ChannelId};
    {ok, []} -> {error, channel_not_found}
  end.

get_reminders(Date) ->
  Q = <<"SELECT ch.name as channel_name, ch.type, ch.configuration, "
        "u.name as user_name, u.email, h.name as holiday_name, h.date FROM channels ch ",
        "JOIN users u ON u.id = ch.user "
        "JOIN reminder_config c ON c.channel = ch.id ",
        "JOIN channel_holidays h ON h.channel = ch.id ",
        "WHERE (c.same_day AND h.date = $1) ",
        "OR (c.days_before IS NOT NULL AND (h.date - c.days_before) = $1)">>,

  {ok, Results} = db:query(Q, [Date]),
  {ok, [{extract_user(R), extract_channel(R), extract_holiday(R)}
        || R <- Results]}.

%%% internal
extract_user(#{user_name := Name, email := Email}) ->
  #{name => Name, email => Email}.

extract_holiday(#{ holiday_name := Name, date := Date}) ->
  #{name => Name, date => Date}.

extract_channel(#{channel_name := Name, type := Type, configuration := Config}) ->
  decode_channel(#{name => Name, type => Type, configuration => Config}).

%% TODO db:results_to_map could be smart enough to figure this decoding based on the column type
decode_channel(Data = #{type := Type, configuration := Config}) ->
  Data#{
    type := binary_to_existing_atom(Type, latin1),
    configuration := hp_json:decode(Config)
   }.
