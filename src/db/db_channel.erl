-module(db_channel).

-export([create/7,
         get/2,
         update/6,
         delete/2,
         list/1,
         get_id/2,
         decode_channel/1,
         channel_keys/0]).

channel_keys () ->
  [user, name, type, configuration, same_day, reminder_days_before, reminder_time, reminder_timezone].

create(User, Name, Type, Config, DaysBefore, Time, TZ) ->
  EncodedConfig = hp_json:encode(Config),
  Q = <<"INSERT INTO channels(\"user\", name, type, configuration, reminder_days_before, reminder_time, reminder_timezone) "
        "VALUES((SELECT id FROM users WHERE email = $1), $2, $3, $4, $5, to_timestamp($6, 'HH24:MI')::time, $7) "
        "RETURNING name, type, configuration">>,
  case db:query(Q, [User, Name, Type, EncodedConfig, DaysBefore, Time, TZ]) of
    {ok, [Result | []]} -> {ok, decode_channel(Result)};
    {error, unique_violation} -> {error, channel_already_exists}
  end.

get(User, ChannelName) ->
  Q = <<"SELECT name, type, configuration, reminder_days_before, to_char(reminder_time, 'HH24:MI') as reminder_time, reminder_timezone FROM channels "
        "WHERE \"user\" = (SELECT id FROM users WHERE email = $1) "
        "AND name = $2">>,
  case db:query(Q, [User, ChannelName]) of
    {ok, []} -> {error, not_found};
    {ok, [Channel | []]} -> {ok, decode_channel(Channel)}
  end.

update(User, ChannelName, Config, DaysBefore, Time, TZ) ->
  EncodedConfig = hp_json:encode(Config),
  Q = <<"UPDATE channels SET configuration = $1, reminder_days_before = $2, reminder_time = to_timestamp($3, 'HH24:MI')::time, reminder_timezone = $4 "
        "WHERE \"user\" = (SELECT id FROM users WHERE email = $5) "
        "AND name = $6">>,
  db:query(Q, [EncodedConfig, DaysBefore, Time, TZ, User, ChannelName]).

delete(User, ChannelName) ->
  Q = <<"DELETE FROM channels WHERE \"user\" = (SELECT id FROM users WHERE email = $1) "
        "AND name = $2">>,
  db:query(Q, [User, ChannelName]).

list(User) ->
  Q = <<"SELECT name, type, configuration, reminder_days_before, to_char(reminder_time, 'HH24:MI') as reminder_time, reminder_timezone FROM channels "
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

decode_channel(Data = #{type := Type, configuration := Config}) ->
  Data#{
    type := binary_to_existing_atom(Type, latin1),
    configuration := hp_json:decode(Config)
   }.
