-module(db_reminder).

-export([save/4,
         get_monthly_count/2,
         get_recent/2,
         reminder_keys/0,
         sent_count/3]).

%% needed so atoms exist.
reminder_keys() -> [user, channel, channel_type, target, timestamp].

get_monthly_count(Email, ChannelType) ->
  Q = <<"SELECT count(*) FROM sent_reminders "
        "WHERE \"user\" = (SELECT id from \"users\" WHERE email = $1) "
        "AND channel_type = $2 "
        "AND date_part('month', timestamp) = $3">>,
  {_, MM, _} = erlang:date(),
  {ok, [#{count := Count}]} = db:query(Q, [Email, ChannelType, MM]),
  {ok, Count}.

sent_count(Email, ChannelName, Date) ->
  Q = <<"SELECT count(*) FROM sent_reminders "
        "WHERE channel = (SELECT id from channels WHERE name = $1 AND \"user\" = (SELECT id from \"users\" where email = $2)) "
        "AND timestamp::date = $3 "
        "AND test = false">>,
  {ok, [#{count := Count}]} = db:query(Q, [ChannelName, Email, Date]),
  {ok, Count}.

get_recent(Email, ChannelName) ->
  Q = <<"SELECT to_char(timestamp::date, 'YYYY-MM-DD') as date FROM sent_reminders "
        "WHERE channel = (SELECT id from channels WHERE name = $1 "
        "AND test = false "
        "AND \"user\" = (SELECT id from \"users\" WHERE email = $2))"
        "ORDER BY timestamp DESC LIMIT 10">>,
  db:query(Q, [ChannelName, Email]).

save(Email, ChannelName, Targets, IsTest) ->
  IdQ = <<"SELECT id FROM \"users\" WHERE email = $1">>,
  {ok, [#{id := UserId}]} = db:query(IdQ, [Email]),

  ChannelQ = <<"SELECT id, type FROM channels WHERE \"user\" = $1 AND name = $2">>,
  {ok, [#{id := ChannelId, type := ChannelType}]} = db:query(ChannelQ, [UserId, ChannelName]),

  Values = [io_lib:format(<<"(~p, ~p, '~s', '~s', ~p)">>, [UserId, ChannelId, ChannelType, db:escape_string(Target), IsTest])
            || Target <- Targets],
  Values2 = lists:join(<<", ">>, Values),

  Q = [<<"INSERT INTO sent_reminders(\"user\", channel, channel_type, target, test) VALUES ">>,
       Values2],
  Q2 = iolist_to_binary(Q),
  db:query(Q2, []).
