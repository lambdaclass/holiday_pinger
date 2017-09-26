-module(db_reminder).

-export([save/3,
         get_current_count/2,
         reminder_keys/0]).

%% needed so atoms exist.
reminder_keys() -> [user, channel, channel_type, target, timestamp].

get_current_count(Email, ChannelType) ->
  Q = <<"SELECT count(*) FROM sent_reminders "
        "WHERE \"user\" = (SELECT id from \"users\" WHERE email = $1) "
        "AND channel_type = $2 "
        "AND date_part('month', timestamp) = $3">>,
  {_, MM, _} = erlang:date(),
  {ok, [#{count := Count}]} = db:query(Q, [Email, ChannelType, MM]),
  {ok, Count}.

save(Email, ChannelName, Targets) ->
  IdQ = <<"SELECT id FROM \"users\" WHERE email = $1">>,
  {ok, [#{id := UserId}]} = db:query(IdQ, [Email]),

  ChannelQ = <<"SELECT id, type FROM channels WHERE \"user\" = $1 AND name = $2">>,
  {ok, [#{id := ChannelId, type := ChannelType}]} = db:query(ChannelQ, [UserId, ChannelName]),


  Values = [io_lib:format(<<"(~p, ~p, '~s', '~s')">>, [UserId, ChannelId, ChannelType, db:escape_string(Target)])
            || Target <- Targets],
  Values2 = lists:join(<<", ">>, Values),

  Q = [<<"INSERT INTO sent_reminders(\"user\", channel, channel_type, target) VALUES ">>,
       Values2],
  Q2 = iolist_to_binary(Q),
  db:query(Q2, []).
