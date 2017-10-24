-module(db_reminder).

-export([create/4,
         clear/2,
         log/4,
         get_monthly_count/2,
         get_recent/2,
         reminder_keys/0,
         get_scheduled/0]).

%% needed so atoms exist.
reminder_keys() -> [user, channel, channel_type, target, timestamp].

create(Email, ChannelName, HolidayDate, SendAtString) ->
  Q = <<"WITH user_id AS (SELECT id FROM users WHERE email = $1), "
        "channel_id AS (SELECT id FROM channels WHERE channels.user = (SELECT id FROM user_id) AND name = $2), "
        "holiday_id AS (SELECT id FROM channel_holidays WHERE channel = (SELECT id FROM channel_id) AND date = $3) "
        "INSERT INTO scheduled_reminders(holiday, send_at) VALUES((SELECT id FROM holiday_id), '",
        %% epgsql wont allow a date string, but postgres does
        SendAtString/binary, "')">>,
  db:query(Q, [Email, ChannelName, HolidayDate]).

clear(Email, ChannelName) ->
  Q = <<"WITH user_id AS (SELECT id FROM users WHERE email = $1), "
        "channel_id AS (SELECT id FROM channels WHERE channels.user = (SELECT id FROM user_id) AND name = $2), "
        "holiday_ids AS (SELECT id FROM channel_holidays WHERE channel = (SELECT id FROM channel_id)) "
        "DELETE FROM scheduled_reminders WHERE holiday IN (SELECT id FROM holiday_ids)">>,
  db:query(Q, [Email, ChannelName]).

get_monthly_count(Email, ChannelType) ->
  Q = <<"SELECT count(*) FROM sent_reminders_log "
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
  Q = <<"SELECT to_char(timestamp::date, 'YYYY-MM-DD') as date FROM sent_reminders_log "
        "WHERE channel = (SELECT id from channels WHERE name = $1 "
        "AND test = false "
        "AND \"user\" = (SELECT id from \"users\" WHERE email = $2))"
        "ORDER BY timestamp DESC LIMIT 10">>,
  db:query(Q, [ChannelName, Email]).

log(Email, ChannelName, Targets, IsTest) ->
  IdQ = <<"SELECT id FROM \"users\" WHERE email = $1">>,
  {ok, [#{id := UserId}]} = db:query(IdQ, [Email]),

  ChannelQ = <<"SELECT id, type FROM channels WHERE \"user\" = $1 AND name = $2">>,
  {ok, [#{id := ChannelId, type := ChannelType}]} = db:query(ChannelQ, [UserId, ChannelName]),

  Values = [io_lib:format(<<"(~p, ~p, '~s', '~s', ~p)">>, [UserId, ChannelId, ChannelType, db:escape_string(Target), IsTest])
            || Target <- Targets],
  Values2 = lists:join(<<", ">>, Values),

  Q = [<<"INSERT INTO sent_reminders_log(\"user\", channel, channel_type, target, test) VALUES ">>,
       Values2],
  Q2 = iolist_to_binary(Q),
  db:query(Q2, []).

get_scheduled() ->
  Q = <<"SELECT ch.name as channel_name, ch.type, ch.configuration, "
        "u.name as user_name, u.email, h.name as holiday_name, h.date FROM scheduled_reminders r ",
        "JOIN channel_holidays h ON h.id = r.holiday "
        "JOIN channels ch ON ch.id = h.channel "
        "JOIN users u ON u.id = ch.user "
        "WHERE now() > r.send_at AND r.send_at > now() - interval '12 hours'">>,
  {ok, Results} = db:query(Q, []),
  {ok, [{extract_user(R), extract_channel(R), extract_holiday(R)}
        || R <- Results]}.

%%% internal
extract_user(#{user_name := Name, email := Email}) ->
  #{name => Name, email => Email}.

extract_holiday(#{ holiday_name := Name, date := Date}) ->
  #{name => Name, date => Date}.

extract_channel(#{channel_name := Name, type := Type, configuration := Config}) ->
  db_channel:decode_channel(#{name => Name, type => Type, configuration => Config}).
