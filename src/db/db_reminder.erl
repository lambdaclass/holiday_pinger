-module(db_reminder).

-export([set_default_reminder_config/2,
         get_reminder_config/2,
         update_reminder_config/4,
         get_reminders/1,
         reminder_keys/0]).

reminder_keys() -> [user, same_day, days_before, holiday_date, reminder_date].

set_default_reminder_config(Email, Channel) ->
  case db_channel:get_id(Email, Channel) of
    {ok, ChannelId} ->
      Q = <<"INSERT INTO reminder_config(channel, same_day, days_before) "
            "VALUES($1, TRUE, NULL)">>,
      db:query(Q, [ChannelId]);
    Error -> Error
  end.

get_reminder_config(Email, Channel) ->
  case db_channel:get_id(Email, Channel) of
    {ok, ChannelId} ->
      Q = <<"SELECT same_day, days_before from reminder_config "
            "WHERE channel = $1">>,
      {ok, [Config]} = db:query(Q, [ChannelId]),
      {ok, Config};
    Error -> Error
  end.

update_reminder_config(Email, Channel, SameDay, DaysBefore) ->
  case db_channel:get_id(Email, Channel) of
    {ok, ChannelId} ->
      Q = <<"UPDATE reminder_config SET same_day = $1, days_before = $2 "
            "WHERE channel = $3">>,
      db:query(Q, [SameDay, DaysBefore, ChannelId]);
    Error -> Error
  end.

get_reminders(Date) ->
  Q = <<"SELECT ch.name as channel_name, ch.type, ch.configuration, "
        "u.name as user_name, u.email, h.name as holiday_name, h.date FROM channels ch ",
        "JOIN users u ON u.id = ch.user"
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
  #{name => Name, type => Type, configuration => Config}.
