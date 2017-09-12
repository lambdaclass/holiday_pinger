-module(db_reminder).

%% TODO this module will probably die soon

-export([set_default_reminder_config/2,
         get_reminder_config/2,
         update_reminder_config/4,
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
