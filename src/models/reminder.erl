-module(reminder).

-export([regenerate/2]).

%% Remove current scheduled reminders and create new ones based on channel
%% config and holidays
regenerate(Email, ChannelName) ->
  {ok, #{
     reminder_days_before := DaysBeforeList,
     reminder_time := Time,
     reminder_timezone := TimeZone
    }} = db_channel:get(Email, ChannelName),

  ok = db_reminder:clear(Email, ChannelName),
  {ok, Holidays} = db_holiday:get_channel_holidays(Email, ChannelName, [date]),
  lists:foreach(fun(#{date := Date}) ->
                    create_holiday_reminders(Email, ChannelName, Date, DaysBeforeList, Time, TimeZone)
                end, Holidays).

%%% internal
create_holiday_reminders(Email, ChannelName, HolidayDate, DaysBeforeList, Time, TimeZone) ->
  lists:foreach(fun(DaysBefore) ->
                    SendAt = build_timestamp(HolidayDate, DaysBefore, Time, TimeZone),
                    ok = db_reminder:create(Email, ChannelName, HolidayDate, SendAt)
                end, DaysBeforeList).

%% TODO create date_utils
build_timestamp(HolidayDate, DaysBefore, Time, TimeZone) ->
  {YY, MM, DD} = add_days(HolidayDate, - DaysBefore),
  list_to_binary(
    io_lib:format(<<"~B-~2..0B-~2..0B ~s~s">>, [YY, MM, DD, Time, TimeZone])).

add_days(Date, Days) ->
  Greg = calendar:date_to_gregorian_days(Date),
  calendar:gregorian_days_to_date(Greg + Days).
