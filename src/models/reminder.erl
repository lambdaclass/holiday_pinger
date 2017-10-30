-module(reminder).

-export([regenerate/2,
         is_already_sent/2,
         check_limit/2,
         log_reminders/4]).

%% Remove current scheduled reminders and create new ones based on channel
%% config and holidays.
regenerate(Email, ChannelName) ->
  {ok, #{
     reminder_days_before := DaysBeforeList,
     reminder_time := Time,
     reminder_timezone := TimeZone
    }} = db_channel:get(Email, ChannelName),

  ok = db_reminder:clear(Email, ChannelName),
  {ok, Holidays} = db_holiday:get_upcoming_holidays(Email, ChannelName),
  lists:foreach(fun(#{date := Date}) ->
                    create_holiday_reminders(Email, ChannelName, Date, DaysBeforeList, Time, TimeZone)
                end, Holidays).

is_already_sent(Email, ChannelName) ->
  %% the channels currently return ok if all their reminders were sent, so if
  %% one exists we can safely assume the entire pack was sent.
  case db_reminder:sent_count(Email, ChannelName, erlang:date()) of
    {ok, 0} -> false;
    {ok, _} -> true
  end.

check_limit(Email, Type) ->
  ChannelLimits = hp_config:get(monthly_limits),
  case maps:get(Type, ChannelLimits, undefined) of
    Limit when is_integer(Limit), Limit > 0 ->
      case db_reminder:get_monthly_count(Email, Type) of
        {ok, Count} when Count >= Limit -> limit_exceeded;
        _ -> ok
      end;
    _ -> ok
  end.

log_reminders(Email, ChannelName, Targets, IsTest) ->
  db_reminder:log(Email, ChannelName, Targets, IsTest).

%%% internal
create_holiday_reminders(Email, ChannelName, HolidayDate, DaysBeforeList, Time, TimeZone) ->
  lists:foreach(fun(DaysBefore) ->
                    SendAt = build_timestamp(HolidayDate, DaysBefore, Time, TimeZone),
                    ok = db_reminder:create(Email, ChannelName, HolidayDate, SendAt)
                end, DaysBeforeList).

build_timestamp(HolidayDate, DaysBefore, TimeBin, TimeZoneBin) ->
  Date = hp_date:add_days(HolidayDate, - DaysBefore),
  DateBin = hp_date:date_to_binary(Date),
  <<DateBin/binary, " ", TimeBin/binary, TimeZoneBin/binary>>.
