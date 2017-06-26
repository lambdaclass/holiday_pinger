-module(hp_reminder_storage).

-export([is_already_sent/4,
         is_already_sent/3,
         set_sent/4,
         set_sent/3]).

is_already_sent(User, Channel, HolidayDate, ReminderDate) ->
    false.

is_already_sent(User, Channel, HolidayDate) ->
    is_already_sent(User, Channel, HolidayDate, erlang:date()).

set_sent(#{id := UserId}, #{id := ChannelId}, HolidayDate, ReminderDate) ->
    Data = #{
      user_id => UserId,
      channel_id => ChannelId,
      holiday_date => HolidayDate,
      reminder_date => ReminderDate
     },
    io:format("Setting reminder as sent ~p~n", [Data]),
    ok.

set_sent(User, Channel, HolidayDate) ->
    set_sent(User, Channel, HolidayDate, erlang:date()).
