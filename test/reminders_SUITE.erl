-module(reminders_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% NOTE this test is currently forcing the notifications as if today's a holiday
%% so we don't want to run it against a db with real users.
%% Will need to isolate somehow in the future

all() ->
  [sent_on_holiday_date,
   not_sent_before_scheduled_time,
   sent_before_holiday,
   not_sent_twice,
   dont_resend_after_regeneration,
   dont_resend_days_before_after_regeneration,
   timezone_honored,
   channel_properly_called,
   monthly_limit_enforced
].

init_per_suite(Config) ->
  {ok, _Apps} = application:ensure_all_started(holiday_ping),
  Config.

end_per_suite(_Config) ->
  ok = application:stop(holiday_ping),
  ok = application:unload(holiday_ping).

sent_on_holiday_date(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  create_channel(Token, <<"test1">>),
  Today =  hp_date:today_binary(),
  Holidays = [#{name => <<"today holiday">>, date => Today}],
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test1/holidays/", Holidays),

  %% get sent reminders, should be empty
  {ok, 200, _, []} = test_utils:api_request(get, Token, "/api/channels/test1/reminders/"),

  remind_checker:check_holidays(),
  timer:sleep(100),

  %% get sent reminders, should have one reminder
  {ok, 200, _, [Reminder]} = test_utils:api_request(get, Token, "/api/channels/test1/reminders/"),
  #{date := Today} = Reminder,

  ok = test_utils:delete_user(Email).

not_sent_before_scheduled_time(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  %% set the reminder time in 20 minutes from now
  {_Date, Time} = calendar:universal_time(),
  NewTime = hp_date:add_minutes(Time, 20),
  create_channel(Token, <<"test2">>, [0], NewTime, <<"+00">>),

  Today =  hp_date:today_binary(),
  Holidays = [#{name => <<"today holiday">>, date => Today}],
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test2/holidays/", Holidays),

  %% get sent reminders, should be empty
  {ok, 200, _, []} = test_utils:api_request(get, Token, "/api/channels/test2/reminders/"),

  remind_checker:check_holidays(),
  timer:sleep(100),

  %% get sent reminders, shouldbe still empty
  {ok, 200, _, []} = test_utils:api_request(get, Token, "/api/channels/test2/reminders/"),

  ok = test_utils:delete_user(Email).

sent_before_holiday(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  {Today, Now} = calendar:universal_time(),
  create_channel(Token, <<"test3">>, [2], Now, <<"+00">>),
  HolidayDate = hp_date:add_days(Today, 2),
  Holidays = [#{name => <<"today holiday">>, date => hp_date:date_to_binary(HolidayDate)}],
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test3/holidays/", Holidays),

  %% get sent reminders, should be empty
  {ok, 200, _, []} = test_utils:api_request(get, Token, "/api/channels/test3/reminders/"),

  remind_checker:check_holidays(),
  timer:sleep(100),

  %% get sent reminders, should have one reminder
  {ok, 200, _, [Reminder]} = test_utils:api_request(get, Token, "/api/channels/test3/reminders/"),
  TodayBin = hp_date:today_binary(),
  #{date := TodayBin} = Reminder,

  ok = test_utils:delete_user(Email).

not_sent_twice(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  create_channel(Token, <<"test4">>),
  Today =  hp_date:today_binary(),
  Holidays = [#{name => <<"today holiday">>, date => Today}],
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test4/holidays/", Holidays),

  %% get sent reminders, should be empty
  {ok, 200, _, []} = test_utils:api_request(get, Token, "/api/channels/test4/reminders/"),

  remind_checker:check_holidays(),
  timer:sleep(100),

  %% get sent reminders, should have one reminder
  {ok, 200, _, [Reminder]} = test_utils:api_request(get, Token, "/api/channels/test4/reminders/"),
  #{date := Today} = Reminder,

  %% run checker again, should not send more reminders
  remind_checker:check_holidays(),
  timer:sleep(100),
  {ok, 200, _, [Reminder]} = test_utils:api_request(get, Token, "/api/channels/test4/reminders/"),

  ok = test_utils:delete_user(Email).

dont_resend_after_regeneration(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  create_channel(Token, <<"test4b">>),
  Today =  hp_date:today_binary(),
  Holidays = [#{name => <<"today holiday">>, date => Today}],
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test4b/holidays/", Holidays),

  %% get sent reminders, should be empty
  {ok, 200, _, []} = test_utils:api_request(get, Token, "/api/channels/test4b/reminders/"),

  remind_checker:check_holidays(),
  timer:sleep(100),

  %% get sent reminders, should have one reminder
  {ok, 200, _, [Reminder]} = test_utils:api_request(get, Token, "/api/channels/test4b/reminders/"),
  #{date := Today} = Reminder,

  %% update holidays again to force reminder regeneration
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test4b/holidays/", Holidays),

  %% run checker again, should not send more reminders
  remind_checker:check_holidays(),
  timer:sleep(100),
  {ok, 200, _, [Reminder]} = test_utils:api_request(get, Token, "/api/channels/test4b/reminders/"),

  ok = test_utils:delete_user(Email).

dont_resend_days_before_after_regeneration(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  {Today, Now} = calendar:universal_time(),
  create_channel(Token, <<"test">>, [1], Now, <<"+00">>),
  HolidayDate = hp_date:add_days(Today, 1),
  Holidays = [#{name => <<"tomorrow holiday">>, date => hp_date:date_to_binary(HolidayDate)}],
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test/holidays/", Holidays),

  %% get sent reminders, should be empty
  {ok, 200, _, []} = test_utils:api_request(get, Token, "/api/channels/test/reminders/"),

  remind_checker:check_holidays(),
  timer:sleep(100),

  %% get sent reminders, should have one reminder
  {ok, 200, _, [Reminder]} = test_utils:api_request(get, Token, "/api/channels/test/reminders/"),
  TodayBin = hp_date:today_binary(),
  #{date := TodayBin} = Reminder,

  %% update holidays again to force reminder regeneration
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test/holidays/", Holidays),

  %% run checker again, should not send more reminders
  remind_checker:check_holidays(),
  timer:sleep(100),
  {ok, 200, _, [Reminder]} = test_utils:api_request(get, Token, "/api/channels/test/reminders/"),

  ok = test_utils:delete_user(Email).

timezone_honored(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  %% Use the same clock time as UTC, but with UTC - 1 => still not time to send it
  {_Date, Time} = calendar:universal_time(),
  create_channel(Token, <<"test5">>, [0], Time, <<"-01">>),

  Today =  hp_date:today_binary(),
  Holidays = [#{name => <<"today holiday">>, date => Today}],
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test5/holidays/", Holidays),

  %% should not send reminders
  remind_checker:check_holidays(),
  timer:sleep(100),
  {ok, 200, _, []} = test_utils:api_request(get, Token, "/api/channels/test5/reminders/"),

  %% now with the same tz, set time as current utc + 1 => it should send reminder
  NewTime = hp_date:add_minutes(Time, -60),
  create_channel(Token, <<"test6">>, [0], NewTime, <<"-01">>),
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test6/holidays/", Holidays),

  remind_checker:check_holidays(),
  timer:sleep(100),
  {ok, 200, _, [Reminder]} = test_utils:api_request(get, Token, "/api/channels/test6/reminders/"),
  #{date := Today} = Reminder,

  ok = test_utils:delete_user(Email).

channel_properly_called(_Config) ->
  %% use an ets channel to inspect the payload received by the channel handler
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  TableId = ets_channel_table,
  {{YYYY, MM, DD}, Time} = calendar:universal_time(),
  Body = #{
    type => ets,
    configuration => #{
      email => Email,
      table_id => TableId
     },
    reminder_days_before => [0],
    reminder_time => hp_date:time_to_binary(Time),
    reminder_timezone => <<"+00">>
   },
  {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/test_ets", Body),
  ets_channel:init_table(TableId),

  Holidays = [#{name => <<"today holiday">>, date => hp_date:today_binary()}],
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test_ets/holidays/", Holidays),

  remind_checker:check_holidays(),
  timer:sleep(100),
  {ok, 200, _, [_Reminder]} = test_utils:api_request(get, Token, "/api/channels/test_ets/reminders/"),
  [{Email, Message}] = ets_channel:get_reminders(TableId, Email),
  Message = <<"This is a holiday reminder: John Doe will be out today.">>,

  ok = test_utils:delete_user(Email).

monthly_limit_enforced(_Config) ->
  {ok, CurrentLimits} = application:get_env(holiday_ping, monthly_limits),
  application:set_env(holiday_ping, monthly_limits, CurrentLimits#{console => 1}),

  #{token := Token} = test_utils:create_user_with_token(),

  %% create a console channel with today holiday
  create_channel(Token, <<"test8">>),

  Today =  hp_date:today_binary(),
  Holidays = [#{name => <<"today holiday">>, date => Today}],
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test8/holidays/", Holidays),

  %% get one reminder
  remind_checker:check_holidays(),
  timer:sleep(100),
  {ok, 200, _, [_Reminder]} = test_utils:api_request(get, Token, "/api/channels/test8/reminders/"),

  % create a second channel of the same type and holidays
  create_channel(Token, <<"test9">>),
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test9/holidays/", Holidays),

  %% holidays not sent on new channel
  remind_checker:check_holidays(),
  timer:sleep(100),
  {ok, 200, _, []} = test_utils:api_request(get, Token, "/api/channels/test9/reminders/"),

  application:set_env(holiday_ping, monthly_limits, CurrentLimits),
  ok.

%%% internal
create_channel(Token, Name) ->
  {_Date, Time} = calendar:universal_time(),
  TZ = <<"+00">>,
  create_channel(Token, Name, [0], Time, TZ).

create_channel(Token, Name, DaysBefore, Time, TimeZone) ->
  TimeBin = hp_date:time_to_binary(Time),
  Body = #{
    type => console,
    configuration => #{},
    reminder_days_before => DaysBefore,
    reminder_time => TimeBin,
    reminder_timezone => TimeZone
   },
  {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/" ++ Name, Body).
