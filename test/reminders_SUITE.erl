-module(reminders_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% NOTE this test is currently forcing the notifications as if today's a holiday
%% so we don't want to run it against a db with real users.
%% Will need to isolate somehow in the future

all() ->
  [send_custom_holiday_reminder,
   dont_send_days_before_when_disabled,
   send_days_before_when_set,
   reminder_limit_enforced].

init_per_suite(Config) ->
  {ok, _Apps} = application:ensure_all_started(holiday_ping),
  Config.

end_per_suite(_Config) ->
  ok = application:stop(holiday_ping),
  ok = application:unload(holiday_ping).

send_custom_holiday_reminder(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  TableId = ets_channel_table3,
  create_channel(Token, Email, <<"test_ets3">>, TableId),
  {ok, 200, _, DefaultHolidays} = test_utils:api_request(get, Token, "/api/holidays/argentina"),

  %% Remove a holiday and add another
  CustomDay = <<"1998-03-03">>,
  Updated = [#{date => CustomDay, name => <<"Custom day">>} | DefaultHolidays],
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test_ets3/holidays/", Updated),

  %% FIXME
  remind_checker:check_holidays(),
  timer:sleep(100),
  [{Email, Message}] = ets_channel:get_reminders(TableId, Email),

  Expected = list_to_binary(
               io_lib:format(<<"This is a holiday reminder: ~s will be out on ~2..0B/~2..0B/~B.">>,
                             ["John Doe", 3, 3, 1998])),
  Message = Expected,

  ok = test_utils:delete_user(Email).

dont_send_days_before_when_disabled(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  TableId = ets_channel_table4,
  create_channel(Token, Email, <<"test_ets4">>, TableId),

  %% copy holidays from argentina
  {ok, 200, _, DefaultHolidays} = test_utils:api_request(get, Token, "/api/holidays/argentina"),
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test_ets4/holidays/", DefaultHolidays),

  %% force a holiday three days before new years
  %% FIXME
  remind_checker:check_holidays(),
  timer:sleep(100),
  [] = ets_channel:get_reminders(TableId, Email),

  ok = test_utils:delete_user(Email).

send_days_before_when_set(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  TableId = ets_channel_table5,
  create_channel(Token, Email, <<"test_ets5">>, TableId, false, 3),

  %% copy holidays from argentina
  {ok, 200, _, DefaultHolidays} = test_utils:api_request(get, Token, "/api/holidays/argentina"),
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test_ets5/holidays/", DefaultHolidays),

  %% no reminder on new years
  %% FIXME
  remind_checker:check_holidays(),
  timer:sleep(100),
  [] = ets_channel:get_reminders(TableId, Email),

  %% reminder three days before new years
  %% FIXME
  remind_checker:check_holidays(),
  timer:sleep(100),

  [{Email, Message}] = ets_channel:get_reminders(TableId, Email),
  Expected = list_to_binary(
               io_lib:format(<<"This is a holiday reminder: ~s will be out on ~2..0B/~2..0B/~B.">>,
                             ["John Doe", 1, 1, 2017])),
  Message = Expected,

  ok = test_utils:delete_user(Email).

reminder_limit_enforced(_Config) ->
  {ok, CurrentLimits} = application:get_env(holiday_ping, monthly_limits),
  application:set_env(holiday_ping, monthly_limits, CurrentLimits#{ets => 1}),

  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  %% create ets channel with argentina holidays
  TableId = ets_channel_table6,
  create_channel(Token, Email, <<"test_ets6">>, TableId),
  {ok, 200, _, DefaultHolidays} = test_utils:api_request(get, Token, "/api/holidays/argentina"),
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test_ets6/holidays/", DefaultHolidays),

  %% get one reminder
  %% FIXME
  remind_checker:check_holidays(),
  timer:sleep(100),
  [{Email, _Message}] = ets_channel:get_reminders(TableId, Email),

  % create a second channel of the same type
  TableId2 = ets_channel_table7,
  create_channel(Token, Email, <<"test_ets7">>, TableId2),
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test_ets7/holidays/", DefaultHolidays),

  %% holidays not sent on new channel
  %% FIXME
  remind_checker:check_holidays(),
  timer:sleep(100),
  [] = ets_channel:get_reminders(TableId2, Email),

  application:set_env(holiday_ping, monthly_limits, CurrentLimits),
  ok.

%%% internal
create_channel(Token, Email, Name, TableId) ->
  create_channel(Token, Email, Name, TableId, true, null).

%% FIXME remove same day
create_channel(Token, Email, Name, TableId, SameDay, DaysBefore) ->
  Body = #{
    type => ets,
    configuration => #{
      email => Email,
      table_id => TableId
     },
    reminder_days_before => [3],
    reminder_time => <<"9:00">>,
    reminder_timezone => <<"+02">>
   },
  {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/" ++ Name, Body),
  ets_channel:init_table(TableId).
