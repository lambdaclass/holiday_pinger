-module(reminders_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% NOTE this test is currently forcing the notifications as if today's a holiday
%% so we don't want to run it against a db with real users.
%% Will need to isolate somehow in the future

all() ->
  [send_reminders,
   send_custom_holiday_reminder,
   dont_send_removed_reminder,
   send_custom_holiday_reminder,
   dont_send_days_before_by_default,
   send_days_before_when_set].

init_per_suite(Config) ->
  {ok, _Apps} = application:ensure_all_started(holiday_ping),
  Config.

end_per_suite(_Config) ->
  ok = application:stop(holiday_ping),
  ok = application:unload(holiday_ping).

send_reminders(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  TableId = ets_channel_table,
  create_channel(Token, Email, <<"test_ets">>, TableId),

  remind_checker:force_holidays({test_utils:current_year(), 1, 1}),
  timer:sleep(1000),
  [{Email, Message}] = ets_channel:get_reminders(TableId, Email),

  Expected = list_to_binary(
               io_lib:format(<<"This is a holiday reminder: ~s will be out on ~2..0B/~2..0B/~B.">>,
                             ["John Doe", 1, 1, 2017])),
  Message = Expected,

  ok = test_utils:delete_user(Email).

dont_send_removed_reminder(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  TableId = ets_channel_table2,
  create_channel(Token, Email, <<"test_ets2">>, TableId),

  {ok, 200, _, DefaultHolidays} = test_utils:api_request(get, Token, "/api/channels/test_ets2/holidays/"),

  %% Remove a holiday and add another
  Updated = lists:filter(fun (Holiday) ->
                             not test_utils:is_same_holiday(Holiday, 1, 1)
                         end, DefaultHolidays),
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test_ets2/holidays/", Updated),

  remind_checker:force_holidays({test_utils:current_year(), 1, 1}),
  timer:sleep(1000),
  [] = ets_channel:get_reminders(TableId, Email),

  ok = test_utils:delete_user(Email).

send_custom_holiday_reminder(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  TableId = ets_channel_table3,
  create_channel(Token, Email, <<"test_ets3">>, TableId),
  {ok, 200, _, DefaultHolidays} = test_utils:api_request(get, Token, "/api/channels/test_ets3/holidays/"),

  %% Remove a holiday and add another
  CustomDay = <<"1998-03-03">>,
  Updated = [#{date => CustomDay, name => <<"Custom day">>} | DefaultHolidays],
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test_ets3/holidays/", Updated),

  remind_checker:force_holidays({1998,3,3}),
  timer:sleep(1000),
  [{Email, Message}] = ets_channel:get_reminders(TableId, Email),

  Expected = list_to_binary(
               io_lib:format(<<"This is a holiday reminder: ~s will be out on ~2..0B/~2..0B/~B.">>,
                             ["John Doe", 3, 3, 1998])),
  Message = Expected,

  ok = test_utils:delete_user(Email).

dont_send_days_before_by_default(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  TableId = ets_channel_table3,
  create_channel(Token, Email, <<"test_ets4">>, TableId),

  %% force a holiday three days before new years
  remind_checker:force_holidays({test_utils:current_year() - 1, 12, 29}),
  timer:sleep(1000),
  [] = ets_channel:get_reminders(TableId, Email),

  ok = test_utils:delete_user(Email).

send_days_before_when_set(_Config) ->
  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  TableId = ets_channel_table4,
  create_channel(Token, Email, <<"test_ets4">>, TableId),

  ReminderConfig = #{days_before => 3, same_day => false},
  {ok, 200, _, _} = test_utils:api_request(put, Token, "/api/channels/test_ets4/reminders", ReminderConfig),

  %% no reminder on new years
  remind_checker:force_holidays({test_utils:current_year(), 1, 1}),
  timer:sleep(1000),
  [] = ets_channel:get_reminders(TableId, Email),

  %% reminder three days before new years
  remind_checker:force_holidays({test_utils:current_year() - 1, 12, 29}),
  timer:sleep(1000),

  [{Email, Message}] = ets_channel:get_reminders(TableId, Email),
  Expected = list_to_binary(
               io_lib:format(<<"This is a holiday reminder: ~s will be out on ~2..0B/~2..0B/~B.">>,
                             ["John Doe", 1, 1, 2017])),
  Message = Expected,

  ok = test_utils:delete_user(Email).

%%% internal
create_channel(Token, Email, Name, TableId) ->
  Body = #{
    type => ets,
    configuration => #{
      email => Email,
      table_id => TableId
     }
   },
  {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/" ++ Name, Body),

  ChannelConfig = #{email => Email, table_id => TableId},
  db_channel:create(Email, Name, ets, ChannelConfig),
  ets_channel:init_table(TableId).
