-module(reminders_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% NOTE this test is currently forcing the notifications as if today's a holiday
%% so we don't want to run it against a db with real users.
%% Will need to isolate somehow in the future

all() ->
  [send_custom_holiday_reminder,
   dont_send_days_before_when_disabled,
   send_days_before_when_set].

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

  remind_checker:force_holidays({1998,3,3}),
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
  remind_checker:force_holidays({test_utils:current_year() - 1, 12, 29}),
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
  remind_checker:force_holidays({test_utils:current_year(), 1, 1}),
  timer:sleep(100),
  [] = ets_channel:get_reminders(TableId, Email),

  %% reminder three days before new years
  remind_checker:force_holidays({test_utils:current_year() - 1, 12, 29}),
  timer:sleep(100),

  [{Email, Message}] = ets_channel:get_reminders(TableId, Email),
  Expected = list_to_binary(
               io_lib:format(<<"This is a holiday reminder: ~s will be out on ~2..0B/~2..0B/~B.">>,
                             ["John Doe", 1, 1, 2017])),
  Message = Expected,

  ok = test_utils:delete_user(Email).

%%% internal
create_channel(Token, Email, Name, TableId) ->
  create_channel(Token, Email, Name, TableId, true, null).

create_channel(Token, Email, Name, TableId, SameDay, DaysBefore) ->
  Body = #{
    type => ets,
    configuration => #{
      email => Email,
      table_id => TableId
     },
    same_day => SameDay,
    days_before => DaysBefore
   },
  {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/" ++ Name, Body),
  ets_channel:init_table(TableId).
