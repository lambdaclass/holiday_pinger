-module(channel_test_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
  [test_channel].

init_per_suite(Config) ->
  {ok, _Apps} = application:ensure_all_started(holiday_ping),

  #{email := Email, token := Token} = test_utils:create_user_with_token(),

  [{user, Email}, {token, Token} | Config].

end_per_suite(Config) ->
  ok = test_utils:delete_user(?config(user, Config)),
  ok = application:stop(holiday_ping),
  ok = application:unload(holiday_ping).

test_channel(Config) ->
  Token = ?config(token, Config),
  Email = ?config(user, Config),

  TableId = ets_channel_test_table,
  ets_channel:init_table(TableId),

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
  {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/my_channel", Body),
  {ok, 204, _, _} = test_utils:api_request(post, Token, "/api/channels/my_channel/test", #{}),

  timer:sleep(1000),
  [{Email, Message}] = ets_channel:get_reminders(TableId, Email),
  <<"This is a Holiday Ping test: John Doe will be out on holidays.">> = Message,

  ok = test_utils:delete_user(Email).
