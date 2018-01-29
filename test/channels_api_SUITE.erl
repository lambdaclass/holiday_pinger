-module(channels_api_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [create_a_channel,
     list_user_channels,
     get_single_channel,
     delete_channel,
     limit_reminders_amount,
     create_delete_channel_notification].

init_per_suite(Config) ->
    {ok, _Apps} = application:ensure_all_started(holiday_ping),

    #{email := Email, token := Token} = test_utils:create_user_with_token(),
    [{user, Email}, {token, Token} | Config].

end_per_suite(Config) ->
    ok = test_utils:delete_user(?config(user, Config)),
    ok = application:stop(holiday_ping),
    ok = application:unload(holiday_ping).

create_a_channel(Config) ->
    Token = ?config(token, Config),
    Body = #{
      type => slack,
      configuration => #{
        url => <<"http://example.com">>,
        channels => [<<"#general">>]
       },
      reminder_days_before => [0, 3],
      reminder_time => <<"9:00">>,
      reminder_timezone => <<"+02">>
     },
    {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/my_channel", Body),
    ok.

limit_reminders_amount(Config) ->
  Token = ?config(token, Config),
  Body = #{
    type => slack,
    configuration => #{
      url => <<"http://example.com">>,
      channels => [<<"#general">>]
     },
    reminder_days_before => [1, 2, 3, 4, 5, 6],
    reminder_time => <<"9:00">>,
    reminder_timezone => <<"+02">>
   },
  {ok, 400, _, _} = test_utils:api_request(put, Token, "/api/channels/my_bad", Body),
  ok.

list_user_channels(Config) ->
    Token = ?config(token, Config),
    Body = #{
      type => slack,
      configuration => #{
        url => <<"http://example.com">>,
        channels => [<<"#general">>]
       },
      reminder_days_before => [3],
      reminder_time => <<"9:00">>,
      reminder_timezone => <<"+02">>
     },
    {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/my_list_channel", Body),
    {ok, 200, _, Channels} = test_utils:api_request(get, Token, "/api/channels/"),
    true = lists:any(fun (#{name := Name}) -> Name == <<"my_list_channel">> end, Channels),
    ok.

get_single_channel(Config) ->
    Token = ?config(token, Config),
    Body = #{
      type => slack,
      configuration => #{
        url => <<"http://example.com">>,
        channels => [<<"#general">>]
       },
      reminder_days_before => [3],
      reminder_time => <<"9:00">>,
      reminder_timezone => <<"+02">>
     },
    {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/my_detail_channel", Body),
    {ok, 200, _, #{name := <<"my_detail_channel">>}} =
        test_utils:api_request(get, Token, "/api/channels/my_detail_channel"),
    ok.

delete_channel(Config) ->
    Token = ?config(token, Config),
    Body = #{
      type => slack,
      configuration => #{
        url => <<"http://example.com">>,
        channels => [<<"#general">>]
       },
      reminder_days_before => [3],
      reminder_time => <<"9:00">>,
      reminder_timezone => <<"+02">>
     },

    {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/my_delete_channel", Body),
    {ok, 204, _, _} = test_utils:api_request(delete, Token, "/api/channels/my_delete_channel"),
    {ok, 404, _, _} = test_utils:api_request(delete, Token, "/api/channels/my_delete_channel"),
    ok.

create_delete_channel_notification(Config) ->
  Token = ?config(token, Config),
  Email = ?config(user, Config),
  TableId = ets_channel_table_create_delete_notification_test,
  ets_channel:init_table(TableId),
  {{YYYY, MM, DD}, Time} = calendar:universal_time(),
  Body = #{
    type => ets,
    configuration => #{
      email => Email,
      table_id => TableId,
      channels => [<<"#general">>]
     },
    reminder_days_before => [0],
    reminder_time => hp_date:time_to_binary(Time),
    reminder_timezone => <<"+00">>
   },
  {ok, 201, _, _} = test_utils:api_request(put, Token,
                                           "/api/channels/test_ets",
                                           Body),
  ok = ktn_task:wait_for_success(
         fun() ->
             [{Email, _}] = ets_channel:get_reminders(TableId, Email),
             ok
         end,10,500),
  {ok, 204, _, _} = test_utils:api_request(delete, Token,
                                           "/api/channels/test_ets"),
  ok = ktn_task:wait_for_success(
         fun() ->
             [{Email, _}, {Email, _}] =
               ets_channel:get_reminders(TableId, Email),
             ok
         end,10,500).
