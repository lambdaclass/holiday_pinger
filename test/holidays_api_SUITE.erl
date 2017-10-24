-module(holidays_api_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
  [get_country_holidays,
   update_holidays].

init_per_suite(Config) ->
  {ok, _Apps} = application:ensure_all_started(holiday_ping),

  #{email := Email1, token := Token1} = test_utils:create_user_with_token(#{country => <<"argentina">>}),
  #{email := Email2, token := Token2} = test_utils:create_user_with_token(#{country => <<"argentina">>}),
  #{email := Email3, token := Token3} = test_utils:create_user_with_token(#{country => <<"united states">>}),

  [{user1, Email1}, {token1, Token1},
   {user2, Email2}, {token2, Token2},
   {user3, Email3}, {token3, Token3} | Config].

end_per_suite(Config) ->
  ok = test_utils:delete_user(?config(user1, Config)),
  ok = test_utils:delete_user(?config(user2, Config)),
  ok = test_utils:delete_user(?config(user3, Config)).

get_country_holidays(Config) ->
  Token1 = ?config(token1, Config),

  {ok, 200, _, ArgHolidays} = test_utils:api_request(get, Token1, "/api/holidays/argentina"),
  true = lists:any(fun (Holiday) ->
                       test_utils:is_same_holiday(Holiday, 7, 9, <<"Independence day">>)
                   end, ArgHolidays),

  {ok, 200, _, UsHolidays} = test_utils:api_request(get, Token1, "/api/holidays/united states"),
  true = lists:any(fun (Holiday) ->
                       test_utils:is_same_holiday(Holiday, 7, 4, <<"Independence day">>)
                   end, UsHolidays),
  ok.

update_holidays(Config) ->
  Token1 = ?config(token1, Config),
  Token2 = ?config(token2, Config),

  create_channel(Token1, "ch11"),
  create_channel(Token2, "ch11"),

  {Y, _, _} = erlang:date(),
  CurrentYear = integer_to_binary(Y),
  CustomDay = <<CurrentYear/binary, "-03-03">>,

  %% use argentina holidays as base
  {ok, 200, _, ArgHolidays} = test_utils:api_request(get, Token1, "/api/holidays/argentina"),

  %% Remove a holiday and add another
  ArgHolidays2 = lists:filter(fun (Holiday) ->
                                  not test_utils:is_same_holiday(Holiday, 7, 9, <<"Independence day">>)
                              end, ArgHolidays),
  ArgHolidays3 = [#{date => CustomDay, name => <<"Custom day">>} | ArgHolidays2],
  {ok, 200, _, _} = test_utils:api_request(put, Token1, "/api/channels/ch11/holidays/", ArgHolidays3),

  %% Check the changes were saved
  {ok, 200, _, StoredHolidays} = test_utils:api_request(get, Token1, "/api/channels/ch11/holidays/"),
  false = lists:any(fun (Holiday) ->
                        test_utils:is_same_holiday(Holiday, 7, 9, <<"Independence day">>)
                    end, StoredHolidays),
  true = lists:any(fun (Holiday) ->
                       test_utils:is_same_holiday(Holiday, 3, 3, <<"Custom day">>)
                   end, StoredHolidays),

  %% Check the changes dont affect another user of the same country
  {ok, 200, _, []} = test_utils:api_request(get, Token2, "/api/channels/ch11/holidays/"),
  ok.

%%% internal
create_channel(Token, Name) ->
  Body = #{
    type => slack,
    configuration => #{
      url => <<"http://example.com">>,
      channels => [<<"#general">>]
     },
    reminder_days_before => [0],
    reminder_time => <<"9:00">>,
    reminder_timezone => <<"+02">>
   },
  {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/" ++ Name, Body),
  Body.
