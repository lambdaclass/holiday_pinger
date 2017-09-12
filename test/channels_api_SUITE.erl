-module(channels_api_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [create_a_channel,
     list_user_channels,
     get_single_channel,
     delete_channel].

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
      same_day => true,
      days_before => 3
     },
    {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/my_channel", Body),
    ok.

list_user_channels(Config) ->
    Token = ?config(token, Config),
    Body = #{
      type => slack,
      configuration => #{
        url => <<"http://example.com">>,
        channels => [<<"#general">>]
       },
      same_day => true,
      days_before => 3
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
      same_day => true,
      days_before => 3
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
      same_day => true,
      days_before => 3
     },

    {ok, 201, _, _} = test_utils:api_request(put, Token, "/api/channels/my_delete_channel", Body),
    {ok, 204, _, _} = test_utils:api_request(delete, Token, "/api/channels/my_delete_channel"),
    {ok, 404, _, _} = test_utils:api_request(delete, Token, "/api/channels/my_delete_channel"),
    ok.
