-module(holidays_api_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [get_default_channels,
     update_channels].

init_per_suite(Config) ->
    %% FIXME add a create_user_with_token util, update everywhere
    #{email := Email1, password := Password1} = test_utils:create_user(#{country => <<"argentina">>}),
    #{email := Email2, password := Password2} = test_utils:create_user(#{country => <<"argentina">>}),
    #{email := Email3, password := Password3} = test_utils:create_user(#{country => <<"united states">>}),

    {ok, 200, _, #{access_token := Token1}} =
        test_utils:api_request(get, public, "/api/auth/token", <<"">>,
                               [{basic_auth, {Email1, Password1}}]),

    {ok, 200, _, #{access_token := Token2}} =
        test_utils:api_request(get, public, "/api/auth/token", <<"">>,
                               [{basic_auth, {Email2, Password2}}]),

    {ok, 200, _, #{access_token := Token3}} =
        test_utils:api_request(get, public, "/api/auth/token", <<"">>,
                               [{basic_auth, {Email3, Password3}}]),

    [{user1, Email1}, {token1, Token1},
     {user2, Email2}, {token2, Token2},
     {user3, Email3}, {token3, Token3}
     | Config].

end_per_suite(Config) ->
    ok = test_utils:delete_user(?config(user1, Config)),
    ok = test_utils:delete_user(?config(user2, Config)).

get_default_channels(Config) ->
    Token1 = ?config(token1, Config),
    Token3 = ?config(token3, Config),

    {ok, 200, _, ArgHolidays} = test_utils:api_request(get, Token1, "/api/holidays/"),
    true = lists:any(fun (Holiday) ->
                             is_same_holiday(Holiday, 7, 9, <<"Independence day">>)
                     end, ArgHolidays),

    {ok, 200, _, UsHolidays} = test_utils:api_request(get, Token3, "/api/holidays/"),
    true = lists:any(fun (Holiday) ->
                             is_same_holiday(Holiday, 7, 4, <<"Independence day">>)
                     end, UsHolidays),
    ok.

update_channels(Config) ->
    Token1 = ?config(token1, Config),
    Token2 = ?config(token1, Config),
    {Y, _, _} = erlang:date(),
    CurrentYear = integer_to_binary(Y),
    CustomDay = <<CurrentYear/binary, "-03-03">>,

    {ok, 200, _, ArgHolidays} = test_utils:api_request(get, Token1, "/api/holidays/"),

    %% Remove a holiday and add another
    ArgHolidays2 = lists:drop_while(fun (Holiday) ->
                                            is_same_holiday(Holiday, 7, 9, <<"Independece day">>)
                                    end, ArgHolidays),
    ArgHolidays3 = [#{date => CustomDay, name => <<"Custom day">>} | ArgHolidays2],
    {ok, 200, _, _} = test_utils:api_request(put, Token1, "/api/holidays/", ArgHolidays3),

    %% Check the changes were saved
    {ok, 200, _, StoredHolidays} = test_utils:api_request(get, Token1, "/api/holidays/"),
    false = lists:any(fun (Holiday) ->
                              is_same_holiday(Holiday, 7, 9, <<"Independence day">>)
                      end, StoredHolidays),
    true = lists:any(fun (Holiday) ->
                             is_same_holiday(Holiday, 3, 3, <<"Custom day">>)
                     end, StoredHolidays),

    %% Check the changes dont affect another user of the same country
    {ok, 200, _, ArgHolidays} = test_utils:api_request(get, Token2, "/api/holidays/"),
    ok.

%% helpers

is_same_holiday(Holiday, MM, DD, Name) ->
    {CurrentYear, _, _} = erlang:date(),
    Expected = list_to_binary(
                 io_lib:format(<<"~B-~2..0B-~2..0B">>, [CurrentYear, MM, DD])),
    (maps:get(date, Holiday) == Expected) and (maps:get(name, Holiday) == Name).
