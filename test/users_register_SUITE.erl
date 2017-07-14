-module(users_register_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [register_valid_user,
     fail_register_on_missing_fields,
     fail_register_on_invalid_fields,
     fail_register_on_email_already_registered].

init_per_suite(Config) ->
    {ok, _Apps} = application:ensure_all_started(holiday_ping),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(holiday_ping),
    ok = application:unload(holiday_ping).

%% User registration
register_valid_user(_Config) ->
    Email = test_utils:unique_email(),
    Body = #{
      email => Email,
      name => <<"John Doe">>,
      password => <<"S3cr3t!!">>,
      country => <<"argentina">>
     },

    {ok, 201, _, _} = test_utils:api_request(post, public, "/api/users", Body),

    ok = test_utils:delete_user(Email).

fail_register_on_missing_fields(_Config) ->
    Email = test_utils:unique_email(),
    Body = #{
      email => Email,
      name => <<"John Doe">>,
      password => <<"S3cr3t!!">>,
      country => <<"argentina">>
     },

    {ok, 400, _, #{<<"message">> := <<"Missing required fields">>}} =
        test_utils:api_request(post, public, "/api/users", maps:remove(email, Body)),
    {ok, 400, _, #{<<"message">> := <<"Missing required fields">>}} =
        test_utils:api_request(post, public, "/api/users", maps:remove(name, Body)),
    {ok, 400, _, #{<<"message">> := <<"Missing required fields">>}} =
        test_utils:api_request(post, public, "/api/users", maps:remove(password, Body)),
    {ok, 400, _, #{<<"message">> := <<"Missing required fields">>}} =
        test_utils:api_request(post, public, "/api/users", maps:remove(country, Body)),
    ok.

fail_register_on_invalid_fields(_Config) ->
    %% TODO start validating :P
    ok.

fail_register_on_email_already_registered(_Config) ->
    Email = test_utils:unique_email(),
    Body = #{
      email => Email,
      name => <<"John Doe">>,
      password => <<"S3cr3t!!">>,
      country => <<"argentina">>
     },

    {ok, 201, _, _} = test_utils:api_request(post, public, "/api/users", Body),
    {ok, 409, _, #{<<"message">> := <<"User already exists">>}} =
        test_utils:api_request(post, public, "/api/users", Body),

    ok = test_utils:delete_user(Email).
