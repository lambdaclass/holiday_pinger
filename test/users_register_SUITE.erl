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
    Email = unique_email(),
    Body = #{
      email => Email,
      name => <<"John Doe">>,
      password => <<"S3cr3t!!">>,
      country => <<"argentina">>
     },

    {ok, 201, _, _} = api_request(post, "/api/users",Body),

    %% FIXME delete user via API, not db
    ok = db_user:delete(Email),
    ok.

fail_register_on_missing_fields(_Config) ->
    ok.

fail_register_on_invalid_fields(_Config) ->
    ok.

fail_register_on_email_already_registered(_Config) ->
    ok.

%% TODO move to some test utils
unique_email() ->
    "test_user" ++ ktn_random:string(5) ++ "@example.com".

api_request(post, Path, Data) ->
    %% TODO make url configurable
    Port = erlang:integer_to_list(hp_config:get(port)),
    Url = "http://localhost:" ++ Port ++ Path,
    Body = hp_json:encode(Data),
    hackney:post(Url, [{<<"Content-Type">>, <<"application/json">>}], Body, []).
