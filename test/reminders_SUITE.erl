-module(reminders_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [].

init_per_suite(Config) ->
    Email = [<<"user">>, erlang:unique_integer(), <<"@example.com">>],
    {ok, _} = db_user:create(Email, <<"John Doe">>, <<"S3cr3t!!">>, <<"argentina">>),
    [{user, Email} | Config].

end_per_suite(Config) ->
    Email = ?config(user, Config),
    db_user:delete(Email),
    ok.

send_slack_reminders() ->
    % create a mock channel
    % assert that the channel was called with the expected arguments
    [].
