-module(reminders_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% NOTE this test is currently forcing the notifications as if today's a holiday
%% so we don't want to run it against a db with real users.
%% Will need to isolate somehow in the future

all() ->
    [send_slack_reminders].

init_per_suite(Config) ->
    {ok, _Apps} = application:ensure_all_started(holiday_ping),
    #{email := Email} = test_utils:create_user(),

    TableId = ets_channel_table,
    ChannelConfig = #{email => Email, table_id => TableId},
    db_channel:create(Email, <<"test_ets">>, ets, ChannelConfig),

    [{user, Email}, {table_id, TableId} | Config].

end_per_suite(Config) ->
    ok = test_utils:delete_user(?config(user, Config)),
    ok = application:stop(holiday_ping),
    ok = application:unload(holiday_ping).

send_slack_reminders(Config) ->
    Email = ?config(user, Config),
    TableId = ?config(table_id, Config),

    %% this can't happen in the init per suite
    ets_channel:init_table(TableId),

    hp_checker:force_holidays(),
    timer:sleep(1000),
    [{Email, Message}] = ets_channel:get_reminders(TableId, Email),

    Expected = list_to_binary(
                 io_lib:format(<<"This is a holiday reminder: ~s will be out on ~2..0B/~2..0B/~B.">>,
                               ["John Doe", 1, 1, 2017])),
    Message = Expected,
    ok.
