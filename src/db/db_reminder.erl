-module(db_reminder).

-export([set_default_reminder_config/1,
         get_reminder_config/1,
         update_reminder_config/3,
         get_reminders/1,
         reminder_keys/0]).

reminder_keys() -> [user, same_day, days_before, holiday_date, reminder_date].

set_default_reminder_config(Email) ->
    Q = <<"INSERT INTO reminder_config(\"user\", same_day, days_before) "
          "VALUES((SELECT id from users WHERE email = $1), TRUE, NULL)">>,
    db:query(Q, [Email]).

get_reminder_config(Email) ->
    Q = <<"SELECT same_day, days_before from reminder_config "
          "WHERE \"user\" = (SELECT id from users WHERE email = $1)">>,
    {ok, [Config]} = db:query(Q, [Email]),
    {ok, Config}.

update_reminder_config(Email, SameDay, DaysBefore) ->
    Q = <<"UPDATE reminder_config SET same_day = $1, days_before = $2 "
          "WHERE \"user\" = (SELECT id FROM users WHERE email = $3) ">>,
    db:query(Q, [SameDay, DaysBefore, Email]).

get_reminders(Date) ->
    Q = <<"SELECT u.name as user_name, u.email, h.name as holiday_name, h.date FROM users u ",
          "JOIN reminder_config c ON c.user = u.id ",
          "JOIN user_holidays h ON h.user = u.id ",
          "WHERE (c.same_day AND h.date = $1) ",
          "OR (c.days_before IS NOT NULL AND (h.date - c.days_before) = $1)">>,

    {ok, Results} = db:query(Q, [Date]),
    {ok, [{extract_user(R), extract_holiday(R)} || R <- Results]}.

%%% internal
extract_user(#{user_name := Name, email := Email}) ->
    #{name => Name, email => Email}.

extract_holiday(#{ holiday_name := Name, date := Date}) ->
    #{name => Name, date => Date}.
