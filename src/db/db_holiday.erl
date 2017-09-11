-module(db_holiday).

-export([holidays_of_country/1,
         create/3,
         get_user_holidays/1,
         set_user_holidays/2,
         set_default_holidays/2,
         holiday_keys/0]).

%% needed so atoms exist.
holiday_keys () -> [country, date, name, user].

holidays_of_country(Country) ->
  Q = <<"SELECT name, date FROM holidays WHERE country = $1">>,
  db:query(Q, [Country]).

create(Country, Date, Name) ->
  Q = <<"INSERT INTO holidays(country, date, name) "
        "VALUES($1, $2, $3) RETURNING country, date, name">>,
  case db:query(Q, [Country, Date, Name]) of
    {ok, [Result | []]} -> {ok, Result};
    {error, unique_violation} -> {error, holiday_already_exists}
  end.

get_user_holidays(Email) ->
  Q = <<"SELECT to_char(date, 'YYYY-MM-DD') as date, name FROM user_holidays "
        "WHERE \"user\" = (SELECT id FROM users WHERE email = $1) "
        "ORDER BY date">>,
  db:query(Q, [Email]).

set_default_holidays(Email, Country) ->
  Q = <<"INSERT INTO user_holidays(\"user\", date, name) "
        "SELECT (SELECT id FROM users WHERE email = $1), date, name FROM holidays "
        "WHERE country = $2">>,
  db:query(Q, [Email, Country]).

set_user_holidays(Email, Holidays) ->
  UserQ = <<"SELECT id FROM users WHERE email = $1">>,
  {ok, [#{id := UserId}]} = db:query(UserQ, [Email]),

  Values = [io_lib:format(<<"(~p, '~s', '~s')">>, [UserId, Date, db:escape_string(Name)])
            || #{date := Date, name := Name} <- Holidays],
  Values2 = lists:join(<<", ">>, Values),

  Q = [<<"INSERT INTO user_holidays(\"user\", date, name) VALUES ">>,
       Values2,
       <<" RETURNING date, name">>],
  Q2 = iolist_to_binary(Q),

  DeleteQ = <<"DELETE FROM user_holidays WHERE \"user\" = $1">>,
  case db:query(DeleteQ, [UserId]) of
    ok -> db:query(Q2, [])
  end.
