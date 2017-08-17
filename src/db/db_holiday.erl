-module(db_holiday).

-export([countries_with_holiday/0,
         countries_with_holiday/1,
         holidays_of_country/1,
         create/3,
         get_user_holidays/1,
         set_user_holidays/2,
         holiday_keys/0]).

%% needed so atoms exist.
holiday_keys () -> [country, date, name, user].

%% Get the list of the countries that have a holiday in the given date
countries_with_holiday(Date) ->
  Q = <<"SELECT country FROM holidays WHERE date = $1">>,
  {ok, Results} = db:query(Q, [Date]),
  lists:map(fun (R) -> maps:get(country, R) end, Results).

countries_with_holiday() ->
    countries_with_holiday(erlang:date()).

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
    QDefault = <<"SELECT to_char(date, 'YYYY-MM-DD') as date, name FROM holidays "
                 "WHERE country = (SELECT country FROM users WHERE email = $1) "
                 "ORDER BY date">>,

    case db:query(Q, [Email]) of
        {ok, []} ->
            db:query(QDefault, [Email]);
        R -> R
    end.

set_user_holidays(Email, Holidays) ->
    UserQ = <<"SELECT id FROM users WHERE email = $1">>,
    {ok, [#{id := UserId}]} = db:query(UserQ, [Email]),

    Values = [io_lib:format(<<"(~p, '~s', '~s')">>, [UserId, Date, Name]) ||
                 #{date := Date, name := Name} <- Holidays],
    Values2 = lists:join(<<", ">>, Values),

    Q = [<<"INSERT INTO user_holidays(\"user\", date, name) VALUES ">>,
         Values2, <<" RETURNING date, name">>],
    Q2 = iolist_to_binary(Q),

    lager:debug("Inserting user holidays ~p", [Q2]),
    case db:query(Q2, []) of
        {ok, Result} -> {ok, Result};
        {error, unique_violation} -> {error, holiday_already_exists}
    end.
