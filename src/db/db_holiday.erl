-module(db_holiday).

-export([countries_with_holiday/0,
         countries_with_holiday/1,
         holidays_of_country/1,
         create/3,
         user_keys/0]).

%% needed so atoms exist.
user_keys () -> [country, date, name].

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
