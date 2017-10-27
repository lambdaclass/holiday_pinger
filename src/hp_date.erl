-module(hp_date).

-export([date_to_binary/1,
         time_to_binary/1,
         today_binary/0,
         add_days/2,
         add_minutes/2,
         current_year/0]).

date_to_binary({YYYY, MM, DD}) ->
  list_to_binary(io_lib:format(<<"~B-~2..0B-~2..0B">>, [YYYY, MM, DD])).

time_to_binary({H, M, S}) ->
  list_to_binary(io_lib:format(<<"~2..0B:~2..0B:~2..0B">>, [H, M, S])).

today_binary() ->
  date_to_binary(erlang:date()).

add_days(Date, Days) ->
  Greg = calendar:date_to_gregorian_days(Date),
  calendar:gregorian_days_to_date(Greg + Days).

add_minutes(Time, Minutes) ->
  Greg = calendar:datetime_to_gregorian_seconds({erlang:date(), Time}),
  Seconds = Minutes * 60,
  {_NewDate, NewTime} = calendar:gregorian_seconds_to_datetime(Greg + Seconds),
  NewTime.

current_year() ->
  {CurrentYear, _, _} = erlang:date(),
  CurrentYear.
