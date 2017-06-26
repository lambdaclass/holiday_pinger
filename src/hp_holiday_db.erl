-module(hp_holiday_db).

%% will be hardcoded to argentinian holidays for now

-export([countries_with_holiday/0,
         countries_with_holiday/1]).

%% Get the list of the countries that have a holiday in the given date
%% FIXME implement
countries_with_holiday(Date) ->
    [argentina].

countries_with_holiday() ->
    countries_with_holiday(erlang:date()).
