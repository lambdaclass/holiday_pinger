-module(db_holiday).

-export([countries_with_holiday/0,
         countries_with_holiday/1]).

%% TODO take from db or somewhere else
-define(HOLIDAYS, #{
          <<"argentina">> => [{1, 1},
                              {2, 27},
                              {2, 28},
                              {3, 24},
                              {4, 2},
                              {4, 13},
                              {4, 14},
                              {5, 1},
                              {5, 25},
                              {6, 17},
                              {6, 20},
                              {7, 9},
                              {11, 20},
                              {12, 8},
                              {12, 25}]
         }).

%% Get the list of the countries that have a holiday in the given date
countries_with_holiday(Date) ->
    {_Y, M, D} = Date,
    CountryMap = maps:filter(fun (_K, Holidays) -> lists:member({M, D}, Holidays) end, ?HOLIDAYS),
    maps:keys(CountryMap).

countries_with_holiday() ->
    countries_with_holiday(erlang:date()).
