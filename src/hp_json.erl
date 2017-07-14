-module(hp_json).

-export([encode/1,
         decode/1]).

encode(Data) ->
    jiffy:encode(Data).

%% TODO handle invalid_json case
decode(Data) ->
    jiffy:decode(Data, [return_maps]).
