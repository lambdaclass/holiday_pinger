-module(hp_json).

-export([encode/1,
         decode/1]).

encode(Data) ->
    jiffy:encode(Data).

decode(Data) ->
    jiffy:decode(Data, [return_maps]).
