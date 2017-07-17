-module(hp_json).

-export([encode/1,
         decode/1]).

encode(Data) ->
    jsx:encode(Data).

%% TODO handle invalid_json case
decode(Data) ->
    jsx:decode(Data, [{labels, attempt_atom}, return_maps]).
