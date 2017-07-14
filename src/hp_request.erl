-module(hp_request).

-export([post_json/2]).

post_json(Url, Data) ->
    Body = hp_json:encode(Data),
    hackney:post(Url, [{<<"Content-Type">>, <<"application/json">>}], Body, []).
