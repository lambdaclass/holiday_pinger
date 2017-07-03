-module(console_channel).

-export([handle/2]).

handle(#{type := console}, Message) ->
    lager:info(Message).
