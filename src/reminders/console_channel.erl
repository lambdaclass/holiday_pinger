-module(console_channel).

-export([handle/2]).

handle(_Config, Message) ->
    lager:info(Message).
