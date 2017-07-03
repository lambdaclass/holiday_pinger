-module(slack_channel).

-export([handle/2]).

handle(#{type := slack}, Message) ->
    lager:info(Message).
