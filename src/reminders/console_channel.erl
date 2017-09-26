-module(console_channel).

-export([handle/4]).

handle(_User, _Date, _Config, Message) ->
  lager:info(Message),
  {ok, [<<"console">>]}.
