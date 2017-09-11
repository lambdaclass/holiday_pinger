-module(hp_config).

-export([get/1]).

get(Key) ->
  {ok, Value} = application:get_env(holiday_ping, Key),
  Value.
