-module(slack_channel).

-export([handle/2]).

handle(Config, Message) ->
    Targets = maps:get(channels, Config),
    HookUrl = maps:get(url, Config),

    BasePayload = #{
      text => Message,
      username => get_not_empty(username, Config, <<"Holiday Reminder">>),
      icon_emoji => get_not_empty(emoji, Config, <<":calendar:">>)
     },

    lists:foreach(
      fun (Target) ->
              Payload = BasePayload#{channel => Target},
              lager:debug("Sending request to slack: ~p", [Payload]),
              {ok, 200, _, _} = hp_request:post_json(HookUrl, Payload)
      end, Targets).

get_not_empty(Key, Map, Default) ->
    Value = maps:get(Key, Map, Default),
    case re:replace(Value, "\\s+", "", [global,{return,binary}]) of
        <<"">> ->
            Default;
        _ -> Value
    end.
