-module(slack_channel).

-export([handle/2]).

handle(Channel = #{type := slack}, Message) ->
    Payload = #{
      channel => maps:get(channel, Channel),
      %% need to make it binary for proper json encoding, if all channels require this just do it on message building
      text => Message,
      username => maps:get(username, Channel, <<"Holiday Reminder">>),
      icon_emoji => maps:get(emoji, Channel, <<":calendar:">>)
     },

    JsonPayload = hp_json:encode(Payload),
    HookUrl = maps:get(url, Channel),
    lager:debug("Sending request to slack: ~p", [Payload]),
    hackney:post(HookUrl, [{<<"Content-Type">>, <<"application/json">>}], JsonPayload, []).
