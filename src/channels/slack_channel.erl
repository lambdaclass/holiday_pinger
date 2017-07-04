-module(slack_channel).

-export([handle/2]).

handle(Config, Message) ->
    %% FIXME Config keys should be atoms, not binaries, fix hp_json decode
    Payload = #{
      channel => maps:get(<<"channel">>, Config),
      %% need to make it binary for proper json encoding, if all channels require this just do it on message building
      text => Message,
      username => maps:get(<<"username">>, Config, <<"Holiday Reminder">>),
      icon_emoji => maps:get(<<"emoji">>, Config, <<":calendar:">>)
     },

    JsonPayload = hp_json:encode(Payload),
    HookUrl = maps:get(<<"url">>, Config),
    lager:debug("Sending request to slack: ~p", [Payload]),
    hackney:post(HookUrl, [{<<"Content-Type">>, <<"application/json">>}], JsonPayload, []).
