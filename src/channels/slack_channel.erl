-module(slack_channel).

-export([handle/2]).

handle(Config, Message) ->
    Payload = #{
      channel => maps:get(channel, Config),
      text => Message,
      username => maps:get(username, Config, <<"Holiday Reminder">>),
      icon_emoji => maps:get(emoji, Config, <<":calendar:">>)
     },

    HookUrl = maps:get(url, Config),
    lager:debug("Sending request to slack: ~p", [Payload]),
    {ok, 200, _, _} = hp_request:post_json(HookUrl, Payload).
