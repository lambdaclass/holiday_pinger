-module(slack_channel).

-export([handle/4]).

handle(_User, _Date, Config, Message) ->
  Targets = maps:get(channels, Config),
  HookUrl = maps:get(url, Config),

  BasePayload = #{
    text => Message,
    username => maps:get(username, Config, <<"Holiday Reminder">>),
    icon_emoji => maps:get(emoji, Config, <<":calendar:">>)
   },

  send_to_slack(HookUrl, BasePayload, Targets).

%%% internal
send_to_slack(HookUrl, Payload, Targets) when is_list(Targets), length(Targets) > 0 ->
  lists:foreach(
    fun (Target) -> do_send(HookUrl, Payload#{channel => Target}) end,
    Targets
   );
send_to_slack(HookUrl, Payload, _) ->
  %% don't set the channel, use configured default
  do_send(HookUrl, Payload).

do_send(HookUrl, Payload) ->
  lager:debug("Sending request to slack: ~p", [Payload]),
  {ok, 200, _, _} = hp_request:post_json(HookUrl, Payload).
