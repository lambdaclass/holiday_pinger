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

get_not_empty(Key, Map, Default) ->
  Value = maps:get(Key, Map, Default),
  case re:replace(Value, "\\s+", "", [global,{return,binary}]) of
    <<"">> ->
      Default;
    _ -> Value
  end.
