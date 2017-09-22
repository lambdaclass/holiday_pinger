-module(webhook_channel).

-export([handle/4]).

handle(User, Date, Config, Message) ->
  HookUrl = maps:get(url, Config),
  {YY, MM, DD} = Date,
  DateString = list_to_binary(io_lib:format(<<"~B-~2..0B-~2..0B">>, [YY, MM, DD])),
  Payload = #{
    name => maps:get(name, User),
    email => maps:get(email, User),
    date => DateString,
    message => Message
   },

  Headers = get_headers(Config, Payload),
  lager:debug("Sending webhook request: ~p ~p", [HookUrl, Payload]),
  {ok, 200, _, _} = hp_request:post_json(HookUrl, Payload, Headers).

%%% internal
get_headers(#{secret := Secret}, Payload) ->
  JsonPayload = hp_json:encode(Payload),
  Digest = base64:encode(crypto:hmac(sha256, Secret, JsonPayload)),
  [{<<"X-Holiday-Signature">>, Digest}];
get_headers(_, _Payload) ->
  [].
