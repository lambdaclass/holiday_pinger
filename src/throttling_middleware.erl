-module(throttling_middleware).

-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  {Path, Req2} = cowboy_req:path(Req),
  {{IP, _}, Req3} = cowboy_req:peer(Req2),

  %% convert to list to be able to ignore traling slashes and path segments
  PathList = binary:split(Path, <<"/">>, [global, trim_all]),

  case check(PathList, IP) of
    {limit_exceeded, _, _} ->
      lager:warning("IP ~p exceeded limit for path ~p", [IP, PathList]),
      {error, 429, Req3};
    _ ->
      {ok, Req3, Env}
  end.

%%% internal
check([<<"api">>, <<"users">>], IP) ->
  throttle:check(register_rate, IP);
check([<<"api">>, <<"users">>, <<"password">>], IP) ->
  throttle:check(password_reset_rate, IP);
check([<<"api">>, <<"users">>, <<"confirmation">>], IP) ->
  throttle:check(resend_confirmation_rate, IP);
check([<<"api">>, <<"channels">>, _, <<"test">>], IP) ->
  throttle:check(test_channel_rate, IP);
check([<<"api">> | _], IP) ->
  throttle:check(global_api_rate, IP);
check(_, _) ->
  ok.
