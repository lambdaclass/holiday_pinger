-module(google_redirect_handler).

%%% HTTP handler that redirects the user to the OAuth endpoint of Google
%%% with the client_id that identifies this application

-export([init/3,
         handle/2,
         terminate/3]).

init(_Type, Req, []) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  case os:getenv("GOOGLE_CLIENTID", false) of
    false ->
      lager:error(<<"Google credentials not found in environment">>);
    GoogleClientId ->
      GoogleUrl = [<<"https://accounts.google.com/o/oauth2/auth">>,
                   <<"?response_type=code&redirect_uri=https://holidaypinger.com/oauth/google/callback&scope=email profile&client_id=">>, GoogleClientId],
      {ok, Req2} = cowboy_req:reply(303, [{<<"location">>, GoogleUrl}], Req),
      {ok, Req2, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.
