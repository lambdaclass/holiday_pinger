-module(provider_redirect_handler).

%%% HTTP handler that redirects the user to the OAuth endpoint of the corresponding
%%% provider with the client_id that identifies this application

-export([init/3,
         handle/2,
         terminate/3]).

init(_Type, Req, []) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {Provider, _} = cowboy_req:binding(provider, Req),
  {EnvVar, ProviderUrl} = provider_vars(binary_to_list(Provider)),

  case os:getenv(EnvVar, false) of
    false ->
      lager:error([Provider, <<" credentials not found in environment">>]);
    ClientId ->
      Url = [ProviderUrl | ClientId],
      {ok, Req2} = cowboy_req:reply(303, [{<<"location">>, Url}], Req),
      {ok, Req2, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.

provider_vars("google") ->
    {"GOOGLE_CLIENTID",
    [<<"https://accounts.google.com/o/oauth2/auth">>,
    <<"?response_type=code&redirect_uri=https://holidaypinger.com/oauth/google/callback&scope=email profile&client_id=">>]};

provider_vars("github") ->
    {"GITHUB_CLIENTID",
    [<<"http://github.com/login/oauth/authorize">>,
    <<"?scope=user:email&client_id=">>]}.
