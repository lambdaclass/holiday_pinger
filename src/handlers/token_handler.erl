-module(token_handler).

-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

%% Get user/password from basic auth and save the user to state if auth succeeds
is_authorized(Req, _State) ->
  req_utils:is_authorized(basic, Req, #{}).

content_types_provided(Req, State) ->
  {[{<<"application/json">>, to_json}], Req, State}.

%% If auth was successful generate an access token with the user data,
%% to be used to authenticate other API requests
to_json(Req, State = #{user := User}) ->
  {ok, Token} = hp_auth:access_token_encode(User),
  Body = hp_json:encode(#{access_token => Token}),
  {Body, Req, State}.
