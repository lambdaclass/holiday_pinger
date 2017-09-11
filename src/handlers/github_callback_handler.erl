-module(github_callback_handler).

%%% REST handler that receives the authentication code set by GitHub in the OAuth callback.
%%% If the user was registered before and it's Logging in, return an access_token.
%%% If the user is new, return a registration callback that can be used to fill
%%% the rest of the profile (i.e. the Country).

-export([init/3,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, to_json}], Req, State}.

from_json(Req, State) ->
  {ok, Body, Req2} = cowboy_req:body(Req),

  #{code := Code} = hp_json:decode(Body),

  GithubToken = get_github_access_token(Code),
  Profile = get_public_profile(GithubToken),
  Email = get_primary_email(GithubToken),

  %% return a different token according to the registration status of the user
  RespBody = case db_user:get(Email) of
               {ok, User} ->
                 Token = build_holiday_access_token(User, Profile),
                 #{new_user => false, access_token => Token};
               {error, not_found} ->
                 Token = build_registration_token(Email, Profile),
                 #{new_user => true, registration_token => Token}
             end,

  Encoded = hp_json:encode(RespBody),
  Req3 = cowboy_req:set_resp_body(Encoded, Req2),
  {true, Req3, State}.


%% internal
get_github_access_token(Code) ->
  ClientId = list_to_binary(os:getenv("GITHUB_CLIENTID", false)),
  ClientSecret = list_to_binary(os:getenv("GITHUB_SECRET", false)),
  GithubBody = #{
    code => Code,
    client_id => ClientId,
    client_secret => ClientSecret
   },

  {ok, 200, _, GithubTokenResponse} =
    hp_request:post_json(<<"https://github.com/login/oauth/access_token">>,
                         GithubBody),
  maps:get(access_token, GithubTokenResponse).

get_public_profile(GithubToken) ->
  {ok, 200, _, Profile} = hp_request:get_json(<<"https://api.github.com/user">>,
                                              [{<<"Authorization">>, <<"token ", GithubToken/binary>>}]),
  Profile.

get_primary_email(GithubToken) ->
  {ok, 200, _, Emails} = hp_request:get_json(<<"https://api.github.com/user/emails">>,
                                             [{<<"Authorization">>, <<"token ", GithubToken/binary>>}]),
  [Primary | _] = lists:dropwhile(fun(#{primary := IsPrimary}) ->
                                      not IsPrimary
                                  end, Emails),
  maps:get(email, Primary).

build_holiday_access_token(User, #{ avatar_url := Avatar}) ->
  {ok, Token} = hp_auth:access_token_encode(User#{avatar => Avatar}),
  Token.

build_registration_token(Email, GithubProfile) ->
  UserData = #{
    email => Email,
    name => maps:get(name, GithubProfile),
    avatar => maps:get(avatar_url, GithubProfile)
   },
  {ok, Token} = hp_auth:registration_token_encode(UserData),
  Token.
