-module(github_auth_handler).

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

from_json(Req, State ) ->
    {ok, Body, Req2} = cowboy_req:body(Req),

    #{code := Code, country := Country} = hp_json:decode(Body),
    LCountry = string:lowercase(Country),

    GithubToken = get_github_access_token(Code),
    Profile = get_public_profile(GithubToken),
    Email = get_primary_email(GithubToken),

    ok = register_user(Email, LCountry, Profile),
    Token = build_holiday_access_token(Email, LCountry, Profile),
    RespBody = hp_json:encode(#{access_token => Token}),
    Req3 = cowboy_req:set_resp_body(RespBody, Req2),
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

register_user(Email, Country, GithubProfile) ->
    % FIXME implement. first update database
    ok.

build_holiday_access_token(Email, Country, GithubProfile) ->
    UserData = #{
      email => Email,
      country => Country,
      name => maps:get(name, GithubProfile),
      avatar => maps:get(avatar_url, GithubProfile)
     },

    %% generate holiday ping access token
    {ok, Token} = hp_auth:token_encode(UserData),
    Token.
