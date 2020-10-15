-module(google_callback_handler).

%%% REST handler that receives the authentication code set by Google in the OAuth callback.
%%% If the user is new, register it with the data from Google
%%% In any case, respond with an access_token

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

  GoogleToken = get_google_access_token(Code),
  Profile = get_public_profile(GoogleToken),

  Email = get_primary_field(Profile, emailAddresses),
  Name = get_primary_field(Profile, names),
  AvatarUrl = get_primary_field(Profile, photos),

  ok = register_user(Email, Name),
  Token = hp_auth:build_holiday_access_token(Email, Name, AvatarUrl),

  Encoded = hp_json:encode(#{access_token => Token}),
  Req3 = cowboy_req:set_resp_body(Encoded, Req2),
  {true, Req3, State}.

%% internal
get_google_access_token(Code) ->
  ClientId = list_to_binary(os:getenv("GOOGLE_CLIENTID", false)),
  ClientSecret = list_to_binary(os:getenv("GOOGLE_SECRET", false)),
  GoogleBody = #{
    code => Code,
    client_id => ClientId,
    client_secret => ClientSecret,
    grant_type => << "authorization_code" >>,
    redirect_uri => << "https://holidaypinger.com/oauth/google/callback" >>
   },

  {ok, 200, _, GoogleTokenResponse} =
    hp_request:post_json(<<"https://oauth2.googleapis.com/token">>,
                         GoogleBody),
  maps:get(access_token, GoogleTokenResponse).

get_public_profile(GoogleToken) ->
  {ok, 200, _, Profile} = hp_request:get_json(<<"https://people.googleapis.com/v1/people/me?personFields=names,emailAddresses,photos">>,
                                              [{<<"Authorization">>, <<"Bearer ", GoogleToken/binary>>}]),
  Profile.

get_primary_field(Profile, Field) ->
  Fields = maps:get(Field, Profile),
  [PrimaryField | _] = lists:dropwhile(fun(#{metadata := #{primary := IsPrimary}}) -> not IsPrimary end, Fields),
  case Field of
    emailAddresses ->
      maps:get(value, PrimaryField);
    names ->
      maps:get(displayName, PrimaryField);
    photos ->
      maps:get(url, PrimaryField)
  end.

register_user(Email, Name) ->
  %% only attempt to create it if it's not already registered
  case db_user:get(Email) of
    {error, not_found} ->
      {ok, _} = db_user:create_google_user(Email,  Name),
      ok;
    {ok, _} ->
      ok
  end.
