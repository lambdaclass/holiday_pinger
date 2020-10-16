-module(hp_auth).

-export([token_encode/1,
         token_decode/1,
         password_hash/1,
         password_match/2,
         authenticate/2,
         reset_verification/1,
         reset_password/1,
         build_holiday_access_token/3]).

password_hash(Value) ->
  erlpass:hash(Value).

password_match(Value, Hash) ->
  erlpass:match(Value, Hash).

%% Return the user data if exits and password match.
authenticate(Email, Password) ->
  case db_user:get_with_password(Email) of
    {ok, #{verified := false}} ->
      {error, not_verified};
    {ok, User = #{password := Hash}} ->
      case password_match(Password, Hash) of
        true ->
          {ok, maps:remove(password, User)};
        false ->
          {error, unauthorized}
      end;
    _ -> {error, unauthorized}
  end.

token_encode(Data) ->
  Expiration = hp_config:get(token_expiration),
  Secret = hp_config:get(token_secret),
  jwt:encode(<<"HS256">>, maps:to_list(Data), Expiration, Secret).

token_decode(Token) ->
  Secret = hp_config:get(token_secret),
  case jwt:decode(Token, Secret) of
    {ok, Map} ->
      Map2 = maps:fold(fun (K, V, Acc) ->
                           K2 = binary_to_existing_atom(K, latin1),
                           Acc#{K2 => V}
                       end, #{}, Map),
      {ok, Map2};
    {error, Error} ->
      {error, Error}
  end.

reset_verification(Email) ->
  VerificationCode = base64url:encode(crypto:strong_rand_bytes(20)),
  db_user:reset_verification(Email, VerificationCode),
  hp_email:send_email_verification(Email, VerificationCode).

reset_password(Email) ->
  VerificationCode = base64url:encode(crypto:strong_rand_bytes(20)),
  db_user:reset_password(Email, VerificationCode),
  hp_email:send_password_reset(Email, VerificationCode).

build_holiday_access_token(Email, Name, AvatarUrl) ->
  Data = #{
    email => Email,
    name => Name,
    avatar => AvatarUrl
   },
  {ok, Token} = token_encode(Data),
  Token.
