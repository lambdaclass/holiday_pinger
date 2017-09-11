-module(hp_auth).

-export([access_token_encode/1,
         access_token_decode/1,
         registration_token_encode/1,
         registration_token_decode/1,
         password_hash/1,
         password_match/2,
         authenticate/2]).

access_token_encode(Data) ->
  Expiration = hp_config:get(token_expiration),
  token_encode(<<"access_token">>, Expiration, Data).

access_token_decode(Token) ->
  token_decode(<<"access_token">>, Token).

registration_token_encode(Data) ->
  Expiration = hp_config:get(registration_token_expiration),
  token_encode(<<"registration_token">>, Expiration, Data).

registration_token_decode(Token) ->
  token_decode(<<"registration_token">>, Token).

password_hash(Value) ->
  erlpass:hash(Value).

password_match(Value, Hash) ->
  erlpass:match(Value, Hash).

%% Return the user data if exits and password match.
authenticate(Email, Password) ->
  case db_user:get_with_password(Email) of
    {ok, User = #{password := Hash}} ->
      case password_match(Password, Hash) of
        true -> {ok, maps:remove(password, User)};
        false -> {error, unauthorized}
      end;
    _ -> {error, unauthorized}
  end.

%%% internal
token_encode(Type, Expiration, Data) ->
  Secret = hp_config:get(token_secret),
  ListData = maps:to_list(Data#{ <<"token_type">> => Type }),
  jwt:encode(<<"HS256">>, ListData, Expiration, Secret).

token_decode(Type, Token) ->
  Secret = hp_config:get(token_secret),
  case jwt:decode(Token, Secret) of
    {ok, #{<<"token_type">> := Type} = Data} ->
      {ok, maps:remove(<<"token_type">>, Data)};
    _ ->
      {error, invalid_token}
  end.
