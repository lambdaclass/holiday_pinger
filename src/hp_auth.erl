-module(hp_auth).

-export([token_encode/1,
         token_decode/1,
         password_hash/1,
         password_match/2,
         authenticate/2]).

token_encode(Data) ->
    Secret = hp_config:get(token_secret),
    Expiration = hp_config:get(token_expiration),
    jwt:encode(<<"HS256">>, maps:to_list(Data), Expiration, Secret).

token_decode(Token) ->
    Secret = hp_config:get(token_secret),
    jwt:decode(Token, Secret).

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
