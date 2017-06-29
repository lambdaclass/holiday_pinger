-module(hp_auth).

-export([token_encode/1,
         token_decode/1,
         password_hash/1,
         password_match/2,
         authenticate/2]).

%% FIXME take from configuration/environment
-define(SECRET, <<"secret">>).
-define(EXPIRATION, 60 * 60 * 24).

token_encode(Data) ->
    jwt:encode(<<"HS256">>, maps:to_list(Data), ?EXPIRATION, ?SECRET).

token_decode(Token) ->
    maps:from_list(jwt:decode(Token, ?SECRET)).

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
