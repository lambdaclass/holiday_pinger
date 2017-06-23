-module(hp_auth_tokens).

-export([encode/1,
         decode/1]).

%% FIXME take from configuration/environment
-define(SECRET, <<"secret">>).
-define(EXPIRATION, 60 * 60 * 24).

encode(Data) ->
    jwt:encode(<<"HS256">>, Data, ?EXPIRATION, ?SECRET).

decode(Token) ->
    jwt:decode(Token, ?SECRET).
