-module(hp_user_storage).

-export([create/3,
         authenticate/2,
         get_from_country/1]).

create(Email, Password, Country) ->
    {ok, #{email => Email,
           country => Country}}.

authenticate(Email, Password) ->
    {ok, #{email => <<"user@fake.com">>,
           country => argentina}}.

get_from_country(Country) ->
    [].
