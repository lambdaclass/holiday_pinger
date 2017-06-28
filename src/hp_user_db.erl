-module(hp_user_db).

-export([create/3,
         authenticate/2,
         get_from_countries/1]).

%% FIXME implement this stuff

create(Email, Password, Country) ->
    {ok, #{email => Email,
           country => Country}}.

authenticate(Email, Password) ->
    {ok, #{email => <<"user@fake.com">>,
           country => argentina}}.

get_from_countries(Countries) ->
    [#{
        id => 1,
        email => "jesus@example.com",
        country => argentina
      },
     #{
        id => 2,
        email => "contoso@example.com",
        country => argentina
      }].
