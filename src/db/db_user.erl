-module(db_user).

-export([create/4,
         get/1,
         delete/1,
         get_with_password/1,
         get_from_countries/1,
         user_keys/0]).

%% needed so atoms exist. TODO maybe put somewhere else
user_keys () -> [email, password, name, country].

create(Email, Name, Password, Country) ->
    Q = <<"INSERT INTO users(email, name, password, country)"
          "VALUES($1, $2, $3, $4) RETURNING email, name, country ">>,
    {ok, [Result | []]} = db:query(Q, [Email, Name, Password, Country]),
    {ok, Result}.

get(Email) ->
    {ok, Result} = get_with_password(Email),
    {ok, maps:remove(password, Result)}.

get_with_password(Email) ->
    Q = <<"SELECT email, name, country, password FROM users WHERE email = $1">>,
    case db:query(Q, [Email]) of
        {ok, []} -> {error, not_found};
        {ok, [User | []]} -> {ok, User}
    end.

get_from_countries(Countries) ->
    %% TODO paginate this call
    Q = <<"SELECT email, name, country FROM users WHERE country IN ($1)">>,
    Joined = lists:join(<<",">>, Countries),
    db:query(Q, [Joined]).

delete(Email) ->
    Q = <<"DELETE FROM users WHERE email = $1">>,
    db:query(Q, [Email]).
