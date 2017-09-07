-module(db_user).

-export([create_holiday/4,
         create_github/3,
         get/1,
         exists/1,
         delete/1,
         get_with_password/1,
         get_from_countries/1,
         user_keys/0]).

%% needed so atoms exist.
user_keys () -> [email, password, name, country].

create_holiday(Email, Name, Password, Country) ->
    Q = <<"INSERT INTO users(email, name, password, country, auth_type)"
          "VALUES($1, $2, $3, $4, 'holiday') RETURNING email, name, country ">>,
    case db:query(Q, [Email, Name, Password, Country]) of
        {ok, [Result | []]} -> {ok, Result};
        {error, unique_violation} -> {error, user_already_exists}
    end.

create_github(Email, Name, Country) ->
    Q = <<"INSERT INTO users(email, name, country, auth_type)"
          "VALUES($1, $2, $3, 'github') RETURNING email, name, country ">>,
    case db:query(Q, [Email, Name, Country]) of
        {ok, [Result | []]} -> {ok, Result};
        {error, unique_violation} -> {error, user_already_exists}
    end.

get(Email) ->
    {ok, Result} = get_with_password(Email),
    {ok, maps:remove(password, Result)}.

get_with_password(Email) ->
    Q = <<"SELECT email, name, country, password FROM users "
          "WHERE email = $1 AND auth_type = 'holiday'">>,
    case db:query(Q, [Email]) of
        {ok, []} -> {error, not_found};
        {ok, [User | []]} -> {ok, User}
    end.

exists(Email) ->
    Q = <<"SELECT count(id) FROM users WHERE email = $1">>,
    case db:query(Q, [Email]) of
        {ok, [#{count := 0}]} -> false;
        {ok, [#{count := _Count}]} -> true
    end.

get_from_countries([]) ->
    {ok, []};
get_from_countries(Countries) ->
    %% TODO paginate this call
    Q = [<<"SELECT email, name, country FROM users WHERE country IN">>,
         <<"('">>, lists:join(<<"', '">>, Countries), <<"')">>],
    Joined = iolist_to_binary(Q),
    db:query(Joined, []).

delete(Email) ->
    Q = <<"DELETE FROM users WHERE email = $1">>,
    db:query(Q, [Email]).
