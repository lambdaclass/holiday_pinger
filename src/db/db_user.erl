-module(db_user).

-export([create/4,
         authenticate/2,
         get_from_countries/1,
         user_keys/0]).

%% needed so atoms exist. TODO maybe put somewhere else
user_keys () -> [email, password, name, country].

%% FIXME move password stuff to the controller
create(Email, Name, Password, Country) ->
    Q = <<"INSERT INTO users(email, name, password, country)"
          "VALUES($1, $2, $3, $4) RETURNING email, name, country ">>,
    {ok, [Result | []]} = db:query(Q, [Email, Name, erlpass:hash(Password), Country]),
    {ok, Result}.

authenticate(Email, Password) ->
    Q = <<"SELECT * FROM users WHERE email = $1">>,

    case db:query(Q, [Email]) of
        [] -> {error, not_found};
        [User = #{<<"password">> := Hash} | []] ->
            case erlpass:match(Password, Hash) of
                true -> {ok, maps:remove(<<"password">>, User)};
                false -> {error, unauthorized}
            end
    end.

get_from_countries(Countries) ->
    %% TODO paginate this call
    Q = <<"SELECT (email, name, country) FROM users WHERE country IN ($1)">>,
    Joined = lists:join(<<",">>, Countries),
    db:query(Q, [Joined]).
