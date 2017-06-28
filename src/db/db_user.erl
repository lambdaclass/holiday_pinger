-module(db_user).

-export([create/1,
         authenticate/2,
         get_from_countries/1]).

create(#{<<"email">> := Email,
         <<"name">> := Name,
         <<"password">> := Password,
         <<"country">> := Country}) ->
    Q = <<"INSERT INTO users(email, name, password, country)"
          "VALUES($1, $2, $3, $4) RETURNING *">>,
    [Result | []] = db:query(Q, [Email, Name, erlpass:hash(Password), Country]),
    maps:remove(<<"password">>, Result).

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
    %% TODO maybe better to not select all the table
    Q = <<"SELECT * FROM users WHERE country IN ($1)">>,
    Joined = lists:join(<<",">>, Countries),
    db:query(Q, [Joined]).
