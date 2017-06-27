-module(hp_user_db).

-export([create/1,
         authenticate/2,
         get_from_countries/1]).

create(#{<<"email">> := Email,
         <<"name">> := Name,
         <<"password">> := Password,
         <<"country">> := Country}) ->
    Q = <<"INSERT INTO users(email, name, password, country)"
          "VALUES($1, $2, $3, $4) RETURNING *">>,
    [Result | []] = query(Q, [Email, Name, erlpass:hash(Password), Country]),
    maps:remove(<<"password">>, Result).

authenticate(Email, Password) ->
    Q = <<"SELECT * FROM users WHERE email = $1">>,

    case query(Q, [Email]) of
        [] -> {error, not_found};
        [User = #{<<"password">> := Hash} | []] ->
            case erlpass:match(Password, Hash) of
                true -> {ok, maps:remove(<<"password">>, User)};
                false -> {error, unauthorized}
            end
    end.

%% TODO implement
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

%% FIXME move to some db_utils
query(Q, Params) ->
    case pgapp:equery(Q, Params) of
        {ok, _, Columns, Values} -> results_to_map(Columns, Values);
        {ok, Columns, Values} -> results_to_map(Columns, Values);
        E -> {error, E}
    end.

results_to_map(ColumnNames, RowList) ->
    Keys = [Name || {column, Name, _, _, _, _} <- ColumnNames],
    ToMap = fun (Row) ->
                    RowAsList = tuple_to_list(Row),
                    maps:from_list(lists:zip(Keys, RowAsList))
            end,

    lists:map(ToMap, RowList).
