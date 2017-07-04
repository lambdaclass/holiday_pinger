-module(db).

-export([query/2]).

query(Q, Params) ->
    case pgapp:equery(Q, Params) of
        {ok, _, Columns, Values} -> {ok, results_to_map(Columns, Values)};
        {ok, Columns, Values} -> {ok, results_to_map(Columns, Values)};
        {ok, _} -> ok;
        E -> {error, E}
    end.

results_to_map(ColumnNames, RowList) ->
    Keys = [erlang:binary_to_existing_atom(Name, latin1) ||
               {column, Name, _, _, _, _} <- ColumnNames],
    Seq = lists:seq(1, length(Keys)),
    ToMap = fun (Row) ->
                    lists:foldl(fun (Pos, Map) ->
                                        Key = lists:nth(Pos, Keys),
                                        Value = element(Pos, Row),
                                        Map#{Key => Value}
                                end, #{}, Seq)
            end,
    lists:map(ToMap, RowList).
