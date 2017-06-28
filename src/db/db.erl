-module(db).

-export([query/2]).

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
