-module(erma).

-export([build/1, append/2]).
-import(erma_utils, [prepare_table_name/1, prepare_name/1, prepare_value/1]).
-include("erma.hrl").


%%% module API

-spec build(sql_query()) -> sql().
build({select, Fields, Table}) -> build_select("SELECT ", Fields, Table, []);
build({select, Fields, Table, Entities}) -> build_select("SELECT ", Fields, Table, Entities);
build({select_distinct, Fields, Table}) -> build_select("SELECT DISTINCT ", Fields, Table, []);
build({select_distinct, Fields, Table, Entities}) -> build_select("SELECT DISTINCT ", Fields, Table, Entities);
build({insert, Table, Names, Values}) -> build_insert(Table, Names, [Values], []);
build({insert, Table, Names, Values, Returning}) -> build_insert(Table, Names, [Values], [Returning]);
build({insert_rows, Table, Names, Rows}) -> build_insert(Table, Names, Rows, []);
build({insert_rows, Table, Names, Rows, Returning}) -> build_insert(Table, Names, Rows, [Returning]);
build({update, Table, KV}) -> build_update(Table, KV, []);
build({update, Table, KV, Entities}) -> build_update(Table, KV, Entities);
build({delete, Table}) -> build_delete(Table, []);
build({delete, Table, Entities}) -> build_delete(Table, Entities).


-spec build_select(select(), [field()], table_name(), [join() | where() | order() | limit()]) -> sql().
build_select(Select, Fields, Table, Entities) ->
    unicode:characters_to_binary([Select, build_fields(Fields), " FROM ",
                                  prepare_table_name(Table),
                                  build_joins(Table, Entities),
                                  build_where(Entities),
                                  build_order(Entities),
                                  build_limit(Entities)
                                 ]).


-spec build_insert(name(), [name()], [[value()]], [returning()]) -> sql().
build_insert(Table, Names, Rows, Entities) ->
    Names2 = case Names of
                [] -> [];
                _ -> N1 = lists:map(fun erma_utils:prepare_name/1, Names),
                     N2 = string:join(N1, ", "),
                     [" (", N2, ")"]
            end,
    Rows2 = lists:map(fun(V1) ->
                                V2 = lists:map(fun erma_utils:prepare_value/1, V1),
                                ["(", string:join(V2, ", "), ")"]
                        end, Rows),
    Rows3 = string:join(Rows2, ", "),
    unicode:characters_to_binary(["INSERT INTO ",
                                  prepare_name(Table),
                                  Names2, " VALUES ", Rows3,
                                  build_returning(Entities)]).


-spec build_update(name(), [{name(), value()}], [where() | returning()]) -> sql().
build_update(Table, KV, Entities) ->
    Values = lists:map(fun({K, V}) -> [prepare_name(K), " = ", prepare_value(V)];
                          (K) -> [prepare_name(K), " = ?"]
                       end, KV),
    Values2 = string:join(Values, ", "),
    unicode:characters_to_binary(["UPDATE ", prepare_name(Table),
                                  " SET ", Values2,
                                  build_where(Entities),
                                  build_returning(Entities)]).


-spec build_delete(name(), [where() | returning()]) -> sql().
build_delete(Table, Entities) ->
    unicode:characters_to_binary(["DELETE FROM ", prepare_name(Table),
                                  build_where(Entities),
                                  build_returning(Entities)]).


-spec append(sql_query(), list()) -> sql_query().
append({select, Fields, Table}, NewEntities) -> {select, Fields, Table, NewEntities};
append({select, Fields, Table, Entities}, NewEntities) -> {select, Fields, Table, merge(Entities, NewEntities)};
append({select_distinct, Fields, Table}, NewEntities) -> {select_distinct, Fields, Table, NewEntities};
append({select_distinct, Fields, Table, Entities}, NewEntities) -> {select_distinct, Fields, Table, merge(Entities, NewEntities)};
append({update, Table, KV}, NewEntities) -> {update, Table, KV, NewEntities};
append({update, Table, KV, Entities}, NewEntities) -> {update, Table, KV, merge(Entities, NewEntities)};
append({delete, Table}, NewEntities) -> {delete, Table, NewEntities};
append({delete, Table, Entities}, NewEntities) -> {delete, Table, merge(Entities, NewEntities)};
append(Query, _NewEntities) -> Query.


%%% inner functions

-spec build_fields([field()]) -> iolist().
build_fields([]) -> "*";
build_fields(Fields) ->
    Fields2 = lists:map(fun({AggFun, Name}) ->
                                [string:to_upper(atom_to_list(AggFun)),
                                 "(", prepare_name(Name), ")"];
                           (Name) ->
                                prepare_name(Name)
                        end, Fields),
    string:join(Fields2, ", ").


-spec build_joins(table_name(), list()) -> iolist().
build_joins(MainTable, Entities) ->
    Joins = lists:filtermap(
              fun({inner_join, Table}) -> {true, build_join_entity("INNER JOIN ", Table, MainTable, [])};
                 ({inner_join, Table, Props}) -> {true, build_join_entity("INNER JOIN ", Table, MainTable, Props)};
                 ({left_join, Table}) -> {true, build_join_entity("LEFT JOIN ", Table, MainTable, [])};
                 ({left_join, Table, Props}) -> {true, build_join_entity("LEFT JOIN ", Table, MainTable, Props)};
                 ({right_join, Table}) -> {true, build_join_entity("RIGHT JOIN ", Table, MainTable, [])};
                 ({right_join, Table, Props}) -> {true, build_join_entity("RIGHT JOIN ", Table, MainTable, Props)};
                 ({full_join, Table}) -> {true, build_join_entity("FULL JOIN ", Table, MainTable, [])};
                 ({full_join, Table, Props}) -> {true, build_join_entity("FULL JOIN ", Table, MainTable, Props)};
                 (_) -> false
              end, Entities),
    case Joins of
        [] -> [];
        _ -> [" ", string:join(Joins, " ")]
    end.


-spec build_join_entity(string(), table_name(), table_name(), [join_prop()]) -> iolist().
build_join_entity(Join, JoinTable, ToTable, JoinProps) ->
    Table = case JoinTable of
                {Name1, as, Alias1} -> [prepare_name(Name1),
                                        " AS ", prepare_name(Alias1)];
                Name1 -> prepare_name(Name1)
            end,
    ToAlias = case ToTable of
                  {_, as, Alias2} -> prepare_name(Alias2);
                  Name2 -> prepare_name(Name2)
              end,
    {JoinName, JoinAlias} =
        case JoinTable of
            {Name3, as, Alias3} -> {Name3, prepare_name(Alias3)};
            Name4 -> {Name4, prepare_name(Name4)}
        end,
    PrimaryKey = case proplists:get_value(pk, JoinProps) of
                     undefined -> "id";
                     Pk -> prepare_name(Pk)
                 end,
    ForeignKey = case proplists:get_value(fk, JoinProps) of
                     undefined -> prepare_name([JoinName, "_id"]);
                     Fk -> prepare_name(Fk)
                 end,
    [Join, Table, " ON ", JoinAlias, ".", PrimaryKey, " = ", ToAlias, ".", ForeignKey].


-spec build_where(list()) -> iolist().
build_where(Conditions) ->
    case proplists:get_value(where, Conditions) of
        undefined -> [];
        [] -> [];
        WConditions -> W1 = lists:map(fun build_where_condition/1, WConditions),
                     case lists:flatten(W1) of
                         [] -> [];
                         _ -> W2 = string:join(W1, " AND "),
                              [" WHERE ", W2]
                     end
    end.

-spec build_where_condition(where_condition()) -> iolist().
build_where_condition({'not', WEntity}) ->
    ["(NOT ", build_where_condition(WEntity), ")"];
build_where_condition({'or', []}) -> [];
build_where_condition({'or', WConditions}) ->
    W = lists:map(fun build_where_condition/1, WConditions),
    case W of
        [] -> [];
        _ -> ["(", string:join(W, " OR "), ")"]
    end;
build_where_condition({'and', []}) -> [];
build_where_condition({'and', WConditions}) ->
    W = lists:map(fun build_where_condition/1, WConditions),
    case lists:flatten(W) of
        [] -> [];
        _ -> ["(", string:join(W, " AND "), ")"]
    end;
build_where_condition({Key, '=', Value}) ->
    [prepare_name(Key), " = ", prepare_value(Value)];
build_where_condition({Key, '<>', Value}) ->
    [prepare_name(Key), " <> ", prepare_value(Value)];
build_where_condition({Key, '>', Value}) ->
    [prepare_name(Key), " > ", prepare_value(Value)];
build_where_condition({Key, gt, Value}) ->
    [prepare_name(Key), " > ", prepare_value(Value)];
build_where_condition({Key, '<', Value}) ->
    [prepare_name(Key), " < ", prepare_value(Value)];
build_where_condition({Key, lt, Value}) ->
    [prepare_name(Key), " < ", prepare_value(Value)];
build_where_condition({Key, '>=', Value}) ->
    [prepare_name(Key), " >= ", prepare_value(Value)];
build_where_condition({Key, '<=', Value}) ->
    [prepare_name(Key), " <= ", prepare_value(Value)];
build_where_condition({Key, true}) ->
    [prepare_name(Key), " = true"];
build_where_condition({Key, false}) ->
    [prepare_name(Key), " = false"];
build_where_condition({Key, like, Value}) when is_list(Value) ->
    [prepare_name(Key), " LIKE ", prepare_value(Value)];
build_where_condition({Key, in, []}) ->
    [prepare_name(Key), " IN (NULL)"];
build_where_condition({Key, in, Values}) when is_list(Values) ->
    V = lists:map(fun erma_utils:prepare_value/1, Values),
    [prepare_name(Key), " IN (", string:join(V, ", "), ")"];
build_where_condition({Key, not_in, []}) ->
    [prepare_name(Key), " NOT IN (NULL)"];
build_where_condition({Key, not_in, Values}) when is_list(Values) ->
    V = lists:map(fun erma_utils:prepare_value/1, Values),
    [prepare_name(Key), " NOT IN (", string:join(V, ", "), ")"];
build_where_condition({Key, between, Value1, Value2}) ->
    [prepare_name(Key), " BETWEEN ", prepare_value(Value1), " AND ", prepare_value(Value2)];
build_where_condition({Key, Value}) ->
    [prepare_name(Key), " = ", prepare_value(Value)].


-spec build_order(list()) -> iolist().
build_order(Entities) ->
    case proplists:get_value(order, Entities) of
        undefined -> [];
        [] -> [];
        OEntities -> O1 = lists:map(fun(Entity) -> build_order_entity(Entity) end, OEntities),
                     O2 = string:join(O1, ", "),
                     [" ORDER BY ", O2]
    end.


-spec build_order_entity(name() | {name(), atom()}) -> iolist().
build_order_entity({Field, asc}) -> [prepare_name(Field), " ASC"];
build_order_entity({Field, desc}) -> [prepare_name(Field), " DESC"];
build_order_entity(Field) -> [prepare_name(Field), " ASC"].


-spec build_limit(list()) -> iolist().
build_limit(Entities) ->
    lists:filtermap(fun({limit, Num}) -> {true, [" LIMIT ", integer_to_list(Num)]};
                       ({offset, N1, limit, N2}) -> {true, [" OFFSET ", integer_to_list(N1), ", LIMIT ", integer_to_list(N2)]};
                       (_) -> false
                    end, Entities).


-spec build_returning(list()) -> iolist().
build_returning(Entities) ->
    case proplists:get_value(returning, Entities) of
        undefined -> [];
        id -> " RETURNING id";
        Names -> Names2 = lists:map(fun erma_utils:prepare_name/1, Names),
                 [" RETURNING ", string:join(Names2, ", ")]
    end.


-spec merge(list(), list()) -> list().
merge([], Entities2) -> Entities2;
merge(Entities1, []) -> Entities1;
merge([{Tag, Props1} | Rest], Entities2) ->
    case proplists:get_value(Tag, Entities2) of
        undefined -> [{Tag, Props1} | merge(Rest, Entities2)];
        Props2 -> [{Tag, Props1 ++ Props2} | merge(Rest, proplists:delete(Tag, Entities2))]
    end.
