-module(erma).

-export([build/1, append/2]).
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
    Fields = build_fields(Fields),
    From = case Table of
               {Name, as, Alias} ->
                   [" FROM ", erma_utils:escape_name(Name),
                    " AS ", erma_utils:escape_name(Alias)];
               Name ->
                   [" FROM ", erma_utils:escape_name(Name)]
           end,
    Joins = build_joins(Table, Entities),
    Where = build_where(Entities),
    Order = build_order(Entities),
    Limit = build_limit(Entities),
    unicode:characters_to_binary([Select, Fields, From, Joins, Where, Order, Limit]).


-spec build_insert(name(), [name()], [[value()]], [returning()]) -> sql().
build_insert(Table, Names, Rows, Entities) ->
    TableName = erma_utils:escape_name(Table),
    Names2 = case Names of
                [] -> [];
                _ -> N1 = lists:map(fun erma_utils:escape_name/1, Names),
                     N2 = string:join(N1, ", "),
                     [" (", N2, ")"]
            end,
    Rows2 = lists:map(fun(V1) ->
                                V2 = lists:map(fun(V) -> build_value(V) end, V1),
                                ["(", string:join(V2, ", "), ")"]
                        end, Rows),
    Rows3 = string:join(Rows2, ", "),
    Returning = build_returning(Entities),
    unicode:characters_to_binary(["INSERT INTO ", TableName, Names2, " VALUES ", Rows3, Returning]).


-spec build_update(name(), [{name(), value()}], [where() | returning()]) -> sql().
build_update(Table, KV, Entities) ->
    TableName = erma_utils:escape_name(Table),
    Values = lists:map(fun({K, V}) -> [erma_utils:escape_name(K), " = ", build_value(V)];
                          (K) -> [erma_utils:escape_name(K), " = ?"]
                       end, KV),
    Values2 = string:join(Values, ", "),
    Where = build_where(Entities),
    Returning = build_returning(Entities),
    unicode:characters_to_binary(["UPDATE ", TableName, " SET ", Values2, Where, Returning]).


-spec build_delete(name(), [where() | returning()]) -> sql().
build_delete(Table, Entities) ->
    TableName = erma_utils:escape_name(Table),
    Where = build_where(Entities),
    Returning = build_returning(Entities),
    unicode:characters_to_binary(["DELETE FROM ", TableName, Where, Returning]).


%% TODO implement and spec
append({Query, Table, Entities}, NewEntities) when is_list(NewEntities) ->
    {Query, Table, merge(Entities, NewEntities)};
append(Query, NewEntity) -> append(Query, [NewEntity]).


%%% inner functions

-spec build_fields(list()) -> iolist().
build_fields(Entities) ->
    F = fun(Fields) ->
                Fields2 = lists:map(fun({AggFun, Name}) ->
                                          [string:to_upper(atom_to_list(AggFun)),
                                           "(", erma_utils:escape_name(Name), ")"];
                                     (Name) ->
                                          erma_utils:escape_name(Name)
                                  end, Fields),
                string:join(Fields2, ", ")
        end,
    case lists:keyfind(fields, 1, Entities) of
        false -> "*";
        {fields, distinct, Fields} -> ["DISTINCT ", F(Fields)];
        {fields, Fields} -> F(Fields)
    end.


-spec build_joins(table_name(), list()) -> iolist().
build_joins(MainTable, Entities) ->
    Joins = lists:filtermap(
              fun({inner_join, Table, Props}) -> {true, build_join_entity("INNER JOIN ", Table, MainTable, Props)};
                 ({left_join, Table, Props}) -> {true, build_join_entity("LEFT JOIN ", Table, MainTable, Props)};
                 ({right_join, Table, Props}) -> {true, build_join_entity("RIGHT JOIN ", Table, MainTable, Props)};
                 ({full_join, Table, Props}) -> {true, build_join_entity("FULL JOIN ", Table, MainTable, Props)};
                 (_) -> false
              end, Entities),
    [" ", string:join(Joins, " ")].


-spec build_join_entity(string(), table_name(), table_name(), [join_prop()]) -> iolist().
build_join_entity(Join, JoinTable, ToTable, JoinProps) ->
    Table = case JoinTable of
                {Name1, as, Alias1} -> [erma_utils:escape_name(Name1),
                                        " AS ", erma_utils:escape_name(Alias1)];
                Name1 -> erma_utils:escape_name(Name1)
            end,
    ToAlias = case ToTable of
                  {_, as, Alias2} -> erma_utils:escape_name(Alias2);
                  Name2 -> erma_utils:escape_name(Name2)
              end,
    {JoinName, JoinAlias} =
        case JoinTable of
            {Name3, as, Alias3} -> {Name3, erma_utils:escape_name(Alias3)};
            Name4 -> {Name4, erma_utils:escape_name(Name4)}
        end,
    PrimaryKey = case proplists:get_value(pk, JoinProps) of
                     undefined -> "id";
                     Pk -> erma_utils:escape_name(Pk)
                 end,
    ForeignKey = case proplists:get_value(fk, JoinProps) of
                     undefined -> erma_utils:escape_name([JoinName, "_id"]);
                     Fk -> erma_utils:escape_name(Fk)
                 end,
    [Join, Table, " ON ", JoinAlias, ".", PrimaryKey, " = ", ToAlias, ".", ForeignKey].


-spec build_where(list()) -> iolist().
build_where(Entities) ->
    case proplists:get_value(where, Entities) of
        undefined -> [];
        [] -> [];
        WEntities -> W1 = lists:map(fun(Entity) ->
                                            build_where_entity(Entity)
                                    end, WEntities),
                     case lists:flatten(W1) of
                         [] -> [];
                         _ -> W2 = string:join(W1, " AND "),
                              [" WHERE ", W2]
                     end
    end.

-spec build_where_entity(where_value()) -> iolist().
build_where_entity({'not', WEntity}) ->
    ["(NOT ", build_where_entity(WEntity), ")"];
build_where_entity({'or', []}) -> [];
build_where_entity({'or', WEntities}) ->
    W = lists:map(fun build_where_entity/1, WEntities),
    case W of
        [] -> [];
        _ -> ["(", string:join(W, " OR "), ")"]
    end;
build_where_entity({'and', []}) -> [];
build_where_entity({'and', WEntities}) ->
    W = lists:map(fun build_where_entity/1, WEntities),
    case lists:flatten(W) of
        [] -> [];
        _ -> ["(", string:join(W, " AND "), ")"]
    end;
build_where_entity({Key, '=', Value}) ->
    [erma_utils:escape_name(Key), " = ", build_value(Value)];
build_where_entity({Key, '<>', Value}) ->
    [erma_utils:escape_name(Key), " <> ", build_value(Value)];
build_where_entity({Key, '>', Value}) ->
    [erma_utils:escape_name(Key), " > ", build_value(Value)];
build_where_entity({Key, gt, Value}) ->
    [erma_utils:escape_name(Key), " > ", build_value(Value)];
build_where_entity({Key, '<', Value}) ->
    [erma_utils:escape_name(Key), " < ", build_value(Value)];
build_where_entity({Key, lt, Value}) ->
    [erma_utils:escape_name(Key), " < ", build_value(Value)];
build_where_entity({Key, '>=', Value}) ->
    [erma_utils:escape_name(Key), " >= ", build_value(Value)];
build_where_entity({Key, '<=', Value}) ->
    [erma_utils:escape_name(Key), " <= ", build_value(Value)];
build_where_entity({Key, true}) ->
    [erma_utils:escape_name(Key), " = true"];
build_where_entity({Key, false}) ->
    [erma_utils:escape_name(Key), " = false"];
build_where_entity({Key, like, Value}) when is_list(Value) ->
    [erma_utils:escape_name(Key), " LIKE ", build_value(Value)];
build_where_entity({Key, in, []}) ->
    [erma_utils:escape_name(Key), " IN (NULL)"];
build_where_entity({Key, in, Values}) when is_list(Values) ->
    V = lists:map(fun build_value/1, Values),
    [erma_utils:escape_name(Key), " IN (", string:join(V, ", "), ")"];
build_where_entity({Key, not_in, []}) ->
    [erma_utils:escape_name(Key), " NOT IN (NULL)"];
build_where_entity({Key, not_in, Values}) when is_list(Values) ->
    V = lists:map(fun build_value/1, Values),
    [erma_utils:escape_name(Key), " NOT IN (", string:join(V, ", "), ")"];
build_where_entity({Key, between, Value1, Value2}) ->
    [erma_utils:escape_name(Key), " BETWEEN ", build_value(Value1), " AND ", build_value(Value2)];
build_where_entity({Key, Value}) ->
    [erma_utils:escape_name(Key), " = ", build_value(Value)].


-spec build_value(value()) -> iolist().
build_value({date, D}) -> ["'", erma_utils:format_date(D), "'"];
build_value({time, T}) ->  ["'", erma_utils:format_time(T), "'"];
build_value({datetime, DT}) ->  ["'", erma_utils:format_datetime(DT), "'"];
build_value("?") -> "?"; % mysql placeholder
build_value([$$ | Rest] = Value) -> % postgresql placeholder
    case string:to_integer(Rest) of
        {_Num, []} -> Value;
        _ ->  ["'", Value, "'"]
    end;
build_value(Value) when is_integer(Value) -> integer_to_list(Value);
build_value(Value) when is_float(Value) -> io_lib:format("~p", [Value]);
build_value(Value) when is_binary(Value) -> build_value(unicode:characters_to_list(Value));
build_value(Value) when is_list(Value) -> ["'", Value, "'"].


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
build_order_entity({Field, asc}) -> [erma_utils:escape_name(Field), " ASC"];
build_order_entity({Field, desc}) -> [erma_utils:escape_name(Field), " DESC"];
build_order_entity(Field) -> [erma_utils:escape_name(Field), " ASC"].


-spec build_limit(list()) -> iolist().
build_limit(Entities) ->
    F = fun(Key, Str) ->
                case proplists:get_value(Key, Entities) of
                    undefined -> "";
                    Num when is_integer(Num) ->
                        [Str, " ", integer_to_list(Num)]
                end
        end,
    Offset = F(offset, "OFFSET"),
    Limit = F(limit, "LIMIT"),
    case {Offset, Limit} of
        {"", ""} -> "";
        {"", _} -> [" ", Limit];
        {_, ""} -> [" ", Offset];
        _ -> [" ", Offset, ", ", Limit]
    end.


-spec build_returning(list()) -> iolist().
build_returning(Entities) ->
    case proplists:get_value(returning, Entities) of
        undefined -> [];
        id -> " RETURNING id";
        Names -> Names2 = lists:map(fun erma_utils:escape_name/1, Names),
                 [" RETURNING ", string:join(Names2, ", ")]
    end.


%% TODO implementation and spec
merge([], Entities2) -> Entities2;
merge(Entities1, []) -> Entities1;
merge([{Tag, Props1} | Rest], Entities2) ->
    case proplists:get_value(Tag, Entities2) of
        undefined -> [{Tag, Props1} | merge(Rest, Entities2)];
        Props2 -> [{Tag, Props1 ++ Props2} | merge(Rest, proplists:delete(Tag, Entities2))]
    end.
