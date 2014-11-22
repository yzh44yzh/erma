-module(erma).

-export([build/1, append/2]).
-include("erma.hrl").


%%% module API

-spec build(equery()) -> sql().
build({select, Table, Entities}) ->
    Fields = build_fields(Entities),
    From = build_from(Table),
    Joins = build_joins(Table, Entities),
    Where = build_where(Entities),
    Order = build_order(Entities),
    OffsetLimit = build_offset_limit(Entities),
    <<"SELECT ",
      Fields/binary,
      From/binary,
      Joins/binary,
      Where/binary,
      Order/binary,
      OffsetLimit/binary>>;

build({insert, Table, {rows, Keys, Values}}) ->
    TableName = get_table_name(Table),
    Keys2 = case Keys of
                [] -> <<>>;
                _ -> K1 = lists:map(fun erma_utils:escape_name/1, Keys),
                     K2 = list_to_binary(string:join(K1, ", ")),
                     <<" (", K2/binary, ")">>
            end,
    Values2 = lists:map(fun(V1) ->
                                V2 = lists:map(fun(V) ->
                                                       lists:flatten(build_value(V))
                                               end, V1),
                                lists:flatten(["(", string:join(V2, ", "), ")"])
                        end, Values),
    Values3 = list_to_binary(string:join(Values2, ", ")),

    <<"INSERT INTO ", TableName/binary,
      Keys2/binary, " VALUES ", Values3/binary>>;

build({insert, Table, KV}) ->
    TableName = get_table_name(Table),
    {Keys, Values} = lists:unzip(lists:map(
                                   fun({K, V}) -> {erma_utils:escape_name(K),
                                                   lists:flatten(build_value(V))};
                                      (K) -> {erma_utils:escape_name(K), "?"}
                                   end, KV)),
    Keys2 = list_to_binary(string:join(Keys, ", ")),
    Values2 = list_to_binary(string:join(Values, ", ")),
    <<"INSERT INTO ", TableName/binary,
      " (", Keys2/binary, ") VALUES (", Values2/binary, ")">>;

build({insert, Table, Rows, Returning}) ->
    Sql = build({insert, Table, Rows}),
    Ret = build_returning([Returning]),
    <<Sql/binary, Ret/binary>>;


build({update, Table, KV}) ->
    build({update, Table, KV, []});

build({update, Table, KV, Entities}) when is_list(Entities) ->
    TableName = get_table_name(Table),
    Values = lists:map(fun({K, V}) ->
                               lists:flatten([list_to_binary(erma_utils:escape_name(K)),
                                              " = ", build_value(V)]);
                          (K) ->
                               lists:flatten([list_to_binary(erma_utils:escape_name(K)),
                                              " = ?"])
                       end, KV),
    Values2 = list_to_binary(string:join(Values, ", ")),

    Where = build_where(Entities),
    Returning = build_returning(Entities),

    <<"UPDATE ", TableName/binary, " SET ", Values2/binary,
      Where/binary, Returning/binary>>;

build({update, Table, KV, Entity}) ->
    build({update, Table, KV, [Entity]});

build({delete, Table, Entities}) when is_list(Entities) ->
    TableName = get_table_name(Table),
    Where = build_where(Entities),
    Returning = build_returning(Entities),
    <<"DELETE FROM ", TableName/binary, Where/binary, Returning/binary>>;

build({delete, Table, Entity}) ->
    build({delete, Table, [Entity]}).


-spec append(equery(), [entity()] | entity()) -> equery().
append({Query, Table, Entities}, NewEntities) when is_list(NewEntities) ->
    {Query, Table, merge(Entities, NewEntities)};
append(Query, NewEntity) -> append(Query, [NewEntity]).


%%% inner functions

-spec get_table_name(table()) -> binary().
get_table_name({table, Name}) -> list_to_binary(erma_utils:escape_name(Name));
get_table_name({table, Name, as, _}) -> list_to_binary(erma_utils:escape_name(Name));
get_table_name(Name) when is_list(Name) orelse is_binary(Name) ->
    get_table_name({table, Name}).


-spec build_fields([entity()]) -> binary().
build_fields(Entities) ->
    F = fun(List) ->
                List2 = lists:map(fun erma_utils:escape_name/1, List),
                string:join(List2, ", ")
        end,
    list_to_binary(
      case lists:keyfind(fields, 1, Entities) of
          false -> "*";
          {fields, distinct, List} -> "DISTINCT " ++ F(List);
          {fields, List} -> F(List)
      end).


-spec build_from(table()) -> binary().
build_from({table, Name}) -> list_to_binary(" FROM " ++ erma_utils:escape_name(Name));
build_from({table, Name, as, Alias}) ->
    list_to_binary(lists:flatten([" FROM ", erma_utils:escape_name(Name),
                                  " AS ", erma_utils:escape_name(Alias)]));
build_from(Name) when is_list(Name) orelse is_binary(Name) ->
    build_from({table, Name}).


-spec build_joins(table(), [entity()]) -> binary().
build_joins(MainTable, Entities) ->
    case proplists:get_value(joins, Entities) of
        undefined -> <<>>;
        [] -> <<>>;
        JEntities -> J1 = lists:map(fun(Join) ->
                                            lists:flatten(build_join_entity(MainTable, Join))
                                    end, JEntities),
                     J2 = list_to_binary(string:join(J1, " ")),
                     <<" ", J2/binary>>
    end.


-spec build_join_entity(table(), join()) -> iolist().
build_join_entity(MainTable, {JoinType, JoinTable}) ->
    build_join_entity(JoinType, JoinTable, MainTable, []);

build_join_entity(MainTable, {JoinType, JoinTable, JoinProps}) when is_list(JoinProps) ->
    build_join_entity(JoinType, JoinTable, MainTable, JoinProps);

build_join_entity(_MainTable, {JoinType, JoinTable, ToTable}) when is_tuple(ToTable) ->
    build_join_entity(JoinType, JoinTable, ToTable, []);

build_join_entity(_MainTable, {JoinType, JoinTable, ToTable, JoinProps}) ->
    build_join_entity(JoinType, JoinTable, ToTable, JoinProps).


-spec build_join_entity(join_type(), table(), table(), [join_prop()]) -> iolist().
build_join_entity(JoinType, JoinTable, ToTable, JoinProps) ->
    Join = case JoinType of
               inner -> "INNER JOIN ";
               left -> "LEFT JOIN ";
               right -> "RIGHT JOIN ";
               full -> "FULL JOIN "
           end,
    Table =
        case JoinTable of
            {table, Name1} -> erma_utils:escape_name(Name1);
            {table, Name1, as, Alias1} -> [erma_utils:escape_name(Name1),
                                           " AS ", erma_utils:escape_name(Alias1)]
        end,
    ToAlias =
        case ToTable of
            {table, Name2} -> erma_utils:escape_name(Name2);
            {table, _, as, Alias2} -> erma_utils:escape_name(Alias2)
        end,
    {JoinName, JoinAlias} =
        case JoinTable of
            {table, Name3} ->
                {Name3, erma_utils:escape_name(Name3)};
            {table, Name4, as, Alias4} ->
                {Name4, erma_utils:escape_name(Alias4)}
        end,
    PrimaryKey = case proplists:get_value(pk, JoinProps) of
                     undefined -> "id";
                     Pk -> erma_utils:escape_name(Pk)
                 end,
    ForeignKey = case proplists:get_value(fk, JoinProps) of
                     undefined -> erma_utils:escape_name(JoinName ++ "_id");
                     Fk -> erma_utils:escape_name(Fk)
                 end,
    [Join, Table, " ON ", JoinAlias, ".", PrimaryKey, " = ", ToAlias, ".", ForeignKey].


-spec build_where([entity()]) -> binary().
build_where(Entities) ->
    case proplists:get_value(where, Entities) of
        undefined -> <<>>;
        [] -> <<>>;
        WEntities -> W1 = lists:map(fun(Entity) ->
                                            lists:flatten(build_where_entity(Entity))
                                    end, WEntities),
                     case lists:flatten(W1) of
                         [] -> <<>>;
                         _ -> W2 = list_to_binary(string:join(W1, " AND ")),
                              <<" WHERE ", W2/binary>>
                     end
    end.

-spec build_where_entity(where_value()) -> iolist().
build_where_entity({'not', WEntity}) ->
    ["(NOT ", build_where_entity(WEntity), ")"];
build_where_entity({'or', []}) -> [];
build_where_entity({'or', WEntities}) ->
    W = lists:map(fun build_where_entity/1, WEntities),
    case lists:flatten(W) of
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


-spec build_order([entity()]) -> binary().
build_order(Entities) ->
    case proplists:get_value(order, Entities) of
        undefined -> <<>>;
        [] -> <<>>;
        OEntities -> O1 = lists:map(fun(Entity) ->
                                            lists:flatten(build_order_entity(Entity))
                                    end, OEntities),
                     O2 = list_to_binary(string:join(O1, ", ")),
                     <<" ORDER BY ", O2/binary>>
    end.


-spec build_order_entity(order_value()) -> iolist().
build_order_entity({Field, asc}) -> [erma_utils:escape_name(Field), " ASC"];
build_order_entity({Field, desc}) -> [erma_utils:escape_name(Field), " DESC"];
build_order_entity(Field) -> [erma_utils:escape_name(Field), " ASC"].


-spec build_offset_limit([entity()]) -> binary().
build_offset_limit(Entities) ->
    F = fun(Key, Str) ->
                case proplists:get_value(Key, Entities) of
                    undefined -> "";
                    Num when is_integer(Num) ->
                        [Str, " ", integer_to_list(Num)]
                end
        end,
    Offset = F(offset, "OFFSET"),
    Limit = F(limit, "LIMIT"),
    list_to_binary(
      case {Offset, Limit} of
          {"", ""} -> "";
          {"", _} -> [" ", Limit];
          {_, ""} -> [" ", Offset];
          _ -> [" ", Offset, ", ", Limit]
      end).


-spec build_returning([entity()]) -> binary().
build_returning(Entities) ->
    case proplists:get_value(returning, Entities) of
        undefined -> <<>>;
        Val -> list_to_binary(build_returning_entity({returning, Val}))
    end.


-spec build_returning_entity(returning_entity()) -> iolist().
build_returning_entity({returning, id}) ->
    " RETURNING id";
build_returning_entity({returning, Fields}) ->
    Fields2 = lists:map(fun erma_utils:escape_name/1, Fields),
    [" RETURNING ", string:join(Fields2, ", ")].


-spec merge([entity()], [entity()]) -> [entity()].
merge([], Entities2) -> Entities2;
merge(Entities1, []) -> Entities1;
merge([{Tag, Props1} | Rest], Entities2) ->
    case proplists:get_value(Tag, Entities2) of
        undefined -> [{Tag, Props1} | merge(Rest, Entities2)];
        Props2 -> [{Tag, Props1 ++ Props2} | merge(Rest, proplists:delete(Tag, Entities2))]
    end.
