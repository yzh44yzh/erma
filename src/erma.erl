-module(erma).
-export([build/1, append/2]).


%%% module API

build({select, Table, Entities}) ->
    TableName = list_to_binary(get_name(Table)),
    Fields = build_fields(Entities),
    Joins = build_joins(Table, Entities),
    Where = build_where(Entities),
    Order = build_order(Entities),
    Limit = build_limit(Entities),
    <<"SELECT ", Fields/binary,
      " FROM ", TableName/binary,
      Joins/binary, Where/binary,
      Order/binary, Limit/binary>>.


append({Query, Table, Entities}, NewEntities) when is_list(NewEntities) ->
    {Query, Table, merge(Entities, NewEntities)};
append(Query, NewEntity) -> append(Query, [NewEntity]).


%%% inner functions

get_name({table, Name}) -> Name;
get_name({table, Name, _Props}) -> Name.


build_fields(Entities) ->
    list_to_binary(
      case proplists:get_value(fields, Entities) of
          undefined -> "*";
          List -> string:join(List, ", ")
      end).


build_joins(MainTable, Entities) ->
    case proplists:get_value(with, Entities) of
        undefined -> <<>>;
        JEntities -> J1 = lists:map(fun(Table) ->
                                            lists:flatten(build_join(MainTable, Table))
                                    end, JEntities),
                     J2 = list_to_binary(string:join(J1, " ")),
                     <<" ", J2/binary>>
    end.


build_join(MainTable, JoinTable) ->
    MainTableName = get_name(MainTable),
    JoinTableName = get_name(JoinTable),
    ["LEFT JOIN ", JoinTableName, " ON ",
     JoinTableName, ".id = ",
     MainTableName, ".", JoinTableName, "_id"].


build_where(Entities) ->
    case proplists:get_value(where, Entities) of
        undefined -> <<>>;
        WEntities -> W1 = lists:map(fun(Entity) ->
                                            lists:flatten(build_where_entity(Entity))
                                    end, WEntities),
                     W2 = list_to_binary(string:join(W1, " AND ")),
                     <<" WHERE ", W2/binary>>
    end.


build_where_entity({Key, '>', Value}) when is_integer(Value) -> [Key, " > ", integer_to_list(Value)];
build_where_entity({Key, '<', Value}) when is_integer(Value) -> [Key, " < ", integer_to_list(Value)];
build_where_entity({Key, true}) -> [Key, " = true"];
build_where_entity({Key, false}) -> [Key, " = false"];
build_where_entity({Key, like, Value}) when is_list(Value) -> [Key, " LIKE '", Value, "'"];
build_where_entity({Key, Value}) when is_integer(Value) -> [Key, " = ", integer_to_list(Value)];
build_where_entity({Key, Value}) when is_list(Value) -> [Key, " = '", Value, "'"].


build_order(Entities) ->
    case proplists:get_value(order, Entities) of
        undefined -> <<>>;
        OEntities -> O1 = lists:map(fun(Entity) ->
                                            lists:flatten(build_order_entity(Entity))
                                    end, OEntities),
                     O2 = list_to_binary(string:join(O1, ", ")),
                     <<" ORDER BY ", O2/binary>>
    end.


build_order_entity({Field, asc}) -> [Field, " ASC"];
build_order_entity({Field, desc}) -> [Field, " DESC"];
build_order_entity(Field) -> [Field, " ASC"].


build_limit(Entities) ->
    case proplists:get_value(limit, Entities) of
        undefined -> <<>>;
        Num when is_integer(Num) ->
            L = list_to_binary(integer_to_list(Num)),
            <<" LIMIT ", L/binary>>
    end.


merge([], Entities2) -> Entities2;
merge(Entities1, []) -> Entities1;
merge([{Tag, Props1} | Rest], Entities2) ->
    case proplists:get_value(Tag, Entities2) of
        undefined -> [{Tag, Props1} | merge(Rest, Entities2)];
        Props2 -> [{Tag, Props1 ++ Props2} | merge(Rest, proplists:delete(Tag, Entities2))]
    end.
