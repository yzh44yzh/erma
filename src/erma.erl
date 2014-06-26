-module(erma).
-export([build/1, append/2]).

%%% types

-type name() :: string().
-type action() :: like | '<' | lt | '>' | gt.
-type value() :: atom() | boolean() | integer() | float() | string().
-type sql() :: binary().

-type equery() :: {equery_type(), table(), [entity()]}.
-type equery_type() :: select | insert | update | delete.

-type table() :: {table, name()} | {table, name(), [table_prop()]}.
-type table_relation() :: has_one | has_many | belongs_to.
-type table_prop() :: {table_relation(), table()}.

-type entity() :: {entity_type(), entity_prop() | [entity_prop()]}.
-type entity_type() :: fields | with | where | order | limit.
-type entity_prop() :: name() | value() | {name(), value()} | {name(), action(), value()}.


-export_type([name/0, action/0, value/0, sql/0,
              equery/0, equery_type/0,
              table/0, table_relation/0, table_prop/0,
              entity/0, entity_type/0, entity_prop/0]).


%%% module API

-spec build(equery()) -> sql().
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


-spec append(equery(), [entity()] | entity()) -> equery().
append({Query, Table, Entities}, NewEntities) when is_list(NewEntities) ->
    {Query, Table, merge(Entities, NewEntities)};
append(Query, NewEntity) -> append(Query, [NewEntity]).


%%% inner functions

-spec get_name(table()) -> name().
get_name({table, Name}) -> Name;
get_name({table, Name, _Props}) -> Name.


-spec build_fields([entity()]) -> binary().
build_fields(Entities) ->
    list_to_binary(
      case proplists:get_value(fields, Entities) of
          undefined -> "*";
          List -> string:join(List, ", ")
      end).


-spec build_joins(table(), [entity()]) -> binary().
build_joins(MainTable, Entities) ->
    case proplists:get_value(with, Entities) of
        undefined -> <<>>;
        JEntities -> J1 = lists:map(fun(Table) ->
                                            lists:flatten(build_join(MainTable, Table))
                                    end, JEntities),
                     J2 = list_to_binary(string:join(J1, " ")),
                     <<" ", J2/binary>>
    end.


-spec build_join(table(), table()) -> iolist().
build_join(MainTable, JoinTable) ->
    MainTableName = get_name(MainTable),
    JoinTableName = get_name(JoinTable),
    ["LEFT JOIN ", JoinTableName, " ON ",
     JoinTableName, ".id = ",
     MainTableName, ".", JoinTableName, "_id"].


-spec build_where([entity()]) -> binary().
build_where(Entities) ->
    case proplists:get_value(where, Entities) of
        undefined -> <<>>;
        WEntities -> W1 = lists:map(fun(Entity) ->
                                            lists:flatten(build_where_entity(Entity))
                                    end, WEntities),
                     W2 = list_to_binary(string:join(W1, " AND ")),
                     <<" WHERE ", W2/binary>>
    end.


-spec build_where_entity(entity()) -> iolist().
build_where_entity({Key, '>', Value}) when is_integer(Value) -> [Key, " > ", integer_to_list(Value)];
build_where_entity({Key, '<', Value}) when is_integer(Value) -> [Key, " < ", integer_to_list(Value)];
build_where_entity({Key, true}) -> [Key, " = true"];
build_where_entity({Key, false}) -> [Key, " = false"];
build_where_entity({Key, like, Value}) when is_list(Value) -> [Key, " LIKE '", Value, "'"];
build_where_entity({Key, Value}) when is_integer(Value) -> [Key, " = ", integer_to_list(Value)];
build_where_entity({Key, Value}) when is_list(Value) -> [Key, " = '", Value, "'"].


-spec build_order([entity()]) -> binary().
build_order(Entities) ->
    case proplists:get_value(order, Entities) of
        undefined -> <<>>;
        OEntities -> O1 = lists:map(fun(Entity) ->
                                            lists:flatten(build_order_entity(Entity))
                                    end, OEntities),
                     O2 = list_to_binary(string:join(O1, ", ")),
                     <<" ORDER BY ", O2/binary>>
    end.


-spec build_order_entity(entity()) -> iolist().
build_order_entity({Field, asc}) -> [Field, " ASC"];
build_order_entity({Field, desc}) -> [Field, " DESC"];
build_order_entity(Field) -> [Field, " ASC"].


-spec build_limit([entity()]) -> binary().
build_limit(Entities) ->
    case proplists:get_value(limit, Entities) of
        undefined -> <<>>;
        Num when is_integer(Num) ->
            L = list_to_binary(integer_to_list(Num)),
            <<" LIMIT ", L/binary>>
    end.


-spec merge([entity()], [entity()]) -> [entity()].
merge([], Entities2) -> Entities2;
merge(Entities1, []) -> Entities1;
merge([{Tag, Props1} | Rest], Entities2) ->
    case proplists:get_value(Tag, Entities2) of
        undefined -> [{Tag, Props1} | merge(Rest, Entities2)];
        Props2 -> [{Tag, Props1 ++ Props2} | merge(Rest, proplists:delete(Tag, Entities2))]
    end.
