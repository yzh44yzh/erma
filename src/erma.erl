-module(erma).
-export([build/1, append/2]).

%%% types

-type name() :: string().
-type sql() :: binary().

-type equery() :: {equery_type(), table(), [entity()]}.
-type equery_type() :: select | insert | update | delete.

-type table() :: {table, name()} | {table, name(), [table_prop()]}.
-type table_relation() :: has_one | has_many | belongs_to.
-type table_prop() :: {table_relation(), table()}.

-type entity() :: fields_entity() | with_entity() | where_entity() |
                  order_entity() | offset_entity() | limit_entity().

-type fields_entity() :: {fields, [name()]}.
-type with_entity() :: {with, [table()]}.
-type where_entity() :: {where, [{name(), where_value()} |
                                 {name(), where_action(), where_value()} |
                                 {'not', where_entity()} |
                                 {'and', [where_entity()]} |
                                 {'or', [where_entity()]}]}.
-type order_entity() :: {order, order_value() | [order_value()]}.
-type offset_entity() :: {offset, integer()}.
-type limit_entity() :: {limit, integer()}.

-type where_action() :: '=' | '<' | '>' | '>=' | '<=' | like | in | between.
-type where_value() :: boolean() | integer() | float() | string().
-type order_value() :: name() | {name(), asc} | {name(), desc}.

-export_type([name/0, sql/0,
              equery/0, equery_type/0,
              table/0, table_relation/0, table_prop/0,
              entity/0, fields_entity/0, with_entity/0,
              where_entity/0, order_entity/0, limit_entity/0,
              where_action/0, where_value/0, order_value/0]).


%%% module API

-spec build(equery()) -> sql().
build({select, Table, Entities}) ->
    TableName = list_to_binary(get_name(Table)),
    Fields = build_fields(Entities),
    With = build_with(Table, Entities),
    Where = build_where(Entities),
    Order = build_order(Entities),
    OffsetLimit = build_offset_limit(Entities),
    <<"SELECT ", Fields/binary,
      " FROM ", TableName/binary,
      With/binary, Where/binary,
      Order/binary, OffsetLimit/binary>>.


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


-spec build_with(table(), [entity()]) -> binary().
build_with(MainTable, Entities) ->
    case proplists:get_value(with, Entities) of
        undefined -> <<>>;
        WEntities -> W1 = lists:map(fun(Table) ->
                                            lists:flatten(build_with_entity(MainTable, Table))
                                    end, WEntities),
                     W2 = list_to_binary(string:join(W1, " ")),
                     <<" ", W2/binary>>
    end.


-spec build_with_entity(table(), table()) -> iolist().
build_with_entity(MainTable, WithTable) ->
    MainTableName = get_name(MainTable),
    WithTableName = get_name(WithTable),
    ["LEFT JOIN ", WithTableName, " ON ",
     WithTableName, ".id = ",
     MainTableName, ".", WithTableName, "_id"].


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

%% TODO tests for all cases
-spec build_where_entity(where_entity()) -> iolist().
build_where_entity({Key, '=', Value}) -> [Key, " = ", build_where_value(Value)];
build_where_entity({Key, '>', Value}) -> [Key, " > ", build_where_value(Value)];
build_where_entity({Key, '<', Value}) -> [Key, " < ", build_where_value(Value)];
build_where_entity({Key, '>=', Value}) -> [Key, " >= ", build_where_value(Value)];
build_where_entity({Key, '<=', Value}) -> [Key, " <= ", build_where_value(Value)];
build_where_entity({Key, true}) -> [Key, " = true"];
build_where_entity({Key, false}) -> [Key, " = false"];
build_where_entity({Key, like, Value}) when is_list(Value) -> [Key, " LIKE '", Value, "'"];
build_where_entity({Key, Value}) -> [Key, " = ", build_where_value(Value)].
%% TODO in | between.
%% TODO 'not' 'or' 'and'

-spec build_where_value(where_value()) -> iolist().
build_where_value(Value) when is_integer(Value) -> integer_to_list(Value);
build_where_value(Value) when is_float(Value) -> io_lib:format("~p", [Value]);
build_where_value(Value) when is_list(Value) -> ["'", Value, "'"].

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


-spec build_order_entity(order_entity()) -> iolist().
build_order_entity({Field, asc}) -> [Field, " ASC"];
build_order_entity({Field, desc}) -> [Field, " DESC"];
build_order_entity(Field) -> [Field, " ASC"].


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


-spec merge([entity()], [entity()]) -> [entity()].
merge([], Entities2) -> Entities2;
merge(Entities1, []) -> Entities1;
merge([{Tag, Props1} | Rest], Entities2) ->
    case proplists:get_value(Tag, Entities2) of
        undefined -> [{Tag, Props1} | merge(Rest, Entities2)];
        Props2 -> [{Tag, Props1 ++ Props2} | merge(Rest, proplists:delete(Tag, Entities2))]
    end.
