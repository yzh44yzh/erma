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
      OffsetLimit/binary>>.


-spec append(equery(), [entity()] | entity()) -> equery().
append({Query, Table, Entities}, NewEntities) when is_list(NewEntities) ->
    {Query, Table, merge(Entities, NewEntities)};
append(Query, NewEntity) -> append(Query, [NewEntity]).


%%% inner functions

-spec build_fields([entity()]) -> binary().
build_fields(Entities) ->
    list_to_binary(
      case proplists:get_value(fields, Entities) of
          undefined -> "*";
          List -> string:join(List, ", ")
      end).


-spec build_from(table()) -> binary().
build_from({table, Name}) -> list_to_binary(" FROM " ++ Name);
build_from({table, Name, as, Alias}) ->
    list_to_binary(lists:flatten([" FROM ", Name, " AS ", Alias])).


-spec build_joins(table(), [entity()]) -> binary().
build_joins(MainTable, Entities) ->
    case proplists:get_value(joins, Entities) of
        undefined -> <<>>;
        JEntities -> J1 = lists:map(fun(Join) ->
                                            lists:flatten(build_join_entity(MainTable, Join))
                                    end, JEntities),
                     J2 = list_to_binary(string:join(J1, " ")),
                     <<" ", J2/binary>>
    end.


-spec build_join_entity(table(), join()) -> iolist().
build_join_entity(MainTable, {JoinType, JoinTable}) ->
    build_join_entity(MainTable, {JoinType, JoinTable, []});

build_join_entity(MainTable, {JoinType, JoinTable, _JoinProps}) ->
    MainKey = case MainTable of
                  {table, Name1} -> Name1;
                  {table, _, as, Alias1} -> Alias1
              end,
    {JoinName, JoinKey} = case JoinTable of
                              {table, Name2} -> {Name2, Name2};
                              {table, Name2, as, Alias2} -> {[Name2, " AS ", Alias2], Alias2}
                          end,

    %% TODO use JoinProps
    PrimaryKey = [MainKey, ".", JoinKey, "_id"],
    ForeignKey = [JoinKey, ".id"],
    On = [" ON ", ForeignKey, " = ", PrimaryKey],

    case JoinType of
        inner -> ["INNER JOIN ", JoinName, On];
        left -> ["LEFT JOIN ", JoinName, On];
        right -> ["RIGHT JOIN ", JoinName, On];
        full -> ["FULL JOIN ", JoinName, On]
    end.


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

-spec build_where_entity(where_entity()) -> iolist().
build_where_entity({'not', WEntity}) ->
    ["(NOT ", build_where_entity(WEntity), ")"];
build_where_entity({'or', WEntities}) ->
    W = lists:map(fun build_where_entity/1, WEntities),
    ["(", string:join(W, " OR "), ")"];
build_where_entity({'and', WEntities}) ->
    W = lists:map(fun build_where_entity/1, WEntities),
    ["(", string:join(W, " AND "), ")"];
build_where_entity({Key, '=', Value}) ->
    [Key, " = ", build_where_value(Value)];
build_where_entity({Key, '>', Value}) ->
    [Key, " > ", build_where_value(Value)];
build_where_entity({Key, '<', Value}) ->
    [Key, " < ", build_where_value(Value)];
build_where_entity({Key, '>=', Value}) ->
    [Key, " >= ", build_where_value(Value)];
build_where_entity({Key, '<=', Value}) ->
    [Key, " <= ", build_where_value(Value)];
build_where_entity({Key, true}) ->
    [Key, " = true"];
build_where_entity({Key, false}) ->
    [Key, " = false"];
build_where_entity({Key, like, Value}) when is_list(Value) ->
    [Key, " LIKE '", Value, "'"];
build_where_entity({Key, in, Values}) when is_list(Values) ->
    V = lists:map(fun build_where_value/1, Values),
    [Key, " IN (", string:join(V, ", "), ")"];
build_where_entity({Key, between, Value1, Value2}) ->
    [Key, " BETWEEN ", build_where_value(Value1), % TODO valid syntax?
     " ", build_where_value(Value2)];
build_where_entity({Key, Value}) ->
    [Key, " = ", build_where_value(Value)].


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
