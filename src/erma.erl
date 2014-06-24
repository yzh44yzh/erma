-module(erma).
-export([build/1, append/2]).


%%% module API

build({select, Table, Entities}) ->
    TableName = list_to_binary(get_name(Table)),
    Fields = build_fields(Entities),
    Joins = build_joins(Table, Entities),
    Where = build_where(Entities),
    <<"SELECT ", Fields/binary,
      " FROM ", TableName/binary,
      Joins/binary, Where/binary>>.


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
        WEntities -> W1 = lists:map(fun(Table) ->
                                            build_join(MainTable, Table)
                                    end, WEntities),
                     W2 = list_to_binary(string:join(W1, " ")),
                     <<" ", W2/binary>>
    end.


build_join(MainTable, JoinTable) ->
    MainTableName = get_name(MainTable),
    JoinTableName = get_name(JoinTable),
    lists:flatten(["LEFT JOIN ", JoinTableName, " ON ",
                   JoinTableName, ".id = ",
                   MainTableName, ".", JoinTableName, "_id"]).


build_where(Entities) ->
    case proplists:get_value(where, Entities) of
        undefined -> <<>>;
        WEntities -> W1 = lists:map(fun build_where_entity/1, WEntities),
                     W2 = list_to_binary(string:join(W1, " AND ")),
                     <<" WHERE ", W2/binary>>
    end.


build_where_entity({Key, Value}) ->
    lists:flatten([Key, " = '", Value, "'"]).


merge([], Entities2) -> Entities2;
merge(Entities1, []) -> Entities1;
merge([{Tag, Props1} | Rest], Entities2) ->
    case proplists:get_value(Tag, Entities2) of
        undefined -> [{Tag, Props1} | merge(Rest, Entities2)];
        Props2 -> [{Tag, Props1 ++ Props2} | merge(Rest, proplists:delete(Tag, Entities2))]
    end.
