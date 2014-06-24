-module(erma).
-export([append/2]).


%%% module API

append({Query, Table, Parts}, NewParts) when is_list(NewParts) ->
    {Query, Table, merge(Parts, NewParts)};
append(Query, NewPart) -> append(Query, [NewPart]).


merge([], Parts2) -> Parts2;
merge(Parts1, []) -> Parts1;
merge([{Tag, Props1} | Rest], Parts2) ->
    case proplists:get_value(Tag, Parts2) of
        undefined -> [{Tag, Props1} | merge(Rest, Parts2)];
        Props2 -> [{Tag, Props1 ++ Props2} | merge(Rest, proplists:delete(Tag, Parts2))]
    end.
