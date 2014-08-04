-module(erma_utils).

-export([format_date/1, format_time/1, format_datetime/1]).


%%% module API

-spec format_date(calendar:date()) -> string().
format_date({Y, M, D}) ->
    lists:flatten([add_zero(Y), "-", add_zero(M), "-", add_zero(D)]).


-spec format_time(calendar:time()) -> string().
format_time({H, M, S}) ->
    lists:flatten([add_zero(H), ":", add_zero(M), ":", add_zero(S)]).


-spec format_datetime(calendar:datetime()) -> string().
format_datetime({Date, Time}) ->
    lists:flatten([format_date(Date), " ", format_time(Time)]).


%%% inner functions

-spec add_zero(integer()) -> string().
add_zero(Num) when Num > 9 -> integer_to_list(Num);
add_zero(Num) -> [$0 | integer_to_list(Num)].
