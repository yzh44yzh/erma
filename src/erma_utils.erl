-module(erma_utils).

-export([valid_name/1, escape_name/1,
         format_date/1, format_time/1, format_datetime/1]).
-include("erma.hrl").


%%% module API

-spec valid_name(string()) -> boolean().
valid_name(Name) ->
    case lists:member(string:to_upper(Name), ?SQL_RESERVED_WORDS) of
        true -> false;
        false -> [FirstChar | Last] = Name,
                 case is_alpha(FirstChar) of
                     false -> false;
                     true -> case lists:dropwhile(fun valid_char/1, Last) of
                                 [] -> true;
                                 _ -> false
                             end
                 end
    end.


-spec escape_name(string()) -> string().
escape_name(Name) ->
    lists:flatten(
      case string:tokens(Name, ".") of
          [N1, "*"] -> [escape_name(N1), ".*"];
          [N1, N2] -> [escape_name(N1), ".", escape_name(N2)];
          _ -> case valid_name(Name) of
                   true -> Name;
                   false -> ["`", Name, "`"]
               end
      end).


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


-spec valid_char(char()) -> boolean().
valid_char(Char) ->
    IsDigit = is_digit(Char),
    IsAlpha = is_alpha(Char),
    if
        IsDigit -> true;
        IsAlpha -> true;
        true -> false
    end.


-spec is_digit(char()) -> boolean().
is_digit(Char) when Char >= 48 andalso Char =< 57 -> true; % 0-9
is_digit(_) -> false.


-spec is_alpha(char()) -> boolean().
is_alpha($_) -> true;
is_alpha(Char) when Char >= 65 andalso Char =< 90 -> true; % A-Z
is_alpha(Char) when Char >= 97 andalso Char =< 122 -> true; % a-z
is_alpha(_) -> false.
