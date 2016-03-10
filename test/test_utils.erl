-module(test_utils).

-include_lib("eunit/include/eunit.hrl").

-export([generate/1, generate/2]).

generate(Tests) ->
    generate({erma, build}, Tests).


generate({M, F}, Tests) ->
    {generator,
        fun() -> lists:map(
            fun({Q, S}) -> ?_assertEqual(S, erlang:apply(M, F, [Q])) end,
            Tests)
        end}.
