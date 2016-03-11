-module(test_utils).

-include_lib("eunit/include/eunit.hrl").

-export([generate/1, generate/2, generate/3]).

generate(Tests) ->
    generate({erma, build}, Tests).


generate({M, F}, Tests) ->
    generate({M, F}, [], Tests).


generate({M, F}, MoreArgs, Tests) ->
    {generator,
        fun() -> lists:map(
            fun({Query, WaitRes}) ->
                Res = erlang:apply(M, F, [Query] ++ MoreArgs),
                ?_assertEqual(WaitRes, Res) end,
            Tests)
        end}.
