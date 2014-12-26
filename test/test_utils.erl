-module(test_utils).

-include_lib("eunit/include/eunit.hrl").

-export([generate/1]).

generate(Tests) ->
    {generator, fun() -> lists:map(
                           fun({Q, S}) -> ?_assertEqual(S, erma:build(Q)) end,
                           Tests)
                end}.
