-module(delete_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


delete1_test() ->
    Q = <<"DELETE FROM users WHERE id = 3">>,
    D = {delete, "users", [{where, [{"id", 3}]}]},
    ?assertEqual(Q, erma:build(D)),
    ok.


delete2_test() ->
    Q = <<"DELETE FROM users">>,
    D = {delete, <<"users">>, [{where, []}]},
    ?assertEqual(Q, erma:build(D)),
    ok.


delete3_test() ->
    Q = <<"DELETE FROM users WHERE id = 3">>,
    D = {delete, users, [{where, [{"id", 3}]}]},
    ?assertEqual(Q, erma:build(D)),
    ok.
