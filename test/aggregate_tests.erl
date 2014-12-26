-module(aggregate_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

agg1_test() ->
    Q1 = <<"SELECT COUNT(id) FROM users">>,
    A1 = {select, [{count, "id"}], "users"},
    ?assertEqual(Q1, erma:build(A1)),

    Q2 = <<"SELECT id, MAX(age) FROM users WHERE id > 3">>,
    A2 = {select, ["id", {max, "age"}], "users", [{where, [{"id", '>', 3}]}]},
    ?assertEqual(Q2, erma:build(A2)),

    Q3 = <<"SELECT id, AVG(age), age FROM users WHERE id > 3">>,
    A3 = {select, ["id", {avg, "age"}, "age"], "users", [{where, [{"id", '>', 3}]}]},
    ?assertEqual(Q3, erma:build(A3)),
    ok.

agg2_test() ->
    Q1 = <<"SELECT DISTINCT STDEV(users.age) FROM users">>,
    A1 = {select_distinct, [{stdev, "users.age"}], "users"},
    ?assertEqual(Q1, erma:build(A1)),

    Q2 = <<"SELECT MAX(users.age), AVG(users.height) FROM users WHERE users.`state` <> 'blocked'">>,
    A2 = {select, [{max, "users.age"}, {avg, "users.height"}], "users",
          [{where, [{"users.state", '<>', "blocked"}]}]},
    ?assertEqual(Q2, erma:build(A2)),
    ok.
