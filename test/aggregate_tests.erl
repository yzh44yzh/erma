-module(aggregate_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

agg1_test() ->
    Q1 = <<"SELECT COUNT(id) FROM users">>,
    A1 = {select, "users", [{fields, [{count, "id"}]}]},
    ?assertEqual(Q1, erma:build(A1)),

    Q2 = <<"SELECT id, MAX(age) FROM users WHERE id > 3">>,
    A2 = {select, "users", [{fields, ["id", {max, "age"}]},
                            {where, [{"id", '>', 3}]}]},
    ?assertEqual(Q2, erma:build(A2)),

    Q3 = <<"SELECT id, AVG(age), age FROM users WHERE id > 3">>,
    A3 = {select, "users", [{fields, ["id", {avg, "age"}, "age"]},
                            {where, [{"id", '>', 3}]}]},
    ?assertEqual(Q3, erma:build(A3)),
    ok.

agg2_test() ->
    Q1 = <<"SELECT DISTINCT STDEV(users.age) FROM users">>,
    A1 = {select, "users", [{fields, distinct, [{stdev, "users.age"}]}]},
    ?assertEqual(Q1, erma:build(A1)),

    Q2 = <<"SELECT MAX(users.age), AVG(users.height) FROM users WHERE users.`state` <> 'blocked'">>,
    A2 = {select, "users", [{fields, [{max, "users.age"}, {avg, "users.height"}]},
                            {where, [{"users.state", '<>', "blocked"}]}]},
    ?assertEqual(Q2, erma:build(A2)),
    ok.
