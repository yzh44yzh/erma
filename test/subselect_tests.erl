-module(subselect_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% module API

subselect1_test() ->
    SQ = {select, ["id"], "users", [{where, [{"age", gt, 18}]}]},
    Q = {select, [], "users", [{where, [{"id", in, SQ}]}]},
    S = <<"SELECT * FROM users WHERE id IN (SELECT id FROM users WHERE age > 18)">>,
    ?assertEqual(S, erma:build(Q)),
    ok.


subselect2_test() ->
    SQ = {select, ["state_id"], "user_states", [{where, [{"type", '<>', "obsolete"}]}]},
    Q = {update, "users", [{"level", 15}], [{where, [{"state_id", in, SQ}]}]},
    S = <<"UPDATE users SET `level` = 15 ",
          "WHERE state_id IN ",
          "(SELECT state_id FROM user_states WHERE `type` <> 'obsolete')">>,
    ?assertEqual(S, erma:build(Q)),
    ok.


subselect3_test() ->
    Q = {delete, "users",
         [{where, [{"age", not_in,
                    {select, ["age_limit"], "settings", [{where, [{"region", "RU"}]}]}
                   }]}]},
    S = <<"DELETE FROM users WHERE age NOT IN ",
          "(SELECT age_limit FROM settings WHERE region = 'RU')">>,
    ?assertEqual(S, erma:build(Q)),
    ok.


subselect4_test() ->
    Q = {select, [], "orders",
         [{where, [{"snum",
                    {select, ["snum"], "sales", [{where, [{"id", gt, 777}]}]}
                   }]}]},
    S = <<"SELECT * FROM orders WHERE snum = ",
          "(SELECT snum FROM sales WHERE id > 777)">>,
    ?assertEqual(S, erma:build(Q)),
    ok.


subselect5_test() ->
    Q = {select, ["price"], "orders",
         [{where, [{"price", '>',
                    {select, [{avg, "price"}], "orders", [{where, [{"id", '>', 100}]}]}
                   }]}]},
    S = <<"SELECT price FROM orders WHERE price > ",
          "(SELECT AVG(price) FROM orders WHERE id > 100)">>,
    ?assertEqual(S, erma:build(Q)),
    ok.
