-module(update_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


update1_test() ->
    Q1 = <<"UPDATE users SET `first` = 'Chris', `last` = 'Granger' WHERE id = 3">>,
    U1 = {update, "users",
          [{"first", "Chris"},
           {"last", "Granger"}],
          [{where, [{"id", 3}]}]},
    ?assertEqual(Q1, erma:build(U1)),

    Q2 = <<"UPDATE users SET `first` = ?, `last` = ? WHERE id = ?">>,
    U2 = {update, "users",
          [{"first", "?"}, {"last", "?"}],
          [{where, [{"id", "?"}]}]},
    ?assertEqual(Q2, erma:build(U2)),

    Q3 = <<"UPDATE users SET `first` = 'Chris', `last` = 'Granger'">>,
    U3 = {update, "users",
          [{"first", "Chris"},
           {"last", "Granger"}]},
    ?assertEqual(Q3, erma:build(U3)),

    Q4 = <<"UPDATE users SET `first` = ?, `last` = ?">>,
    U4 = {update, "users", [{"first", "?"}, {"last", "?"}]},
    ?assertEqual(Q4, erma:build(U4)),
    ok.


update2_test() ->
    Q1 = <<"UPDATE users SET `first` = 'Chris'">>,
    U1 = {update, "users", [{"first", "Chris"}]},
    ?assertEqual(Q1, erma:build(U1)),

    Q2 = <<"UPDATE users SET `first` = ? WHERE id = ?">>,
    U2 = {update, "users", [{"first", "?"}], [{where, [{"id", "?"}]}]},
    ?assertEqual(Q2, erma:build(U2)),

    Q3 = <<"UPDATE users SET `first` = 'Chris', `last` = ? WHERE id = ?">>,
    U3 = {update, "users", [{"first", "Chris"}, {"last", "?"}], [{where, [{"id", "?"}]}]},
    ?assertEqual(Q3, erma:build(U3)),
    ok.
