-module(placeholders_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


placeholders1_test() ->
    %% mysql placeholders
    Q1 = <<"INSERT INTO users (`first`, `last`, age) VALUES (?, ?, ?)">>,
    S1 = {insert, {table, "users"}, ["first", "last", "age"]},
    ?assertEqual(Q1, erma:build(S1)),

    %% postgresql placeholders
    Q2 = <<"INSERT INTO users (`first`, `last`, age) VALUES ($1, $2, $3)">>,
    S2 = {insert, {table, "users"}, [{"first", "$1"}, {"last", "$2"}, {"age", "$3"}]},
    ?assertEqual(Q2, erma:build(S2)),
    ok.


placeholders2_test() ->
    Q1 = <<"SELECT * FROM users WHERE ((`last` = ? AND `name` = ?) OR email = ? OR age > ?)">>,
    S1 = {select, {table, "users"},
          [{where, [{'or', [{'and', [{"last", "?"}, {"name", "?"}]},
                            {"email", "?"},
                            {"age", gt, "?"}]}]}
          ]},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"SELECT * FROM users WHERE ((`last` = $1 AND `name` = $1) OR email = $2 OR age > $3)">>,
    S2 = {select, {table, "users"},
          [{where, [{'or', [{'and', [{"last", "$1"}, {"name", "$1"}]},
                            {"email", "$2"},
                            {"age", gt, "$3"}]}]}
          ]},
    ?assertEqual(Q2, erma:build(S2)),
    ok.


placeholders3_test() ->
    Q1 = <<"UPDATE users SET `first` = 'Chris', `last` = ? WHERE id = ?">>,
    U1 = {update, {table, "users"}, [{"first", "Chris"}, "last"], {where, [{"id", "?"}]}},
    ?assertEqual(Q1, erma:build(U1)),

    Q2 = <<"UPDATE users SET `first` = 'Chris', `last` = $2 WHERE id = $1">>,
    U2 = {update, {table, "users"}, [{"first", "Chris"}, {"last", "$2"}], {where, [{"id", "$1"}]}},
    ?assertEqual(Q2, erma:build(U2)),
    ok.


placeholders4_test() ->
    Q1 = <<"DELETE FROM users WHERE id = ?">>,
    D1 = {delete, {table, "users"}, {where, [{"id", "?"}]}},
    ?assertEqual(Q1, erma:build(D1)),

    Q2 = <<"DELETE FROM users WHERE id = $1">>,
    D2 = {delete, {table, "users"}, {where, [{"id", "$1"}]}},
    ?assertEqual(Q2, erma:build(D2)),
    ok.
