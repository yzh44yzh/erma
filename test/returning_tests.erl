-module(returning_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


returning1_test() ->
    Q1 = <<"INSERT INTO users (`first`, `last`) VALUES ('Chris', 'Granger') RETURNING id">>,
    S1 = {insert, "users", ["first", "last"], ["Chris", "Granger"], [{returning, id}]},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"INSERT INTO users (`first`, `last`, age) VALUES ('Bob', 'Dou', 25) ",
           "RETURNING id, `first`, `last`">>,
    S2 = {insert, "users", ["first", "last", "age"], ["Bob", "Dou", 25],
          [{returning, ["id", "first", "last"]}]},
    ?assertEqual(Q2, erma:build(S2)),

    Q3 = <<"INSERT INTO users (`first`, `last`) VALUES (?, ?) RETURNING id">>,
    S3 = {insert, "users", ["first", "last"], ["?", "?"], [{returning, id}]},
    ?assertEqual(Q3, erma:build(S3)),

    Q4 = <<"INSERT INTO users (`first`, `last`, age) ",
           "VALUES ('Bill', 'Foo', 24), ('Bob', 'Dou', 25), ('Helen', 'Rice', 21) ",
           "RETURNING id">>,
    S4 = {insert_rows, "users", ["first", "last", "age"],
          [["Bill", "Foo", 24], ["Bob", "Dou", 25], ["Helen", "Rice", 21]],
          [{returning, id}]},
    ?assertEqual(Q4, erma:build(S4)),

    Q5 = <<"INSERT INTO users VALUES (5, 'Bob', 'Dou', 25) ",
           "RETURNING `name`, age, id">>,
    I5 = {insert, <<"users">>, [],
          [5, "Bob", "Dou", 25],
          [{returning, ["name", <<"age">>, "id"]}]},
    ?assertEqual(Q5, erma:build(I5)),

    Q6 = <<"INSERT INTO users VALUES (1, 'Bob', 'Dou', 65), (6, 'Bill', 'Foo', 31) ",
           "RETURNING id">>,
    I6 = {insert_rows, "users", [],
          [[1, "Bob", "Dou", 65], [6, "Bill", "Foo", 31]],
          [{returning, id}]},
    ?assertEqual(Q6, erma:build(I6)),
    ok.


returning2_test() ->
    Q1 = <<"UPDATE users SET `first` = 'Chris' RETURNING id">>,
    U1 = {update, "users", [{"first", "Chris"}], [{returning, id}]},
    ?assertEqual(Q1, erma:build(U1)),

    Q2 = <<"UPDATE users SET `first` = ? WHERE id = ? RETURNING id, `first`">>,
    U2 = {update, <<"users">>, ["first"],
          [{where, [{"id", "?"}]},
           {returning, [<<"id">>, <<"first">>]}]},
    ?assertEqual(Q2, erma:build(U2)),

    Q3 = <<"UPDATE users SET `first` = 'Chris', `last` = ? WHERE id = ? ",
           "RETURNING id, `name`, `first`, `last`, age">>,
    U3 = {update, "users", [{"first", "Chris"}, "last"],
          [{where, [{"id", "?"}]},
           {returning, ["id", "name", <<"first">>, "last", <<"age">>]}]},
    ?assertEqual(Q3, erma:build(U3)),

    Q4 = <<"UPDATE users SET `first` = ?, `last` = ? RETURNING id">>,
    U4 = {update, "users",
          ["first", "last"],
          [{returning, ["id"]}]},
    ?assertEqual(Q4, erma:build(U4)),

    Q5 = <<"UPDATE users SET `first` = 'Chris', `last` = 'Granger' ",
           "WHERE id = 3 RETURNING id">>,
    U5 = {update, "users",
          [{"first", "Chris"},
           {"last", "Granger"}],
          [{where, [{"id", 3}]},
           {returning, id}]},
    ?assertEqual(Q5, erma:build(U5)),
    ok.


returning3_test() ->
    Q1 = <<"DELETE FROM users WHERE id = 3 RETURNING id">>,
    D1 = {delete, "users", [{where, [{"id", 3}]}, {returning, id}]},
    ?assertEqual(Q1, erma:build(D1)),

    Q2 = <<"DELETE FROM users RETURNING id">>,
    D2 = {delete, "users", [{where, []}, {returning, id}]},
    ?assertEqual(Q2, erma:build(D2)),

    Q3 = <<"DELETE FROM users WHERE id = 3 RETURNING id">>,
    D3 = {delete, "users", [{where, [{"id", 3}]}, {returning, id}]},
    ?assertEqual(Q3, erma:build(D3)),

    Q4 = <<"DELETE FROM users RETURNING `name`, age">>,
    D4 = {delete, "users", [{returning, ["name", "age"]}]},
    ?assertEqual(Q4, erma:build(D4)),

    Q5 = <<"DELETE FROM users RETURNING id">>,
    D5 = {delete, "users", [{returning, id}]},
    ?assertEqual(Q5, erma:build(D5)),

    ok.
