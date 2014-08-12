-module(insert_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


insert1_test() ->
    Q1 = <<"INSERT INTO users (`first`, `last`) VALUES ('Chris', 'Granger')">>,
    S1 = {insert, {table, "users"},
          [{"first", "Chris"},
           {"last", "Granger"}]},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"INSERT INTO users (`first`, `last`, age) VALUES ('Bob', 'Dou', 25)">>,
    S2 = {insert, {table, "users"},
          [{"first", "Bob"},
           {"last", "Dou"},
           {"age", 25}]},
    ?assertEqual(Q2, erma:build(S2)),

    Q3 = <<"INSERT INTO users (`first`, `last`) VALUES (?, ?)">>,
    S3 = {insert, {table, "users"},
          ["first", "last"]},
    ?assertEqual(Q3, erma:build(S3)),

    Q4 = <<"INSERT INTO users (`first`, `last`, age) VALUES (?, ?, ?)">>,
    S4 = {insert, {table, "users"},
          ["first", "last", "age"]},
    ?assertEqual(Q4, erma:build(S4)),
    ok.


insert2_test() ->
    Q1 = <<"INSERT INTO users (`first`, `last`) ",
           "VALUES ('Chris', 'Granger'), ('Bob', 'Dou'), ('Helen', 'Rice')">>,
    S1 = {insert, {table, "users"},
          ["first", "last"],
          [["Chris", "Granger"], ["Bob", "Dou"], ["Helen", "Rice"]]},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"INSERT INTO users (`first`, `last`, age) ",
           "VALUES ('Bill', 'Foo', 24), ('Bob', 'Dou', 25), ('Helen', 'Rice', 21)">>,
    S2 = {insert, {table, "users"},
          ["first", "last", "age"],
          [["Bill", "Foo", 24], ["Bob", "Dou", 25], ["Helen", "Rice", 21]]},
    ?assertEqual(Q2, erma:build(S2)),
    ok.


insert3_test() ->
    Q1 = <<"INSERT INTO users VALUES (1, 'Bob', 'Dou', 25)">>,
    I1 = {insert, {table, "users"}, [], [[1, "Bob", "Dou", 25]]},
    ?assertEqual(Q1, erma:build(I1)),

    Q2 = <<"INSERT INTO users VALUES (1, 'Bob', 'Dou', 25), (2, 'Bill', 'Foo', 31)">>,
    I2 = {insert, {table, "users"}, [], [[1, "Bob", "Dou", 25], [2, "Bill", "Foo", 31]]},
    ?assertEqual(Q2, erma:build(I2)),
    ok.
