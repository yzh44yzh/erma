-module(erma_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


simple_test() ->
    Q1 = {select, [], "user"},
    S1 = <<"SELECT * FROM `user`">>,
    ?assertEqual(S1, erma:build(Q1)),

    Q2 = {select, [], "user", [{where, [{"email", "some@where.com"}]}]},
    S2 = <<"SELECT * FROM `user` WHERE email = 'some@where.com'">>,
    ?assertEqual(S2, erma:build(Q2)),

    Q3 = {select, ["first_name", "last_name", "address.state"], "user",
          [{where, [{"email", "some@where.com"}]}]},
    S3 = <<"SELECT first_name, last_name, address.`state` ",
           "FROM `user` ",
           "WHERE email = 'some@where.com'">>,
    ?assertEqual(S3, erma:build(Q3)),

    Q4 = {select, ["first_name", "last_name", "address.state"], "user",
          [{joins, [{left, "address"}]}]},
    S4 = <<"SELECT first_name, last_name, address.`state` ",
           "FROM `user` ",
           "LEFT JOIN address ON address.id = `user`.address_id">>,
    ?assertEqual(S4, erma:build(Q4)),

    Q5 = {select, ["first_name", "last_name", "address.state"], "user",
          [{joins, [{left, "address"}]},
           {where, [{"email", "some@where.com"}]}
          ]},
    S5 = <<"SELECT first_name, last_name, address.`state` ",
           "FROM `user` ",
           "LEFT JOIN address ON address.id = `user`.address_id ",
           "WHERE email = 'some@where.com'">>,
    ?assertEqual(S5, erma:build(Q5)),
    ok.


append_test() ->
    Q1 = {select, ["id", "username"], "user",
          [{where, [{"email", like, "*@gmail.com"}]}]},
    S1 = <<"SELECT id, username FROM `user` WHERE email LIKE '*@gmail.com'">>,
    ?assertEqual(S1, erma:build(Q1)),

    Q2 = erma:append(Q1, [{where, [{"active", true}, {"age", '>', 18}]}, {order, ["created"]}]),
    ?assertEqual(
       {select, ["id", "username"], "user",
        [{where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]},
         {order, ["created"]}]},
       Q2),
    S2 = <<"SELECT id, username ",
           "FROM `user` ",
           "WHERE email LIKE '*@gmail.com' ",
           "AND active = true ",
           "AND age > 18 ",
           "ORDER BY created ASC">>,
    ?assertEqual(S2, erma:build(Q2)),

    Q3 = erma:append(Q2, [{limit, 20}]),
    ?assertEqual(
       {select, ["id", "username"], "user",
        [{where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]},
         {order, ["created"]},
         {limit, 20}]},
       Q3),
    S3 = <<"SELECT id, username ",
           "FROM `user` ",
           "WHERE email LIKE '*@gmail.com' ",
           "AND active = true ",
           "AND age > 18 ",
           "ORDER BY created ASC ",
           "LIMIT 20">>,
    ?assertEqual(S3, erma:build(Q3)),

    Q4 = erma:append(Q3, [{order, [{"id", desc}]}]),
    ?assertEqual(
       {select, ["id", "username"], "user",
        [{where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]},
         {order, ["created", {"id", desc}]},
         {limit, 20}]},
       Q4),
    S4 = <<"SELECT id, username ",
           "FROM `user` ",
           "WHERE email LIKE '*@gmail.com' ",
           "AND active = true ",
           "AND age > 18 ",
           "ORDER BY created ASC, id DESC ",
           "LIMIT 20">>,
    ?assertEqual(S4, erma:build(Q4)),
    ok.


order_test() ->
    Q1 = {select, ["id", "username"], "user",
          [{order, ["created"]}]},
    ?assertEqual(<<"SELECT id, username FROM `user` ",
                   "ORDER BY created ASC">>,
                 erma:build(Q1)),
    Q2 = {select, ["id", "username"], "user",
          [{order, ["created", {"username", desc}]}]},
    ?assertEqual(<<"SELECT id, username FROM `user` ",
                   "ORDER BY created ASC, username DESC">>,
                 erma:build(Q2)),
    Q3 = {select, ["id", "username"], "user",
          [{order, ["id", "created", {"last_login", asc}, {"username", desc}]}]},
    ?assertEqual(<<"SELECT id, username FROM `user` ",
                   "ORDER BY id ASC, created ASC, last_login ASC, username DESC">>,
                 erma:build(Q3)),
    ok.


offset_limit_test() ->
    Q0 = {select, ["id", "username"], "user"},
    ?assertEqual(<<"SELECT id, username FROM `user`">>,
                 erma:build(Q0)),

    Q1 = {select, ["id", "username"], "user",
          [{limit, 20}]},
    ?assertEqual(<<"SELECT id, username FROM `user` LIMIT 20">>,
                 erma:build(Q1)),

    Q2 = {select, ["id", "username"], "user",
          [{offset, 0, limit, 20}]},
    ?assertEqual(<<"SELECT id, username FROM `user` OFFSET 0, LIMIT 20">>,
                 erma:build(Q2)),

    Q3 = {select, ["id", "username"], "user",
          [{offset, 100, limit, 20}]},
    ?assertEqual(<<"SELECT id, username FROM `user` OFFSET 100, LIMIT 20">>,
                 erma:build(Q3)),

    ok.


complex_test() ->
    TAddress = {"addresses", as, "address"},
    TUser = {"foo_users", as, "user"},
    Q = {select, [], TUser,
         [{joins, [{left, TAddress, [{pk, "userID"}, {fk, "userID"}]}]},
          {where, [{"last_login", lt, {date, {2014, 1, 20}}}]}]},
    ?assertEqual(<<"SELECT * FROM foo_users AS `user` ",
                   "LEFT JOIN addresses AS address ON address.userID = `user`.userID ",
                   "WHERE last_login < '2014-01-20'">>,
                 erma:build(Q)),
    ok.


binary_test() ->
    TUser = <<"user">>,
    Q1 = {select, [], TUser},
    ?assertEqual(<<"SELECT * FROM `user`">>, erma:build(Q1)),

    Q2 = {select, [], TUser,
          [{where, [{<<"email">>, <<"some@where.com">>}]}]},
    ?assertEqual(<<"SELECT * FROM `user` WHERE email = 'some@where.com'">>,
                 erma:build(Q2)),

    Q3 = {select, [<<"first_name">>, "last_name", "address.state"], TUser,
          [{where, [{"email", <<"some@where.com">>}]}]},
    ?assertEqual(<<"SELECT first_name, last_name, address.`state` ",
                   "FROM `user` ",
                   "WHERE email = 'some@where.com'">>,
                 erma:build(Q3)),
    ok.
