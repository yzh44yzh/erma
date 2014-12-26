-module(select_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


select1_test() ->
    ?assertEqual(<<"SELECT id, username FROM users WHERE username = 'chris' ",
                   "ORDER BY created ASC OFFSET 3, LIMIT 5">>,
                 erma:build({select, ["id", "username"], "users",
                             [{where, [{"username", "chris"}]},
                              {order, ["created"]},
                              {offset, 3, limit, 5}]})),

    ?assertEqual(<<"SELECT email.* FROM email ",
                   "WHERE (email.email LIKE '%@gmail.com' AND email.user_id = 1)">>,
                 erma:build({select, ["email.*"], "email",
                             [{where, [{'and', [{"email.email", like, "%@gmail.com"},
                                                {"email.user_id", 1}]}]}
                             ]})),
    ok.


select2_test() ->
    ?assertEqual(<<"SELECT * FROM users">>,
                 erma:build({select, [], "users"})),
    ?assertEqual(<<"SELECT u.* FROM users AS u">>,
                 erma:build({select, ["u.*"], {"users", as, "u"}})),
    ?assertEqual(<<"SELECT users.id, users.username FROM users">>,
                 erma:build({select, ["users.id", "users.username"], "users"})),
    ?assertEqual(<<"SELECT * FROM users WHERE (email = 'hey@hey.com' AND username = 'chris')">>,
                 erma:build({select, [], "users",
                             [{where, [{'and', [{"email", "hey@hey.com"},
                                                {"username", "chris"}]}]}]})),
    ?assertEqual(<<"SELECT * FROM users WHERE username = 'chris' ORDER BY created ASC">>,
                 erma:build({select, [], <<"users">>,
                             [{where, [{"username", "chris"}]},
                              {order, ["created"]}]})),
    ?assertEqual(<<"SELECT * FROM users WHERE active = true ORDER BY created ASC OFFSET 3, LIMIT 5">>,
                 erma:build({select, [], users,
                             [{where, [{"active", true}]},
                              {order, ["created"]},
                              {offset, 3, limit, 5}]})),
    ok.


select3_test() ->
    Q1 = <<"SELECT DISTINCT id FROM users">>,
    S1 = {select_distinct, [id], "users"},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"SELECT DISTINCT id, `name`, age FROM `user`">>,
    S2 = {select_distinct, ["id", name, <<"age">>], "user"},
    ?assertEqual(Q2, erma:build(S2)),

    ok.


select4_test() ->
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
    ok.


select5_test() ->
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


select6_test() ->
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

    Q4 = {select, ["id", "username"], "user",
          [{order, ["id", "created", {"last_login", asc}, {"username", desc}]}]},
    S4 = <<"SELECT id, username FROM `user` ORDER BY id ASC, created ASC, last_login ASC, username DESC">>,
    ?assertEqual(S4, erma:build(Q4)),
    ok.
