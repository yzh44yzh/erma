-module(append_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


append1_test() ->
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


append2_test() ->
    TUser = {"user", as, "u"},
    TEmail = "email",
    TAddress = {"address", as, "a"},
    TAccount = "account",
    Select1 = {select, ["u.id", "email.email", "a.state", "account.name"], TUser,
               [{joins, [{left, TEmail}]}]},
    Select2 = erma:append(Select1, [{joins, [{left, TAddress}, {right, TAccount}]}]),
    ?assertEqual(<<"SELECT u.id, email.email, `a`.`state`, account.`name` ",
                   "FROM `user` AS u ",
                   "LEFT JOIN email ON email.id = u.email_id ",
                   "LEFT JOIN address AS `a` ON `a`.id = u.address_id ",
                   "RIGHT JOIN account ON account.id = u.account_id">>,
                 erma:build(Select2)),
    ok.


append3_test() ->
    Select0 = {select, [], "post"},
    ?assertEqual(<<"SELECT * FROM post">>, erma:build(Select0)),

    Select1 = erma:append(Select0, [{where, [{"user_id", 10}]}]),
    ?assertEqual(<<"SELECT * FROM post WHERE user_id = 10">>,
                 erma:build(Select1)),

    Select2 = erma:append(Select1, [{where, [{'not', {"blocked", true}},
                                             {"posted", '>', {date, {2014, 2, 20}}}]}]),
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE user_id = 10 AND (NOT blocked = true) AND posted > '2014-02-20'">>,
                 erma:build(Select2)),
    ok.


append4_test() ->
    Select0 = {select, ["id", "username"], <<"пользователь"/utf8>>,
               [{where, [{"email", like, "*@gmail.com"}]}]},
    ?assertEqual(<<"SELECT id, username ",
                   "FROM `пользователь` "/utf8,
                   "WHERE email LIKE '*@gmail.com'">>,
                 erma:build(Select0)),

    Select1 = erma:append(Select0, [{where, [{<<"активен"/utf8>>, true}, {<<"возраст"/utf8>>, '>', 18}]},
                                    {order, [<<"дата_создания"/utf8>>]}]),
    ?assertEqual(
       {select, ["id", "username"], <<"пользователь"/utf8>>,
        [{where, [{"email", like, "*@gmail.com"}, {<<"активен"/utf8>>, true}, {<<"возраст"/utf8>>, '>', 18}]},
         {order, [<<"дата_создания"/utf8>>]}
        ]},
       Select1),
    ?assertEqual(<<"SELECT id, username ",
                   "FROM `пользователь` "/utf8,
                   "WHERE email LIKE '*@gmail.com' ",
                   "AND `активен` = true "/utf8,
                   "AND `возраст` > 18 "/utf8,
                   "ORDER BY `дата_создания` ASC"/utf8>>,
                 erma:build(Select1)),
    ok.
