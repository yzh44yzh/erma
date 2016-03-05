-module(append_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


append1_test() ->
    Q1 = {select, ["id", "username"], "user",
          [{where, [{"email", like, "*@gmail.com"}]}]},
    S1 = <<"SELECT id, username FROM \"user\" WHERE email LIKE '*@gmail.com'">>,
    ?assertEqual(S1, erma:build(Q1)),

    Q2 = erma:append(Q1, [{where, [{"active", true}, {"age", '>', 18}]}, {order, ["created"]}]),
    ?assertEqual(
       {select, ["id", "username"], "user",
           [
               {order, ["created"]},
               {where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]}
           ]},
       Q2),
    S2 = <<"SELECT id, username ",
           "FROM \"user\" ",
           "WHERE email LIKE '*@gmail.com' ",
           "AND active = true ",
           "AND age > 18 ",
           "ORDER BY created ASC">>,
    ?assertEqual(S2, erma:build(Q2)),

    Q3 = erma:append(Q2, [{limit, 20}]),
    ?assertEqual(
       {select, ["id", "username"], "user",
           [
               {limit, 20},
               {order, ["created"]},
               {where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]}
           ]},
       Q3),
    S3 = <<"SELECT id, username ",
           "FROM \"user\" ",
           "WHERE email LIKE '*@gmail.com' ",
           "AND active = true ",
           "AND age > 18 ",
           "ORDER BY created ASC ",
           "LIMIT 20">>,
    ?assertEqual(S3, erma:build(Q3)),

    Q4 = erma:append(Q3, [{order, [{"id", desc}]}]),
    ?assertEqual(
       {select, ["id", "username"], "user",
           [
               {order, ["created", {"id", desc}]},
               {limit, 20},
               {where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]}
           ]},
       Q4),
    S4 = <<"SELECT id, username ",
           "FROM \"user\" ",
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
    Q1 = {select, ["u.id", "email.email", "a.state", "account.name"], TUser,
               [{joins, [{left, TEmail}]}]},
    Q2 = erma:append(Q1, [{joins, [{left, TAddress}, {right, TAccount}]}]),
    S2 = <<"SELECT u.id, email.email, \"a\".\"state\", account.\"name\" ",
                   "FROM \"user\" AS u ",
                   "LEFT JOIN email ON email.id = u.email_id ",
                   "LEFT JOIN address AS \"a\" ON \"a\".id = u.address_id ",
                   "RIGHT JOIN account ON account.id = u.account_id">>,
    ?assertEqual(S2, erma:build(Q2)),
    ok.


append3_test() ->
    Q1 = {select, [], "post"},
    S1 = <<"SELECT * FROM post">>,
    ?assertEqual(S1, erma:build(Q1)),

    Q2 = erma:append(Q1, [{where, [{"user_id", 10}]}]),
    S2 = <<"SELECT * FROM post WHERE user_id = 10">>,
    ?assertEqual(S2, erma:build(Q2)),

    Q3 = erma:append(Q2, [{where, [{'not', {"blocked", true}},
                                   {"posted", '>', {date, {2014, 2, 20}}}]}]),
    S3 = <<"SELECT * FROM post ",
           "WHERE user_id = 10 AND (NOT blocked = true) AND posted > '2014-02-20'">>,
    ?assertEqual(S3, erma:build(Q3)),
    ok.


append4_test() ->
    Q1 = {select, ["id", "username"], <<"пользователь"/utf8>>,
          [{where, [{"email", like, "*@gmail.com"}]}]},
    ?assertEqual(<<"SELECT id, username ",
                   "FROM \"пользователь\" "/utf8,
                   "WHERE email LIKE '*@gmail.com'">>,
                 erma:build(Q1)),

    Q2 = erma:append(Q1, [{where, [{<<"активен"/utf8>>, true}, {<<"возраст"/utf8>>, '>', 18}]},
                          {order, [<<"дата_создания"/utf8>>]}]),
    ?assertEqual(
       {select, ["id", "username"], <<"пользователь"/utf8>>,
           [
               {order, [<<"дата_создания"/utf8>>]},
               {where, [{"email", like, "*@gmail.com"}, {<<"активен"/utf8>>, true}, {<<"возраст"/utf8>>, '>', 18}]}
           ]},
       Q2),
    S2 = <<"SELECT id, username ",
           "FROM \"пользователь\" "/utf8,
           "WHERE email LIKE '*@gmail.com' ",
           "AND \"активен\" = true "/utf8,
           "AND \"возраст\" > 18 "/utf8,
           "ORDER BY \"дата_создания\" ASC"/utf8>>,
    ?assertEqual(S2, erma:build(Q2)),
    ok.


append5_test() ->
    Q = {select, [id, name], users, [{where, [{email, "some@where.com"}]}]},
    ?assertEqual(<<"SELECT id, \"name\" FROM users WHERE email = 'some@where.com'">>, erma:build(Q)),

    Entities = [
        {joins, [{left, "email"}]},
        {joins, [{left, "category"}]},
        {joins, [{inner, "tag"}]},
        {where, [{id, 5}]},
        {where, [{age, gt, 30}]},
        {where, [{city, "Minsk"}]},
        {order, [id]},
        {order, [{name, desc}]},
        {limit, 10},
        {limit, 20},
        {limit, 30},
        {group, [city, age]},
        {having, [{age, gt, 30}]},
        {group, [tag]},
        {having, [{city, "Minsk"}, {id, gt, 10}]}
    ],
    WaitEntities = [
        {joins,[{left,"email"}, {left,"category"}, {inner,"tag"}]},
        {where,[{email,"some@where.com"}, {id,5}, {age,gt,30}, {city,"Minsk"}]},
        {group, [city, age, tag]},
        {having, [{age, gt, 30}, {city, "Minsk"}, {id, gt, 10}]},
        {order,[id, {name,desc}]},
        {limit,30}
    ],

    GotRes = erma:append(Q, Entities),
    {select, _, _, GotEntities} = GotRes,
    ?assertEqual(lists:sort(WaitEntities), lists:sort(GotEntities)),

    SQL = <<"SELECT id, \"name\" ",
        "FROM users ",
        "LEFT JOIN email ON email.id = users.email_id ",
        "LEFT JOIN category ON category.id = users.category_id ",
        "INNER JOIN tag ON tag.id = users.tag_id ",
        "WHERE email = 'some@where.com' ",
        "AND id = 5 ",
        "AND age > 30 ",
        "AND city = 'Minsk' ",
        "GROUP BY city, age, tag ",
        "HAVING age > 30 ",
        "AND city = 'Minsk' ",
        "AND id > 10 ",
        "ORDER BY id ASC, \"name\" DESC ",
        "LIMIT 30">>,

    ?assertEqual(SQL, erma:build(GotRes)),
    ok.