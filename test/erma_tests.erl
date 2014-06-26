-module(erma_tests).
-include_lib("eunit/include/eunit.hrl").


simple_test() ->
    TUser = {table, "user"},
    Select1 = {select, TUser, []},
    ?assertEqual(<<"SELECT * FROM user">>, erma:build(Select1)),

    Select2 = {select, TUser,
              [{where, [{"email", "some@where.com"}]}]},
    ?assertEqual(<<"SELECT * FROM user WHERE email = 'some@where.com'">>,
                 erma:build(Select2)),

    Select3 = {select, TUser,
               [{fields, ["first_name", "last_name", "address.state"]},
                {where, [{"email", "some@where.com"}]}
              ]},
    ?assertEqual(<<"SELECT first_name, last_name, address.state ",
                   "FROM user ",
                   "WHERE email = 'some@where.com'">>,
                 erma:build(Select3)),

    TAddress = {table, "address"},
    Select4 = {select, TUser,
              [{with, [TAddress]},
               {fields, ["first_name", "last_name", "address.state"]}
              ]},
    ?assertEqual(<<"SELECT first_name, last_name, address.state ",
                   "FROM user ",
                   "LEFT JOIN address ON address.id = user.address_id">>,
                 erma:build(Select4)),
    Select5 = {select, TUser,
              [{with, [TAddress]},
               {fields, ["first_name", "last_name", "address.state"]},
               {where, [{"email", "some@where.com"}]}
              ]},

    ?assertEqual(<<"SELECT first_name, last_name, address.state ",
                   "FROM user ",
                   "LEFT JOIN address ON address.id = user.address_id ",
                   "WHERE email = 'some@where.com'">>,
                 erma:build(Select5)),
    ok.


append_test() ->
    Select0 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {where, [{"email", like, "*@gmail.com"}]}
               ]},
    ?assertEqual(<<"SELECT id, username ",
                   "FROM user ",
                   "WHERE email LIKE '*@gmail.com'">>,
                 erma:build(Select0)),

    Select1 = erma:append(Select0, [{where, [{"active", true}, {"age", '>', 18}]},
                                    {order, ["created"]}]),
    ?assertEqual(
       {select, {table, "user"},
        [{fields, ["id", "username"]},
         {where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]},
         {order, ["created"]}
        ]},
       Select1),
    ?assertEqual(<<"SELECT id, username ",
                   "FROM user ",
                   "WHERE email LIKE '*@gmail.com' ",
                   "AND active = true ",
                   "AND age > 18 ",
                   "ORDER BY created ASC">>,
                 erma:build(Select1)),

    Select2 = erma:append(Select1, {limit, 20}),
    ?assertEqual(
       {select, {table, "user"},
        [{fields, ["id", "username"]},
         {where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]},
         {order, ["created"]},
         {limit, 20}
        ]},
       Select2),
    ?assertEqual(<<"SELECT id, username ",
                   "FROM user ",
                   "WHERE email LIKE '*@gmail.com' ",
                   "AND active = true ",
                   "AND age > 18 ",
                   "ORDER BY created ASC ",
                   "LIMIT 20">>,
                 erma:build(Select2)),

    Select3 = erma:append(Select2, [{order, [{"id", desc}]}]),
    ?assertEqual(
       {select, {table, "user"},
        [{fields, ["id", "username"]},
         {where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]},
         {order, ["created", {"id", desc}]},
         {limit, 20}
        ]},
       Select3),
    ?assertEqual(<<"SELECT id, username ",
                   "FROM user ",
                   "WHERE email LIKE '*@gmail.com' ",
                   "AND active = true ",
                   "AND age > 18 ",
                   "ORDER BY created ASC, id DESC ",
                   "LIMIT 20">>,
                 erma:build(Select3)),
    ok.


relations_test() ->
    TEmail = {table, "email"},
    TAddress = {table, "address"},
    TAccount = {table, "account"},
    TUser = {table, "user",
             [{has_one, TEmail},
              {has_one, TAddress},
              {has_one, TAccount}
             ]},

    Select = {select, TUser,
              [{with, [TEmail, TAddress, TAccount]},
               {fields, ["email.email", "address.state", "account.name"]}
              ]},

    ?assertEqual(<<"SELECT email.email, address.state, account.name ",
                   "FROM user ",
                   "LEFT JOIN email ON email.id = user.email_id ",
                   "LEFT JOIN address ON address.id = user.address_id ",
                   "LEFT JOIN account ON account.id = user.account_id">>,
                 erma:build(Select)),
    ok.


%% complex_test() ->
%%     TAddress = {table, "addresses", [{as, "address"}]},
%%     TUser = {table, "foo_users",
%%              [{as, "user"},
%%               {pk, "userID"},
%%               {has_many, TAddress, [{fk, "userID"}]}]},

%%     Select1 = {select, TUser,
%%                [{with, TAddress},
%%                 {where, [{"last_login", lt, "a_week_ago"}]}]},

%%     Select2 = {select, TUser,
%%                [{aggregate, count, "*", "cnt"},
%%                 {where, [{'or', [{"visits", gt, 20},
%%                                  {"last_login", "a_year_ago"}]}]}]},
%%     ok.
