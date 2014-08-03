-module(erma_tests).

-include("erma.hrl").
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


order_test() ->
    Select1 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {order, ["created"]}
               ]},
    ?assertEqual(<<"SELECT id, username FROM user ",
                   "ORDER BY created ASC">>,
                 erma:build(Select1)),
    Select2 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {order, ["created", {"username", desc}]}
               ]},
    ?assertEqual(<<"SELECT id, username FROM user ",
                   "ORDER BY created ASC, username DESC">>,
                 erma:build(Select2)),
    Select3 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {order, ["id", "created", {"last_login", asc}, {"username", desc}]}
               ]},
    ?assertEqual(<<"SELECT id, username FROM user ",
                   "ORDER BY id ASC, created ASC, last_login ASC, username DESC">>,
                 erma:build(Select3)),
    ok.


offset_limit_test() ->
    Select0 = {select, {table, "user"},
               [{fields, ["id", "username"]}]},
    ?assertEqual(<<"SELECT id, username FROM user">>,
                 erma:build(Select0)),
    Select1 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {limit, 20}
               ]},
    ?assertEqual(<<"SELECT id, username FROM user ",
                   "LIMIT 20">>,
                 erma:build(Select1)),
    Select2 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {limit, 20}, {offset, 0}
               ]},
    ?assertEqual(<<"SELECT id, username FROM user ",
                   "OFFSET 0, LIMIT 20">>,
                 erma:build(Select2)),
    Select3 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {offset, 100}, {limit, 20}
               ]},
    ?assertEqual(<<"SELECT id, username FROM user ",
                   "OFFSET 100, LIMIT 20">>,
                 erma:build(Select3)),
    Select4 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {offset, 10}
               ]},
    ?assertEqual(<<"SELECT id, username FROM user ",
                   "OFFSET 10">>,
                 erma:build(Select4)),
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
