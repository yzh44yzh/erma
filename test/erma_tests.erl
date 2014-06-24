-module(erma_tests).
-include_lib("eunit/include/eunit.hrl").


simple_test() ->
    TAddress = {table, "address"},
    TUser = {table, "user", [{has_one, TAddress}]},

    Select = {select, TUser,
              [{with, [TAddress]},
               {fields, ["first_name", "last_name", "address.state"]},
               {where, [{"email", "some@where.com"}]}
              ]},

    ?assertEqual(<<"SELECT first_name, last_name, address.state",
                   "FROM user ",
                   "LEFT JOIN address ON user.address_id = address.id ",
                   "WHERE email = 'some@where.com'">>,
                 erma:build(Select)),
    ok.


append_test() ->
    Select0 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {where, [{"email", like, "*@gmail.com"}]}
               ]},
    ?assertEqual(<<"SELECT id, username",
                   "FROM user ",
                   "WHERE email LIKE '*@gmail.com' ">>,
                 erma:build(Select0)),

    Select1 = erma:append(Select0, [{where, [{"active", true}, {"age", '>', 18}]},
                                    {order, "created"}]),
    ?assertEqual(
       {select, {table, "user"},
        [{fields, ["id", "username"]},
         {where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]},
         {order, "created"}
        ]},
       Select1),
    ?assertEqual(<<"SELECT id, username",
                   "FROM user ",
                   "WHERE email LIKE '*@gmail.com' ",
                   "AND active = true ",
                   "AND age > 18 ",
                   "ORDER BY created ">>,
                 erma:build(Select1)),

    Select2 = erma:append(Select1, {limit, 20}),
    ?assertEqual(
       {select, {table, "user"},
        [{fields, ["id", "username"]},
         {where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]},
         {order, "created"},
         {limit, 20}
        ]},
       Select2),
    ?assertEqual(<<"SELECT id, username",
                   "FROM user ",
                   "WHERE email LIKE '*@gmail.com' ",
                   "AND active = true ",
                   "AND age > 18 ",
                   "ORDER BY created ",
                   "LIMIT 20" >>,
                 erma:build(Select2)),
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
                   "LEFT JOIN email ON user.email_id = email.id ",
                   "LEFT JOIN address ON user.address_id = address.id ",
                   "LEFT JOIN account ON user.account_id = account.id ">>,
                 erma:build(Select)),
    ok.


%% TODO: Built for the real world.
%% http://sqlkorma.com/
%%
%% (defentity address
%%   (table :__addresses :address))
%%
%% (defentity users
%%   (table :somecrazy_table_name :users)
%%   (pk :userID)
%%   (has-many address {:fk :userID}))
%%
%% (select users
%%   (with address)
%%   (where {:last_login [< a-week-ago]}))
%%
%% (select users
%%   (aggregate (count :*) :cnt)
%%   (where (or (> :visits 20)
%%              (< :last_login a-year-ago))))
