-module(relations_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


relations1_test() ->
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


relations2_test() ->
    TUser = {table, "foo_users", [{as, "user"}]},
    Select = {select, TUser,
              [{fields, ["user.id", "user.name"]}
              ]},
    ?assertEqual(<<"SELECT user.id, user.name FROM foo_users AS user">>,
                 erma:build(Select)),
    ok.


relations3_test() ->
    TAddress = {table, "boo_addresses", [{as, "address"}]},
    TUser = {table, "foo_users", [{as, "user"}, {has_one, TAddress}]},
    Select = {select, TUser,
              [{with, [TAddress]},
               {fields, ["user.id", "user.name", "address.state"]}
              ]},

    ?assertEqual(<<"SELECT user.id, user.name, address.state ",
                   "FROM foo_users AS user ",
                   "LEFT JOIN boo_addresses AS address ON address.id = user.address_id">>,
                 erma:build(Select)),
    ok.


%%relations3_test() ->
%%     TAddress = {table, "addresses", [{as, "address"}]},
%%     TUser = {table, "foo_users",
%%              [{as, "user"},
%%               {pk, "userID"},
%%               {has_many, TAddress, [{fk, "userID"}]}]},
%%    ok.


%%relations4_test() ->
%%     TAddress = {table, "addresses", [{as, "address"}]},
%%     TUser = {table, "foo_users",
%%              [{as, "user"},
%%               {pk, "userID"},
%%               {has_many, TAddress, [{fk, "userID"}]}]},
%%    ok.
