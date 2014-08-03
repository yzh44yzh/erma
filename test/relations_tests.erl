-module(relations_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


relations1_test() ->
    TUser = {table, "user"},
    TEmail = {table, "email"},
    TAddress = {table, "address"},
    TAccount = {table, "account"},

    Select = {select, TUser,
              [{fields, ["email.email", "address.state", "account.name"]},
               {joins, [{left, TEmail},
                        {left, TAddress},
                        {left, TAccount}]}
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
    TUser = {table, "foo_users", [{as, "user"}]},
    Select = {select, TUser,
              [{joins, [{left, TAddress}]},
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
