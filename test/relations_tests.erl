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
    TUser = {table, "foo_users", as, "user"},
    Select = {select, TUser,
              [{fields, ["user.id", "user.name"]}
              ]},
    ?assertEqual(<<"SELECT user.id, user.name FROM foo_users AS user">>,
                 erma:build(Select)),
    ok.


relations3_test() ->
    TAddress = {table, "boo_addresses", as, "address"},
    TUser = {table, "foo_users", as, "user"},
    Select = {select, TUser,
              [{joins, [{left, TAddress}]},
               {fields, ["user.id", "user.name", "address.state"]}
              ]},

    ?assertEqual(<<"SELECT user.id, user.name, address.state ",
                   "FROM foo_users AS user ",
                   "LEFT JOIN boo_addresses AS address ON address.id = user.boo_addresses_id">>,
                 erma:build(Select)),
    ok.


relations4_test() ->
    TUser = {table, "foo_users", as, "user"},
    TEmail = {table, "email"},
    TAddress = {table, "boo_addresses", as, "address"},
    Select = {select, TUser,
              [{joins, [{inner, TEmail}, {right, TAddress}]},
               {fields, ["user.id", "user.name", "address.state"]}
              ]},

    ?assertEqual(<<"SELECT user.id, user.name, address.state ",
                   "FROM foo_users AS user ",
                   "INNER JOIN email ON email.id = user.email_id ",
                   "RIGHT JOIN boo_addresses AS address ON address.id = user.boo_addresses_id">>,
                 erma:build(Select)),
    ok.


relations5_test() ->
    TUser = {table, "user"},
    TEmail = {table, "email"},
    TAddress1 = {table, "address", as, "a1"},
    TAddress2 = {table, "address", as, "a2"},
    TAccount = {table, "account"},

    Select = {select, TUser,
              [{fields, ["email.email", "address1.state", "address2.state", "account.name"]},
               {joins, [{left, TEmail},
                        {right, TAddress1},
                        {inner, TAddress2},
                        {full, TAccount}]}
              ]},

    ?assertEqual(<<"SELECT email.email, address1.state, address2.state, account.name ",
                   "FROM user ",
                   "LEFT JOIN email ON email.id = user.email_id ",
                   "RIGHT JOIN address AS a1 ON a1.id = user.address_id ",
                   "INNER JOIN address AS a2 ON a2.id = user.address_id ",
                   "FULL JOIN account ON account.id = user.account_id">>,
                 erma:build(Select)),
    ok.


relations6_test() ->
    TUser = {table, "user", as, "u"},
    TEmail = {table, "email"},
    TAddress = {table, "address", as, "a"},
    TAccount = {table, "account"},

    Select1 = {select, TUser,
               [{fields, ["u.id", "email.email", "a.state", "account.name"]},
                {joins, [{left, TEmail}]}
               ]},
    Select2 = erma:append(Select1, [{joins, [{left, TAddress}, {right, TAccount}]}]),

    ?assertEqual(<<"SELECT u.id, email.email, a.state, account.name ",
                   "FROM user AS u ",
                   "LEFT JOIN email ON email.id = u.email_id ",
                   "LEFT JOIN address AS a ON a.id = u.address_id ",
                   "RIGHT JOIN account ON account.id = u.account_id">>,
                 erma:build(Select2)),
    ok.
