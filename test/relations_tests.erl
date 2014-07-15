-module(relations_tests).
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
