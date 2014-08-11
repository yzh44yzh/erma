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


relations7_test() ->
    TUser = {table, "user", as, "u"},
    TEmail = {table, "email", as, "e"},
    TAddress = {table, "address"},
    TAccount = {table, "account"},

    Select = {select, TUser,
              [{fields, ["email.email", "address.state", "account.name"]},
               {joins, [{left, TEmail, [{pk, "eid"}]},
                        {left, TAddress, [{fk, "addr_id"}]},
                        {left, TAccount, [{pk, "aid"}, {fk, "acc_id"}]}]}
              ]},

    ?assertEqual(<<"SELECT email.email, address.state, account.name ",
                   "FROM user AS u ",
                   "LEFT JOIN email AS e ON e.eid = u.email_id ",
                   "LEFT JOIN address ON address.id = u.addr_id ",
                   "LEFT JOIN account ON account.aid = u.acc_id">>,
                 erma:build(Select)),
    ok.


relations8_test() ->
    TUser = {table, "user"},
    TEmail = {table, "email"},
    TAddress = {table, "address"},

    Select = {select, TUser,
              [{fields, ["user.id, email.value, address.city"]},
               {joins, [{left, TAddress},
                        {inner, TEmail, TAddress}]}
              ]},

    ?assertEqual(<<"SELECT user.id, email.value, address.city ",
                   "FROM user ",
                   "LEFT JOIN address ON address.id = user.address_id ",
                   "INNER JOIN email ON email.id = address.email_id">>,
                 erma:build(Select)),
    ok.


relations9_test() ->
    TUser = {table, "user", as, "u"},
    TEmail = {table, "email", as, "e"},
    TAddress = {table, "address", as, "a"},

    Select = {select, TUser,
              [{fields, ["u.id, e.value, a.city"]},
               {joins, [{left, TAddress},
                        {inner, TEmail, TAddress}]}
              ]},
    ?assertEqual(<<"SELECT u.id, e.value, a.city ",
                   "FROM user AS u ",
                   "LEFT JOIN address AS a ON a.id = u.address_id ",
                   "INNER JOIN email AS e ON e.id = a.email_id">>,
                 erma:build(Select)),
    ok.


relations10_test() ->
    TUser = {table, "user", as, "u"},
    TEmail = {table, "email", as, "e"},
    TAddress = {table, "address", as, "a"},
    TCity = {table, "city"},

    Select = {select, TUser,
              [{fields, ["u.id, e.value, city.value"]},
               {joins, [{left, TAddress},
                        {inner, TEmail, TAddress, [{pk, "eid"}, {fk, "em_id"}]},
                        {inner, TCity, TAddress}]}
              ]},
    ?assertEqual(<<"SELECT u.id, e.value, city.value ",
                   "FROM user AS u ",
                   "LEFT JOIN address AS a ON a.id = u.address_id ",
                   "INNER JOIN email AS e ON e.eid = a.em_id ",
                   "INNER JOIN city ON city.id = a.city_id">>,
                 erma:build(Select)),
    ok.


relations11_test() ->
    Select = {select, {table, "blah", as, "bb"},
              [{fields, ["bb.*"]},
               {joins, [{left, {table, "blah", as, "bb2"}, [{pk, "cool"}, {fk, "cool2"}]}]}
              ]},
    ?assertEqual(<<"SELECT bb.* "
                   "FROM blah AS bb "
                   "LEFT JOIN blah AS bb2 "
                   "ON bb2.cool = bb.cool2">>,
                 erma:build(Select)),
    ok.


relations12_test() ->
    Select = {select, {table, "address"},
              [{fields, ["address.*", "state.*"]},
               {joins, [{left, {table, "state"}}]},
               {where, [{"state.status", "?"}]},
               {order, ["address.id"]}]},
    ?assertEqual(<<"SELECT address.*, state.* ",
                   "FROM address ",
                   "LEFT JOIN state ON state.id = address.state_id ",
                   "WHERE state.status = ? ",
                   "ORDER BY address.id ASC">>,
                 erma:build(Select)),
    ok.


relations13_test() ->
    Select = {select, {table, "user"},
              [{fields, ["user.*", "address.*"]},
               {joins, [{left, {table, "address"}, [{pk, "user_id"}, {fk, "id"}]}]},
               {where, [{"address.status", "?"}]},
               {order, ["user.id"]}]},
    ?assertEqual(<<"SELECT user.*, address.* ",
                   "FROM user ",
                   "LEFT JOIN address ON address.user_id = user.id ",
                   "WHERE address.status = ? ",
                   "ORDER BY user.id ASC">>,
                 erma:build(Select)),
    ok.


relations14_test() ->
    Select = {select, {table, "users", as, "u"},
              [{joins, [{left, {table, "users", as, "u2"}},
                        {left, {table, "users", as, "u3"}}]}
              ]},
    ?assertEqual(<<"SELECT * FROM users AS u ",
                   "LEFT JOIN users AS u2 ON u2.id = u.users_id ",
                   "LEFT JOIN users AS u3 ON u3.id = u.users_id">>,
                 erma:build(Select)),

    Select2 = {select, {table, "users", as, "u"},
               [{joins, [{left, {table, "address", as, "a"}, [{pk, "users_id"}, {fk, "id"}]},
                         {left, {table, "state", as, "s"}, {table, "address", as, "a"}}]},
                {where, [{'and', [{"s.state", "nc"}, {"a.id", gt, 5}]}]},
                {fields, ["u.*", "a.*", "s.*"]}]},
    ?assertEqual(<<"SELECT u.*, a.*, s.* FROM users AS u ",
                   "LEFT JOIN address AS a ON a.users_id = u.id ",
                   "LEFT JOIN state AS s ON s.id = a.state_id ",
                   "WHERE (s.state = 'nc' AND a.id > 5)">>,
                 erma:build(Select2)),

    ok.
