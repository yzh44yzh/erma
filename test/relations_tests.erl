-module(relations_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


relations1_test() ->
    Select = {select, ["email.email", "address.state", "account.name"], "user",
              [{joins, [{left, "email"},
                        {left, <<"address">>},
                        {left, account}]}]},
    ?assertEqual(<<"SELECT email.email, address.`state`, account.`name` ",
                   "FROM `user` ",
                   "LEFT JOIN email ON email.id = `user`.email_id ",
                   "LEFT JOIN address ON address.id = `user`.address_id ",
                   "LEFT JOIN account ON account.id = `user`.account_id">>,
                 erma:build(Select)),
    ok.


relations2_test() ->
    Select = {select, ["user.id", "user.name"], {"foo_users", as, "user"}},
    ?assertEqual(<<"SELECT `user`.id, `user`.`name` FROM foo_users AS `user`">>,
                 erma:build(Select)),
    ok.


relations3_test() ->
    TAddress = {"boo_addresses", as, "address"},
    TUser = {"foo_users", as, "user"},
    Select = {select, ["user.id", "user.name", "address.state"], TUser,
              [{joins, [{left, TAddress}]}]},
    ?assertEqual(<<"SELECT `user`.id, `user`.`name`, address.`state` ",
                   "FROM foo_users AS `user` ",
                   "LEFT JOIN boo_addresses AS address ON address.id = `user`.boo_addresses_id">>,
                 erma:build(Select)),
    ok.


relations4_test() ->
    TUser = {"foo_users", as, "user"},
    TEmail = email,
    TAddress = {"boo_addresses", as, "address"},
    Select = {select, ["user.id", "user.name", "address.state"], TUser,
              [{joins, [{inner, TEmail}, {right, TAddress}]}]},
    ?assertEqual(<<"SELECT `user`.id, `user`.`name`, address.`state` ",
                   "FROM foo_users AS `user` ",
                   "INNER JOIN email ON email.id = `user`.email_id ",
                   "RIGHT JOIN boo_addresses AS address ON address.id = `user`.boo_addresses_id">>,
                 erma:build(Select)),
    ok.


relations5_test() ->
    TUser = "user",
    TEmail = <<"email">>,
    TAddress1 = {"address", as, "a1"},
    TAddress2 = {"address", as, "a2"},
    TAccount = "account",

    Select = {select, ["email.email", "address1.state", "address2.state", "account.name"], TUser,
              [{joins, [{left, TEmail},
                        {right, TAddress1},
                        {inner, TAddress2},
                        {full, TAccount}]}]},
    ?assertEqual(<<"SELECT email.email, address1.`state`, address2.`state`, account.`name` ",
                   "FROM `user` ",
                   "LEFT JOIN email ON email.id = `user`.email_id ",
                   "RIGHT JOIN address AS a1 ON a1.id = `user`.address_id ",
                   "INNER JOIN address AS a2 ON a2.id = `user`.address_id ",
                   "FULL JOIN account ON account.id = `user`.account_id">>,
                 erma:build(Select)),
    ok.



relations7_test() ->
    TUser = {"user", as, "u"},
    TEmail = {"email", as, "e"},
    TAddress = "address",
    TAccount = "account",
    Select = {select, ["email.email", "address.state", "account.name"], TUser,
              [{joins, [{left, TEmail, [{pk, "eid"}]},
                        {left, TAddress, [{fk, "addr_id"}]},
                        {left, TAccount, [{pk, "aid"}, {fk, "acc_id"}]}]}]},
    ?assertEqual(<<"SELECT email.email, address.`state`, account.`name` ",
                   "FROM `user` AS u ",
                   "LEFT JOIN email AS e ON e.eid = u.email_id ",
                   "LEFT JOIN address ON address.id = u.addr_id ",
                   "LEFT JOIN account ON account.aid = u.acc_id">>,
                 erma:build(Select)),
    ok.


relations8_test() ->
    Select = {select, ["user.id", "email.value", "address.city"], user,
              [{joins, [{left, "address"},
                        {inner, {"email", "address"}}]}]},
    ?assertEqual(<<"SELECT `user`.id, email.`value`, address.city ",
                   "FROM `user` ",
                   "LEFT JOIN address ON address.id = `user`.address_id ",
                   "INNER JOIN email ON email.id = address.email_id">>,
                 erma:build(Select)),
    ok.


relations9_test() ->
    TUser = {"user", as, "u"},
    TEmail = {"email", as, "e"},
    TAddress = {"address", as, "a"},
    Select = {select, ["u.id", "e.value", "a.city"], TUser,
              [{joins, [{left, TAddress},
                        {inner, {TEmail, TAddress}}]}]},
    ?assertEqual(<<"SELECT u.id, e.`value`, `a`.city ",
                   "FROM `user` AS u ",
                   "LEFT JOIN address AS `a` ON `a`.id = u.address_id ",
                   "INNER JOIN email AS e ON e.id = `a`.email_id">>,
                 erma:build(Select)),
    ok.


relations10_test() ->
    TUser = {"user", as, "u"},
    TEmail = {"email", as, "e"},
    TAddress = {"address", as, "a"},
    TCity = "city",
    Select = {select, ["u.id", "e.value", "city.value"], TUser,
              [{joins, [{left, TAddress},
                        {inner, {TEmail, TAddress}, [{pk, "eid"}, {fk, "em_id"}]},
                        {inner, {TCity, TAddress}}]}]},
    ?assertEqual(<<"SELECT u.id, e.`value`, city.`value` ",
                   "FROM `user` AS u ",
                   "LEFT JOIN address AS `a` ON `a`.id = u.address_id ",
                   "INNER JOIN email AS e ON e.eid = `a`.em_id ",
                   "INNER JOIN city ON city.id = `a`.city_id">>,
                 erma:build(Select)),
    ok.


relations11_test() ->
    Select = {select, ["bb.*"], {"blah", as, "bb"},
              [{joins, [{left, {"blah", as, "bb2"}, [{pk, "cool"}, {fk, "cool2"}]}]}]},
    ?assertEqual(<<"SELECT bb.* "
                   "FROM blah AS bb "
                   "LEFT JOIN blah AS bb2 "
                   "ON bb2.cool = bb.cool2">>,
                 erma:build(Select)),
    ok.


relations12_test() ->
    Select = {select, ["address.*", "state.*"], "address",
              [{joins, [{left, "state"}]},
               {where, [{"state.status", "?"}]},
               {order, ["address.id"]}]},
    ?assertEqual(<<"SELECT address.*, `state`.* ",
                   "FROM address ",
                   "LEFT JOIN `state` ON `state`.id = address.state_id ",
                   "WHERE `state`.`status` = ? ",
                   "ORDER BY address.id ASC">>,
                 erma:build(Select)),
    ok.


relations13_test() ->
    Select = {select, ["user.*", "address.*"], "user",
              [{joins, [{left, "address", [{pk, "user_id"}, {fk, "id"}]}]},
               {where, [{"address.status", "?"}]},
               {order, ["user.id"]}]},
    ?assertEqual(<<"SELECT `user`.*, address.* ",
                   "FROM `user` ",
                   "LEFT JOIN address ON address.user_id = `user`.id ",
                   "WHERE address.`status` = ? ",
                   "ORDER BY `user`.id ASC">>,
                 erma:build(Select)),
    ok.


relations14_test() ->
    Select = {select, [], {"users", as, "u"},
              [{joins, [{left, {"users", as, "u2"}},
                        {left, {"users", as, "u3"}}]}]},
    ?assertEqual(<<"SELECT * FROM users AS u ",
                   "LEFT JOIN users AS u2 ON u2.id = u.users_id ",
                   "LEFT JOIN users AS u3 ON u3.id = u.users_id">>,
                 erma:build(Select)),

    Select2 = {select, ["u.*", "a.*", "s.*"], {"users", as, "u"},
               [{joins, [{left, {"address", as, "a"}, [{pk, "users_id"}, {fk, "id"}]},
                         {left, {{"state", as, "s"}, {"address", as, "a"}}}]},
                {where, [{'and', [{"s.state", "nc"}, {"a.id", gt, 5}]}]}]},
    ?assertEqual(<<"SELECT u.*, `a`.*, s.* FROM users AS u ",
                   "LEFT JOIN address AS `a` ON `a`.users_id = u.id ",
                   "LEFT JOIN `state` AS s ON s.id = `a`.state_id ",
                   "WHERE (s.`state` = 'nc' AND `a`.id > 5)">>,
                 erma:build(Select2)),

    ok.


relations15_test() ->
    Q4 = {select, ["first_name", "last_name", "address.state"], "user",
          [{joins, [{left, "address"}]}]},
    S4 = <<"SELECT first_name, last_name, address.`state` ",
           "FROM `user` ",
           "LEFT JOIN address ON address.id = `user`.address_id">>,
    ?assertEqual(S4, erma:build(Q4)),

    Q5 = {select, ["first_name", "last_name", "address.state"], "user",
          [{joins, [{left, "address"}]},
           {where, [{"email", "some@where.com"}]}
          ]},
    S5 = <<"SELECT first_name, last_name, address.`state` ",
           "FROM `user` ",
           "LEFT JOIN address ON address.id = `user`.address_id ",
           "WHERE email = 'some@where.com'">>,
    ?assertEqual(S5, erma:build(Q5)),
    ok.


relations16_test() ->
    TAddress = {"addresses", as, "address"},
    TUser = {"foo_users", as, "user"},
    Q = {select, [], TUser,
         [{joins, [{left, TAddress, [{pk, "userID"}, {fk, "userID"}]}]},
          {where, [{"last_login", lt, {date, {2014, 1, 20}}}]}]},
    ?assertEqual(<<"SELECT * FROM foo_users AS `user` ",
                   "LEFT JOIN addresses AS address ON address.userID = `user`.userID ",
                   "WHERE last_login < '2014-01-20'">>,
                 erma:build(Q)),
    ok.
