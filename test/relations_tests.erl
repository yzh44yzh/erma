-module(relations_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


relations1_test() ->
    Select = {select, ["email.email", "address.state", "account.name"], "user",
              [{left_join, "email"},
               {left_join, <<"address">>},
               {left_join, account}]},
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
              [{left_join, TAddress}]},
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
              [{inner_join, TEmail}, {right_join, TAddress}]},
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
              [{left_join, TEmail},
               {right_join, TAddress1},
               {inner_join, TAddress2},
               {full_join, TAccount}]},
    ?assertEqual(<<"SELECT email.email, address1.`state`, address2.`state`, account.`name` ",
                   "FROM `user` ",
                   "LEFT JOIN email ON email.id = `user`.email_id ",
                   "RIGHT JOIN address AS a1 ON a1.id = `user`.address_id ",
                   "INNER JOIN address AS a2 ON a2.id = `user`.address_id ",
                   "FULL JOIN account ON account.id = `user`.account_id">>,
                 erma:build(Select)),
    ok.



%% TODO append doen't work like it should
%% relations6_test() ->
%%     TUser = {"user", as, "u"},
%%     TEmail = "email",
%%     TAddress = {"address", as, "a"},
%%     TAccount = "account",
%%     Select1 = {select, ["u.id", "email.email", "a.state", "account.name"], TUser,
%%                [{left_join, TEmail}]},
%%     Select2 = erma:append(Select1, [{left_join, TAddress}, {right_join, TAccount}]),
%%     ?assertEqual(<<"SELECT u.id, email.email, `a`.`state`, account.`name` ",
%%                    "FROM `user` AS u ",
%%                    "LEFT JOIN email ON email.id = u.email_id ",
%%                    "LEFT JOIN address AS `a` ON `a`.id = u.address_id ",
%%                    "RIGHT JOIN account ON account.id = u.account_id">>,
%%                  erma:build(Select2)),
%%     ok.


relations7_test() ->
    TUser = {"user", as, "u"},
    TEmail = {"email", as, "e"},
    TAddress = "address",
    TAccount = "account",
    Select = {select, ["email.email", "address.state", "account.name"], TUser,
              [{left_join, TEmail, [{pk, "eid"}]},
               {left_join, TAddress, [{fk, "addr_id"}]},
               {left_join, TAccount, [{pk, "aid"}, {fk, "acc_id"}]}]},
    ?assertEqual(<<"SELECT email.email, address.`state`, account.`name` ",
                   "FROM `user` AS u ",
                   "LEFT JOIN email AS e ON e.eid = u.email_id ",
                   "LEFT JOIN address ON address.id = u.addr_id ",
                   "LEFT JOIN account ON account.aid = u.acc_id">>,
                 erma:build(Select)),
    ok.


relations8_test() ->
    Select = {select, ["user.id", "email.value", "address.city"], user,
              [{left_join, "address"},
               {inner_join, {"email", "address"}}]},
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
              [{left_join, TAddress},
               {inner_join, {TEmail, TAddress}}]},
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
              [{left_join, TAddress},
               {inner_join, {TEmail, TAddress}, [{pk, "eid"}, {fk, "em_id"}]},
               {inner_join, {TCity, TAddress}}]},
    ?assertEqual(<<"SELECT u.id, e.`value`, city.`value` ",
                   "FROM `user` AS u ",
                   "LEFT JOIN address AS `a` ON `a`.id = u.address_id ",
                   "INNER JOIN email AS e ON e.eid = `a`.em_id ",
                   "INNER JOIN city ON city.id = `a`.city_id">>,
                 erma:build(Select)),
    ok.


relations11_test() ->
    Select = {select, ["bb.*"], {"blah", as, "bb"},
              [{left_join, {"blah", as, "bb2"}, [{pk, "cool"}, {fk, "cool2"}]}]},
    ?assertEqual(<<"SELECT bb.* "
                   "FROM blah AS bb "
                   "LEFT JOIN blah AS bb2 "
                   "ON bb2.cool = bb.cool2">>,
                 erma:build(Select)),
    ok.


relations12_test() ->
    Select = {select, ["address.*", "state.*"], "address",
              [{left_join, "state"},
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
              [{left_join, "address", [{pk, "user_id"}, {fk, "id"}]},
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
              [{left_join, {"users", as, "u2"}},
               {left_join, {"users", as, "u3"}}]},
    ?assertEqual(<<"SELECT * FROM users AS u ",
                   "LEFT JOIN users AS u2 ON u2.id = u.users_id ",
                   "LEFT JOIN users AS u3 ON u3.id = u.users_id">>,
                 erma:build(Select)),

    Select2 = {select, ["u.*", "a.*", "s.*"], {"users", as, "u"},
               [{left_join, {"address", as, "a"}, [{pk, "users_id"}, {fk, "id"}]},
                {left_join, {{"state", as, "s"}, {"address", as, "a"}}},
                {where, [{'and', [{"s.state", "nc"}, {"a.id", gt, 5}]}]}]},
    ?assertEqual(<<"SELECT u.*, `a`.*, s.* FROM users AS u ",
                   "LEFT JOIN address AS `a` ON `a`.users_id = u.id ",
                   "LEFT JOIN `state` AS s ON s.id = `a`.state_id ",
                   "WHERE (s.`state` = 'nc' AND `a`.id > 5)">>,
                 erma:build(Select2)),

    ok.
