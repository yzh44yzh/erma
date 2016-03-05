-module(relations_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

relations_test_() ->
    test_utils:generate(
      [{
         %%
         {select, ["email.email", "address.state", "account.name"], "user",
          [{joins, [{left, "email"},
                    {left, <<"address">>},
                    {left, account}]}]},
         %%
         <<"SELECT email.email, address.\"state\", account.\"name\" ",
           "FROM \"user\" ",
           "LEFT JOIN email ON email.id = \"user\".email_id ",
           "LEFT JOIN address ON address.id = \"user\".address_id ",
           "LEFT JOIN account ON account.id = \"user\".account_id">>
       },
       {
         %%
         {select, ["user.id", "user.name"], {"foo_users", as, "user"}},
         %%
         <<"SELECT \"user\".id, \"user\".\"name\" FROM foo_users AS \"user\"">>
       },
       {
         %%
         {select, ["user.id", "user.name", "address.state"], {"foo_users", as, "user"},
          [{joins, [{left, {"boo_addresses", as, "address"}}]}]},
         %%
         <<"SELECT \"user\".id, \"user\".\"name\", address.\"state\" ",
           "FROM foo_users AS \"user\" ",
           "LEFT JOIN boo_addresses AS address ON address.id = \"user\".boo_addresses_id">>
       },
       {
         %%
         {select, ["user.id", "user.name", "address.state"], {"foo_users", as, "user"},
          [{joins, [{inner, email}, {right, {"boo_addresses", as, "address"}}]}]},
         %%
         <<"SELECT \"user\".id, \"user\".\"name\", address.\"state\" ",
           "FROM foo_users AS \"user\" ",
           "INNER JOIN email ON email.id = \"user\".email_id ",
           "RIGHT JOIN boo_addresses AS address ON address.id = \"user\".boo_addresses_id">>
       },
       {
         %%
         {select, ["email.email", "address1.state", "address2.state", "account.name"], "user",
          [{joins, [{left, <<"email">>},
                    {right, {"address", as, "a1"}},
                    {inner, {"address", as, "a2"}},
                    {full, "account"}]}]},
         %%
         <<"SELECT email.email, address1.\"state\", address2.\"state\", account.\"name\" ",
           "FROM \"user\" ",
           "LEFT JOIN email ON email.id = \"user\".email_id ",
           "RIGHT JOIN address AS a1 ON a1.id = \"user\".address_id ",
           "INNER JOIN address AS a2 ON a2.id = \"user\".address_id ",
           "FULL JOIN account ON account.id = \"user\".account_id">>
       },
       {
         %%
         {select, ["email.email", "address.state", "account.name"], {"user", as, "u"},
          [{joins, [{left, {"email", as, "e"}, [{pk, "eid"}]},
                    {left, "address", [{fk, "addr_id"}]},
                    {left, "account", [{pk, "aid"}, {fk, "acc_id"}]}]}]},
         %%
         <<"SELECT email.email, address.\"state\", account.\"name\" ",
           "FROM \"user\" AS u ",
           "LEFT JOIN email AS e ON e.eid = u.email_id ",
           "LEFT JOIN address ON address.id = u.addr_id ",
           "LEFT JOIN account ON account.aid = u.acc_id">>
       },
       {
         %%
         {select, ["user.id", "email.value", "address.city"], user,
          [{joins, [{left, "address"},
                    {inner, {"email", "address"}}]}]},
         %%
         <<"SELECT \"user\".id, email.\"value\", address.city ",
           "FROM \"user\" ",
           "LEFT JOIN address ON address.id = \"user\".address_id ",
           "INNER JOIN email ON email.id = address.email_id">>
       },
       {
         %%
         {select, ["u.id", "e.value", "a.city"], {"user", as, "u"},
          [{joins, [{left, {"address", as, "a"}},
                    {inner, {{"email", as, "e"}, {"address", as, "a"}}}]}]},
         %%
         <<"SELECT u.id, e.\"value\", \"a\".city ",
           "FROM \"user\" AS u ",
           "LEFT JOIN address AS \"a\" ON \"a\".id = u.address_id ",
           "INNER JOIN email AS e ON e.id = \"a\".email_id">>
       },
       {
         %%
         {select, ["u.id", "e.value", "city.value"], {"user", as, "u"},
          [{joins, [{left, {"address", as, "a"}},
                    {inner, {{"email", as, "e"}, {"address", as, "a"}}, [{pk, "eid"}, {fk, "em_id"}]},
                    {inner, {"city", {"address", as, "a"}}}]}]},
         %%
         <<"SELECT u.id, e.\"value\", city.\"value\" ",
           "FROM \"user\" AS u ",
           "LEFT JOIN address AS \"a\" ON \"a\".id = u.address_id ",
           "INNER JOIN email AS e ON e.eid = \"a\".em_id ",
           "INNER JOIN city ON city.id = \"a\".city_id">>
       },
       {
         %%
         {select, ["bb.*"], {"blah", as, "bb"},
          [{joins, [{left, {"blah", as, "bb2"}, [{pk, "cool"}, {fk, "cool2"}]}]}]},
         %%
         <<"SELECT bb.* "
           "FROM blah AS bb "
           "LEFT JOIN blah AS bb2 "
           "ON bb2.cool = bb.cool2">>
       },
       {
         %%
         {select, ["address.*", "state.*"], "address",
          [{joins, [{left, "state"}]},
           {where, [{"state.status", "?"}]},
           {order, ["address.id"]}]},
         %%
         <<"SELECT address.*, \"state\".* ",
           "FROM address ",
           "LEFT JOIN \"state\" ON \"state\".id = address.state_id ",
           "WHERE \"state\".\"status\" = ? ",
           "ORDER BY address.id ASC">>
       },
       {
         %%
         {select, ["user.*", "address.*"], "user",
          [{joins, [{left, "address", [{pk, "user_id"}, {fk, "id"}]}]},
           {where, [{"address.status", "?"}]},
           {order, ["user.id"]}]},
         %%
         <<"SELECT \"user\".*, address.* ",
           "FROM \"user\" ",
           "LEFT JOIN address ON address.user_id = \"user\".id ",
           "WHERE address.\"status\" = ? ",
           "ORDER BY \"user\".id ASC">>
       },
       {
         %%
         {select, [], {"users", as, "u"},
          [{joins, [{left, {"users", as, "u2"}},
                    {left, {"users", as, "u3"}}]}]},
         %%
         <<"SELECT * FROM users AS u ",
           "LEFT JOIN users AS u2 ON u2.id = u.users_id ",
           "LEFT JOIN users AS u3 ON u3.id = u.users_id">>
       },
       {
         %%
         {select, ["u.*", "a.*", "s.*"], {"users", as, "u"},
          [{joins, [{left, {"address", as, "a"}, [{pk, "users_id"}, {fk, "id"}]},
                    {left, {{"state", as, "s"}, {"address", as, "a"}}}]},
           {where, [{'and', [{"s.state", "nc"}, {"a.id", gt, 5}]}]}]},
         %%
         <<"SELECT u.*, \"a\".*, s.* FROM users AS u ",
           "LEFT JOIN address AS \"a\" ON \"a\".users_id = u.id ",
           "LEFT JOIN \"state\" AS s ON s.id = \"a\".state_id ",
           "WHERE (s.\"state\" = 'nc' AND \"a\".id > 5)">>
       },
       {
         %%
         {select, ["first_name", "last_name", "address.state"], "user",
          [{joins, [{left, "address"}]}]},
         %%
         <<"SELECT first_name, last_name, address.\"state\" ",
           "FROM \"user\" ",
           "LEFT JOIN address ON address.id = \"user\".address_id">>
       },
       {
         %%
         {select, ["first_name", "last_name", "address.state"], "user",
          [{joins, [{left, "address"}]},
           {where, [{"email", "some@where.com"}]}]},
         %%
         <<"SELECT first_name, last_name, address.\"state\" ",
           "FROM \"user\" ",
           "LEFT JOIN address ON address.id = \"user\".address_id ",
           "WHERE email = 'some@where.com'">>
       },
       {
         %%
         {select, [], {"foo_users", as, "user"},
          [{joins, [{left, {"addresses", as, "address"}, [{pk, "userID"}, {fk, "userID"}]}]},
           {where, [{"last_login", lt, {date, {2014, 1, 20}}}]}]},
         %%
         <<"SELECT * FROM foo_users AS \"user\" ",
           "LEFT JOIN addresses AS address ON address.userID = \"user\".userID ",
           "WHERE last_login < '2014-01-20'">>
       }
      ]).
