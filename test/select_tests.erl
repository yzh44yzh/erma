-module(select_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

select_test_() ->
    test_utils:generate(
      [{
         %%
         {select, ["id", "username"], "users",
          [{where, [{"username", "chris"}]},
           {order, ["created"]},
           {offset, 3, limit, 5}]},
         %%
         <<"SELECT id, username FROM users WHERE username = 'chris' ",
           "ORDER BY created ASC OFFSET 3 LIMIT 5">>
       },
       {
         %%
         {select, ["email.*"], "email",
          [{where, [{'and', [{"email.email", like, "%@gmail.com"},
                             {"email.user_id", 1}]}]}]},
         %%
         <<"SELECT email.* FROM email ",
           "WHERE (email.email LIKE '%@gmail.com' AND email.user_id = 1)">>
       },
       {
         %%
         {select, [], "users"},
         %%
         <<"SELECT * FROM users">>
       },
       {
         %%
         {select, ["u.*"], {"users", as, "u"}},
         %%
         <<"SELECT u.* FROM users AS u">>
       },
       {
         %%
         {select, ["users.id", "users.username"], "users"},
         %%
         <<"SELECT users.id, users.username FROM users">>
       },
       {
         %%
         {select, [], "users",
          [{where, [{'and', [{"email", "hey@hey.com"},
                             {"username", "chris"}]}]}]},
         %%
         <<"SELECT * FROM users WHERE (email = 'hey@hey.com' AND username = 'chris')">>
       },
       {
         %%
         {select, [], <<"users">>,
          [{where, [{"username", "chris"}]},
           {order, ["created"]}]},
         %%
         <<"SELECT * FROM users WHERE username = 'chris' ORDER BY created ASC">>
       },
       {
         %%
         {select, [], users,
          [{where, [{"active", true}]},
           {order, ["created"]},
           {offset, 3, limit, 5}]},
         %%
         <<"SELECT * FROM users WHERE active = true ORDER BY created ASC OFFSET 3 LIMIT 5">>
       },
       {
         %%
         {select_distinct, [id], "users"},
         %%
         <<"SELECT DISTINCT id FROM users">>
       },
       {
         %%
         {select_distinct, ["id", name, <<"age">>], "user"},
         %%
         <<"SELECT DISTINCT id, \"name\", age FROM \"user\"">>
       },
       {
         %%
         {select, ["id", "username"], "user", [{order, ["created"]}]},
         %%
         <<"SELECT id, username FROM \"user\" ORDER BY created ASC">>
       },
       {
         %%
         {select, ["id", "username"], "user",
          [{order, ["created", {"username", desc}]}]},
         %%
         <<"SELECT id, username FROM \"user\" ORDER BY created ASC, username DESC">>
       },
       {
         %%
         {select, ["id", "username"], "user"},
         %%
         <<"SELECT id, username FROM \"user\"">>
       },
       {
         %%
         {select, ["id", "username"], "user", [{limit, 20}]},
         %%
         <<"SELECT id, username FROM \"user\" LIMIT 20">>
       },
       {
         %%
         {select, ["id", "username"], "user", [{offset, 0, limit, 20}]},
         %%
         <<"SELECT id, username FROM \"user\" OFFSET 0 LIMIT 20">>
       },
       {
         %%
         {select, ["id", "username"], "user", [{offset, 100, limit, 20}]},
         %%
         <<"SELECT id, username FROM \"user\" OFFSET 100 LIMIT 20">>
       },
       {
         %%
         {select, [], <<"user">>},
         %%
         <<"SELECT * FROM \"user\"">>
       },
       {
         %%
         {select, [], <<"user">>, [{where, [{<<"email">>, <<"some@where.com">>}]}]},
         %%
         <<"SELECT * FROM \"user\" WHERE email = 'some@where.com'">>
       },
       {
         %%
         {select, [<<"first_name">>, "last_name", "address.state"], user,
          [{where, [{"email", <<"some@where.com">>}]}]},
         %%
         <<"SELECT first_name, last_name, address.\"state\" ",
           "FROM \"user\" ",
           "WHERE email = 'some@where.com'">>
       },
       {
         %%
         {select, ["id", "username"], "user",
          [{order, ["id", "created", {"last_login", asc}, {"username", desc}]}]},
         %%
         <<"SELECT id, username FROM \"user\" ORDER BY id ASC, created ASC, last_login ASC, username DESC">>
       },
       {
         %%
         {select, [{"id", as, "user_id"}, "username"], "user",
          [{order, ["user_id", "created", {"last_login", asc}, {"username", desc}]}]},
         %%
         <<"SELECT id AS user_id, username FROM \"user\" ORDER BY user_id ASC, created ASC, last_login ASC, username DESC">>
       },
       {
         %%
         {select, [{<<"first_name">>, as, "fname"}, {"last_name", as, "lname"}, "address.state"], user,
          [{where, [{"fname", '<>', <<"Bob">>}]}]},
         %%
         <<"SELECT first_name AS fname, last_name AS lname, address.\"state\" ",
           "FROM \"user\" ",
           "WHERE fname <> 'Bob'">>
       },
       {
         %%
         {select, [{raw, "level"}, {raw, "address.state"}], user,
          [{where, [{"fname", '<>', <<"Bob">>}]}]},
         %%
         <<"SELECT level, address.state ",
           "FROM \"user\" ",
           "WHERE fname <> 'Bob'">>
       },
       {
         %%
         {select, [{raw, "SUM(id)"}], user,
          [{where, [{"fname", '<>', <<"Bob">>}]}]},
         %%
         <<"SELECT SUM(id) ",
           "FROM \"user\" ",
           "WHERE fname <> 'Bob'">>
       },
       {
         %%
         {select, [{sum, "id"}], user},
         %%
         <<"SELECT SUM(id) FROM \"user\"">>
       },
       {
         %%
         {select, [{sum, "id", as, "total"}], user},
         %%
         <<"SELECT SUM(id) AS total FROM \"user\"">>
       },
       {
         %%
         {select, [{function, now, []}], user},
         %%
         <<"SELECT now() FROM \"user\"">>
       },
       {
         %%
         {select, [{function, to_char, ["created_at", <<"HH24:MI:SS">>]}], user},
         %%
         <<"SELECT to_char(created_at, 'HH24:MI:SS') FROM \"user\"">>
       },
       {
         %%
         {select, [{function, age, [{function, now, []}, "created_at"]}], user},
         %%
         <<"SELECT age(now(), created_at) FROM \"user\"">>
       },
       {
         %%
         {select, ["id"], user,
           [{where, [{"created_at", '<', {function, now, []}}]}]},
         %%
         <<"SELECT id ",
           "FROM \"user\" ",
           "WHERE created_at < now()">>
       },
       {
         %%
         {select, ["id"], user,
           [{where, [{{function, calculate_rating, ["created_at", "score"]}, '>', 100}]}]},
         %%
         <<"SELECT id ",
           "FROM \"user\" ",
           "WHERE calculate_rating(created_at, score) > 100">>
       },
       {
         %%
         {select, ["id"], user,
           [{where, [{{function, calculate_rating, [{function, now, []}, "score"]}, '>', 100}]}]},
         %%
         <<"SELECT id ",
           "FROM \"user\" ",
           "WHERE calculate_rating(now(), score) > 100">>
       },
       {
         %%
         {select, ["id"], user,
           [{where, [{{function, calculate_rating, ["user.created_at", "user.score"]}, '>', 100}]}]},
         %%
         <<"SELECT id ",
           "FROM \"user\" ",
           "WHERE calculate_rating(user.created_at, user.score) > 100">>
       }
      ]).
