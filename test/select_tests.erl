-module(select_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


select1_test() ->
    ?assertEqual(<<"SELECT id, username FROM users WHERE username = 'chris' ",
                   "ORDER BY created ASC OFFSET 3, LIMIT 5">>,
                 erma:build({select, {table, "users"},
                             [{fields, ["id", "username"]},
                              {where, [{"username", "chris"}]},
                              {order, ["created"]},
                              {limit, 5},
                              {offset, 3}]})),

    ?assertEqual(<<"SELECT email.* FROM email ",
                   "WHERE (email.email LIKE '%@gmail.com' AND email.user_id = 1)">>,
                 erma:build({select, {table, "email"},
                             [{fields, ["email.*"]},
                              {where, [{'and', [{"email.email", like, "%@gmail.com"},
                                                {"email.user_id", 1}]}]}
                             ]})),
    ok.


select2_test() ->
    ?assertEqual(<<"SELECT * FROM users">>,
                 erma:build({select, {table, "users"}, []})),
    ?assertEqual(<<"SELECT u.* FROM users AS u">>,
                 erma:build({select, {table, "users", as, "u"}, [{fields, ["u.*"]}]})),
    ?assertEqual(<<"SELECT users.id, users.username FROM users">>,
                 erma:build({select, {table, "users"}, [{fields, ["users.id", "users.username"]}]})),
    ?assertEqual(<<"SELECT * FROM users WHERE (email = 'hey@hey.com' AND username = 'chris')">>,
                 erma:build({select, {table, "users"},
                             [{where, [{'and', [{"email", "hey@hey.com"},
                                                {"username", "chris"}]}]}]})),
    ?assertEqual(<<"SELECT * FROM users WHERE username = 'chris' ORDER BY created ASC">>,
                 erma:build({select, {table, "users"},
                             [{where, [{"username", "chris"}]},
                              {order, ["created"]}]})),
    ?assertEqual(<<"SELECT * FROM users WHERE active = true ORDER BY created ASC OFFSET 3, LIMIT 5">>,
                 erma:build({select, {table, "users"},
                             [{where, [{"active", true}]},
                              {order, ["created"]},
                              {limit, 5},
                              {offset, 3}]})),
    ok.
