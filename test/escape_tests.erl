-module(escape_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


escape1_test() ->
    Q1 = <<"SELECT id, first_name, last_name, age FROM users">>,
    S1 = {select, {table, "users"}, [{fields, ["id", "first_name", "last_name", "age"]}]},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"SELECT `user`.id, `user`.first_name, `user`.last_name, `user`.age FROM `user`">>,
    S2 = {select, {table, "user"},
          [{fields, ["user.id", "user.first_name", "user.last_name", "user.age"]}]},
    ?assertEqual(Q2, erma:build(S2)),

    Q3 = <<"SELECT id, `name`, age FROM users">>,
    S3 = {select, {table, "users"}, [{fields, ["id", "name", "age"]}]},
    ?assertEqual(Q3, erma:build(S3)),

    Q4 = <<"SELECT `user`.id, `user`.`name`, `user`.age FROM `user`">>,
    S4 = {select, {table, "user"}, [{fields, ["user.id", "user.name", "user.age"]}]},
    ?assertEqual(Q4, erma:build(S4)),

    Q5 = <<"SELECT `a`.id, `a`.`name`, `a`.age FROM users AS `a`">>,
    S5 = {select, {table, "users", as, "a"},
          [{fields, ["a.id", "a.name", "a.age"]}]},
    ?assertEqual(Q5, erma:build(S5)),

    ok.


escape2_test() ->
    Q1 = <<"SELECT * FROM users LEFT JOIN address ON address.id = users.address_id">>,
    S1 = {select, {table, "users"},
          [{joins, [{left, {table, "address"}}]}]},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"SELECT * FROM `user` LEFT JOIN address ON address.id = `user`.address_id">>,
    S2 = {select, {table, "user"},
          [{joins, [{left, {table, "address"}}]}]},
    ?assertEqual(Q2, erma:build(S2)),

    Q3 = <<"SELECT * FROM `user` LEFT JOIN address AS `a` ON `a`.id = `user`.address_id">>,
    S3 = {select, {table, "user"},
          [{joins, [{left, {table, "address", as, "a"}}]}]},
    ?assertEqual(Q3, erma:build(S3)),

    Q4 = <<"SELECT `scope`.id, `a`.`state` ",
           "FROM `state` AS `scope` ",
           "LEFT JOIN `result` AS `a` ON `a`.id = `scope`.result_id">>,
    S4 = {select, {table, "state", as, "scope"},
          [{joins, [{left, {table, "result", as, "a"}}]},
           {fields, ["scope.id", "a.state"]}
          ]},
    ?assertEqual(Q4, erma:build(S4)),

    ok.


escape4_test() ->
    Q1 = <<"SELECT * FROM users ",
           "LEFT JOIN address ON address.`state` = users.address_id">>,
    S1 = {select, {table, "users"},
          [{joins, [{left, {table, "address"}, [{pk, "state"}]}]}]},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"SELECT * FROM users ",
           "LEFT JOIN address ON address.id = users.`state`">>,
    S2 = {select, {table, "users"},
          [{joins, [{left, {table, "address"}, [{fk, "state"}]}]}]},
    ?assertEqual(Q2, erma:build(S2)),

    Q3 = <<"SELECT * FROM users ",
           "LEFT JOIN address ON address.`a` = users.`state`">>,
    S3 = {select, {table, "users"},
          [{joins, [{left, {table, "address"}, [{pk, "a"}, {fk, "state"}]}]}]},
    ?assertEqual(Q3, erma:build(S3)),

    Q4 = <<"SELECT * FROM `user` ",
           "LEFT JOIN `result` ON `result`.`a` = `user`.`state`">>,
    S4 = {select, {table, "user"},
          [{joins, [{left, {table, "result"}, [{pk, "a"}, {fk, "state"}]}]}]},
    ?assertEqual(Q4, erma:build(S4)),

    ok.


escape5_test() ->
    Q1 = <<"SELECT * FROM post WHERE `status` = 'active'">>,
    S1 = {select, {table, "post"},
          [{where, [{"status", "active"}]}
          ]},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"SELECT * FROM `result` WHERE `result`.`status` = 'active'">>,
    S2 = {select, {table, "result"},
          [{where, [{"result.status", "active"}]}
          ]},
    ?assertEqual(Q2, erma:build(S2)),

    Q3 = <<"SELECT * FROM post WHERE (`user` = 5 AND `status` = 'active')">>,
    S3 = {select, {table, "post"},
          [{where, [{'and', [{"user", 5},
                             {"status", "active"}]}]}
          ]},
    ?assertEqual(Q3, erma:build(S3)),

    ok.


escape6_test() ->
    Q1 = <<"SELECT * FROM post ORDER BY `post-id` DESC, `status` ASC, publish_date ASC">>,
    S1 = {select, {table, "post"},
          [{order, [{"post-id", desc}, "status", "publish_date"]}]},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"SELECT * FROM `last-posts` ORDER BY `last-posts`.id DESC, `last-posts`.`status` ASC">>,
    S2 = {select, {table, "last-posts"},
          [{order, [{"last-posts.id", desc}, {"last-posts.status", asc}]}]},
    ?assertEqual(Q2, erma:build(S2)),

    ok.


escape7_test() ->
    Q1 = <<"INSERT INTO `my-post` (title, `status`) VALUES ('hello', 'active')">>,
    I1 = {insert, {table, "my-post"}, [{"title", "hello"}, {"status", "active"}]},
    ?assertEqual(Q1, erma:build(I1)),

    Q2 = <<"INSERT INTO `my-post` (`@name`, `status`) VALUES ('Bob', 'active'), ('Bill', 'blocked')">>,
    I2 = {insert, {table, "my-post"}, ["@name", "status"], [["Bob", "active"], ["Bill", "blocked"]]},
    ?assertEqual(Q2, erma:build(I2)),

    ok.


escape8_test() ->
    Q1 = <<"UPDATE `user` SET `name` = 'Bob' WHERE `user`.id = 1">>,
    U1 = {update, {table, "user"}, [{"name", "Bob"}], {where, [{"user.id", 1}]}},
    ?assertEqual(Q1, erma:build(U1)),

    Q2 = <<"UPDATE `my-posts` SET `my-posts`.`time` = '22:30:00' WHERE id = 1">>,
    U2 = {update, {table, "my-posts"},
          [{"my-posts.time", {time, {22, 30, 0}}}],
          {where, [{"id", 1}]}},
    ?assertEqual(Q2, erma:build(U2)),

    ok.


escape9_test() ->
    Q1 = <<"DELETE FROM `user` WHERE `user`.id = 1">>,
    U1 = {delete, {table, "user"}, {where, [{"user.id", 1}]}},
    ?assertEqual(Q1, erma:build(U1)),

    Q2 = <<"DELETE FROM `my-posts` WHERE `my-posts`.`time` > '22:30:00'">>,
    U2 = {delete, {table, "my-posts"},
          {where, [{"my-posts.time", '>', {time, {22, 30, 0}}}]}},
    ?assertEqual(Q2, erma:build(U2)),

    ok.
