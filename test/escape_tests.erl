-module(escape_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

empty_test_() ->
    test_utils:generate(
      [{
         %%
         {select, ["id", "first_name", "last_name", "age"], "users"},
         %%
         <<"SELECT id, first_name, last_name, age FROM users">>
       },
       {
         %%
         {select, ["user.id", "user.first_name", "user.last_name", "user.age"], "user"},
         %%
         <<"SELECT \"user\".id, \"user\".first_name, \"user\".last_name, \"user\".age FROM \"user\"">>
       },
       {
         %%
         {select, ["id", "name", "age"], "users"},
         %%
         <<"SELECT id, \"name\", age FROM users">>
       },
       {
         %%
         {select, ["user.id", "user.name", "user.age"], "user"},
         %%
         <<"SELECT \"user\".id, \"user\".\"name\", \"user\".age FROM \"user\"">>
       },
       {
         %%
         {select, ["a.id", "a.name", "a.age"], {"users", as, "a"}},
         %%
         <<"SELECT \"a\".id, \"a\".\"name\", \"a\".age FROM users AS \"a\"">>
       },
       {
         %%
         {select, [], "users", [{joins, [{left, "address"}]}]},
         %%
         <<"SELECT * FROM users LEFT JOIN address ON address.id = users.address_id">>
       },
       {
         %%
         {select, [], "user", [{joins, [{left, "address"}]}]},
         %%
         <<"SELECT * FROM \"user\" LEFT JOIN address ON address.id = \"user\".address_id">>
       },
       {
         %%
         {select, [], "user", [{joins, [{left, {"address", as, "a"}}]}]},
         %%
         <<"SELECT * FROM \"user\" LEFT JOIN address AS \"a\" ON \"a\".id = \"user\".address_id">>
       },
       {
         %%
         {select, ["scope.id", "a.state"], {"state", as, "scope"},
          [{joins, [{left, {"result", as, "a"}}]}]},
         %%
         <<"SELECT \"scope\".id, \"a\".\"state\" ",
           "FROM \"state\" AS \"scope\" ",
           "LEFT JOIN \"result\" AS \"a\" ON \"a\".id = \"scope\".result_id">>
       },
       {
         %%
         {select, [], "users", [{joins, [{left, "address", [{pk, "state"}]}]}]},
         %%
         <<"SELECT * FROM users LEFT JOIN address ON address.\"state\" = users.address_id">>
       },
       {
         %%
         {select, [], "users", [{joins, [{left, "address", [{fk, "state"}]}]}]},
         %%
         <<"SELECT * FROM users LEFT JOIN address ON address.id = users.\"state\"">>
       },
       {
         %%
         {select, [], "users", [{joins, [{left, "address", [{pk, "a"}, {fk, "state"}]}]}]},
         %%
         <<"SELECT * FROM users LEFT JOIN address ON address.\"a\" = users.\"state\"">>
       },
       {
         %%
         {select, [], "user", [{joins, [{left, "result", [{pk, "a"}, {fk, "state"}]}]}]},
         %%
         <<"SELECT * FROM \"user\" LEFT JOIN \"result\" ON \"result\".\"a\" = \"user\".\"state\"">>
       },
       {
         %%
         {select, [], "post", [{where, [{"status", "active"}]}]},
         %%
         <<"SELECT * FROM post WHERE \"status\" = 'active'">>
       },
       {
         %%
         {select, [], "result", [{where, [{"result.status", "active"}]}]},
         %%
         <<"SELECT * FROM \"result\" WHERE \"result\".\"status\" = 'active'">>
       },
       {
         %%
         {select, [], "post",
          [{where, [{'and', [{"user", 5},
                             {"status", "active"}]}]}
          ]},
         %%
         <<"SELECT * FROM post WHERE (\"user\" = 5 AND \"status\" = 'active')">>
       },
       {
         %%
         {select, [], "post", [{order, [{"post-id", desc}, "status", "publish_date"]}]},
         %%
         <<"SELECT * FROM post ORDER BY \"post-id\" DESC, \"status\" ASC, publish_date ASC">>
       },
       {
         %%
         {select, [], "last-posts", [{order, [{"last-posts.id", desc}, {"last-posts.status", asc}]}]},
         %%
         <<"SELECT * FROM \"last-posts\" ORDER BY \"last-posts\".id DESC, \"last-posts\".\"status\" ASC">>
       },
       {
         %%
         {insert, "my-post", ["title", "status"], ["hello", "active"]},
         %%
         <<"INSERT INTO \"my-post\" (title, \"status\") VALUES ('hello', 'active')">>
       },
       {
         %%
         {insert_rows, "my-post", ["@name", "status"], [["Bob", "active"], ["Bill", "blocked"]]},
         %%
         <<"INSERT INTO \"my-post\" (\"@name\", \"status\") VALUES ('Bob', 'active'), ('Bill', 'blocked')">>
       },
       {
         %%
         {update, "user", [{"name", "Bob"}], [{where, [{"user.id", 1}]}]},
         %%
         <<"UPDATE \"user\" SET \"name\" = 'Bob' WHERE \"user\".id = 1">>
       },
       {
         %%
         {update, "my-posts", [{"my-posts.time", {time, {22, 30, 0}}}], [{where, [{"id", 1}]}]},
         %%
         <<"UPDATE \"my-posts\" SET \"my-posts\".\"time\" = '22:30:00' WHERE id = 1">>
       },
       {
         %%
         {delete, "user", [{where, [{"user.id", 1}]}]},
         %%
         <<"DELETE FROM \"user\" WHERE \"user\".id = 1">>
       },
       {
         %%
         {delete, "my-posts", [{where, [{"my-posts.time", '>', {time, {22, 30, 0}}}]}]},
         %%
         <<"DELETE FROM \"my-posts\" WHERE \"my-posts\".\"time\" > '22:30:00'">>
       }
      ]).
