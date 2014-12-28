-module(aggregate_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

aggregate_test_() ->
    test_utils:generate(
      [{
         %%
         {select, [{count, "id"}], "users"},
         %%
         <<"SELECT COUNT(id) FROM users">>
       },
       {
         %%
         {select, ["id", {max, "age"}], "users", [{where, [{"id", '>', 3}]}, {group, ["id"]}]},
         %%
         <<"SELECT id, MAX(age) FROM users WHERE id > 3 GROUP BY id">>
       },
       {
         %%
         {select, ["id", {avg, "age"}, "age"], "users", [{where, [{"id", '>', 3}]}, {group, ["id", "age"]}]},
         %%
         <<"SELECT id, AVG(age), age FROM users WHERE id > 3 GROUP BY id, age">>
       },
       {
         %%
         {select_distinct, [{stdev, "users.age"}], "users"},
         %%
         <<"SELECT DISTINCT STDEV(users.age) FROM users">>
       },
       {
         %%
         {select, [{max, "users.age"}, {avg, "users.height"}, "level"], "users",
          [{where, [{"users.state", '<>', "blocked"}]}, {group, [<<"level">>]}]},
         %%
         <<"SELECT MAX(users.age), AVG(users.height), `level` FROM users WHERE users.`state` <> 'blocked' GROUP BY `level`">>
       },
       {
         %%
         {select, ["id", {max, "age"}], "users", [{where, [{"id", '>', 3}]}, {group, ["id"]}, {having, [{"id", lt, 10}]}]},
         %%
         <<"SELECT id, MAX(age) FROM users WHERE id > 3 GROUP BY id HAVING id < 10">>
       },
       {
         %%
         {select, ["id", {avg, "age"}, "age"], "users", [{where, [{"id", '>', 3}]}, {group, ["id", "age"]}, {having, [{"age", gt, 18}]}]},
         %%
         <<"SELECT id, AVG(age), age FROM users WHERE id > 3 GROUP BY id, age HAVING age > 18">>
       },
       {
         %%
         {select, [{count, "id", as, "num_users"}], "users"},
         %%
         <<"SELECT COUNT(id) AS num_users FROM users">>
       },
       {
         %%
         {select, ["id", {avg, "age", as, "avg_age"}], "users",
          [{where, [{"id", '>', 3}]}, {group, ["id"]}, {having, [{"avg_age", gt, 18}]}]},
         %%
         <<"SELECT id, AVG(age) AS avg_age FROM users WHERE id > 3 GROUP BY id HAVING avg_age > 18">>
       },
       {
         %%
         {select, [{max, "users.age", as, "max_age"}, {avg, "users.height", as, "avg_height"}], "users",
          [{where, [{"users.state", '<>', "blocked"}]}]},
         %%
         <<"SELECT MAX(users.age) AS max_age, AVG(users.height) AS avg_height FROM users WHERE users.`state` <> 'blocked'">>
       }
      ]).
