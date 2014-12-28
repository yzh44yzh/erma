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
       }
      ]).
