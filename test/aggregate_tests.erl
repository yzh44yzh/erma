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
         {select, ["id", {max, "age"}], "users", [{where, [{"id", '>', 3}]}]},
         %%
         <<"SELECT id, MAX(age) FROM users WHERE id > 3">>
       },
       {
         %%
         {select, ["id", {avg, "age"}, "age"], "users", [{where, [{"id", '>', 3}]}]},
         %%
         <<"SELECT id, AVG(age), age FROM users WHERE id > 3">>
       },
       {
         %%
         {select_distinct, [{stdev, "users.age"}], "users"},
         %%
         <<"SELECT DISTINCT STDEV(users.age) FROM users">>
       },
       {
         %%
         {select, [{max, "users.age"}, {avg, "users.height"}], "users",
          [{where, [{"users.state", '<>', "blocked"}]}]},
         %%
         <<"SELECT MAX(users.age), AVG(users.height) FROM users WHERE users.`state` <> 'blocked'">>
       }
      ]).
