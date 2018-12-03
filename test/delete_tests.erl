-module(delete_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

delete_test_() ->
    test_utils:generate(
      [{
         %%
         {delete, "users", [{where, [{"id", 3}]}]},
         %%
         <<"DELETE FROM users WHERE id = 3">>
       },
       {
         %%
         {delete, <<"users">>, [{where, []}]},
         %%
         <<"DELETE FROM users">>
       },
       {
         %%
         {delete, users, [{where, [{"id", 3}]}]},
         %%
         <<"DELETE FROM users WHERE id = 3">>
       },
       {
         %%
         {delete, users, [{where, [{{function, calculate_rating, ["created_at", "score"]}, '>', 100}]}]},
         %%
         <<"DELETE FROM users WHERE calculate_rating(created_at, score) > 100">>
       }
      ]).
