-module(update_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

update_test_() ->
    test_utils:generate(
      [{
         %%
         {update, "users", [{"first", "Chris"}, {"last", "Granger"}], [{where, [{"id", 3}]}]},
         %%
         <<"UPDATE users SET \"first\" = 'Chris', \"last\" = 'Granger' WHERE id = 3">>
       },
       {
         %%
         {update, "users", [{"first", "?"}, {"last", "?"}], [{where, [{"id", "?"}]}]},
         %%
         <<"UPDATE users SET \"first\" = ?, \"last\" = ? WHERE id = ?">>
       },
       {
         %%
         {update, "users", [{"first", "Chris"}, {"last", "Granger"}]},
         %%
         <<"UPDATE users SET \"first\" = 'Chris', \"last\" = 'Granger'">>
       },
       {
         %%
         {update, "users", [{"first", "?"}, {"last", "?"}]},
         %%
         <<"UPDATE users SET \"first\" = ?, \"last\" = ?">>
       },
       {
         %%
         {update, "users", [{"first", "Chris"}]},
         %%
         <<"UPDATE users SET \"first\" = 'Chris'">>
       },
       {
         %%
         {update, "users", [{"first", "?"}], [{where, [{"id", "?"}]}]},
         %%
         <<"UPDATE users SET \"first\" = ? WHERE id = ?">>
       },
       {
         %%
         {update, "users", [{"first", "Chris"}, {"last", "?"}], [{where, [{"id", "?"}]}]},
         %%
         <<"UPDATE users SET \"first\" = 'Chris', \"last\" = ? WHERE id = ?">>
       },
       {
         %%
         {update, "users", [{"logged_out", {function, now, []}}], [{where, [{"id", 3}]}]},
         %%
         <<"UPDATE users SET logged_out = now() WHERE id = 3">>
       },
       {
         %%
         {update, "users", [{"rating", {function, calculate_rating, [{function, now, []}, "score"]}}], [{where, [{"id", 3}]}]},
         %%
         <<"UPDATE users SET rating = calculate_rating(now(), score) WHERE id = 3">>
       },
       {
         %%
         {update, "users",
           [{"status", 2}],
           [{where, [{{function, calculate_rating, ["created_at", "score"]}, '>', 100}]}]
         },
         %%
         <<"UPDATE users SET \"status\" = 2 WHERE calculate_rating(created_at, score) > 100">>
       }
      ]).
