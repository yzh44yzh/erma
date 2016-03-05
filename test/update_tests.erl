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
       }
      ]).
