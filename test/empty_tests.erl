-module(empty_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

empty_test_() ->
    test_utils:generate(
      [{
         %%
         {select, ["id", "username"], "users", [{where, []}]},
         %%
         <<"SELECT id, username FROM users">>
       },
       {
         %%
         {select, ["id", "username"], <<"users">>, [{where, [{'and', []}]}]},
         %%
         <<"SELECT id, username FROM users">>
       },
       {
         %%
         {select, ["id", "username"], users, [{where, [{'or', []}]}]},
         %%
         <<"SELECT id, username FROM users">>
       },
       {
         %%
         {select, ["id", "username"], "users", [{where, [{'and', [{'and', []}, {'or', []}]}]}]},
         %%
         <<"SELECT id, username FROM users">>
       },
       {
         %%
         {select, ["id", "username"], "users", [{order, []}]},
         %%
         <<"SELECT id, username FROM users">>
       }]).
