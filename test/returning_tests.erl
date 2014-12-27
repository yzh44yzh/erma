-module(returning_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

returning_test_() ->
    test_utils:generate(
      [{
         %%
         {insert, "users", ["first", "last"], ["Chris", "Granger"], [{returning, id}]},
         %%
         <<"INSERT INTO users (`first`, `last`) VALUES ('Chris', 'Granger') RETURNING id">>
       },
       {
         %%
         {insert, "users", ["first", "last", "age"], ["Bob", "Dou", 25],
          [{returning, ["id", "first", "last"]}]},
         %%
         <<"INSERT INTO users (`first`, `last`, age) VALUES ('Bob', 'Dou', 25) ",
           "RETURNING id, `first`, `last`">>
       },
       {
         %%
         {insert, "users", ["first", "last"], ["?", "?"], [{returning, id}]},
         %%
         <<"INSERT INTO users (`first`, `last`) VALUES (?, ?) RETURNING id">>
       },
       {
         %%
         {insert_rows, "users", ["first", "last", "age"],
          [["Bill", "Foo", 24], ["Bob", "Dou", 25], ["Helen", "Rice", 21]],
          [{returning, id}]},
         %%
         <<"INSERT INTO users (`first`, `last`, age) ",
           "VALUES ('Bill', 'Foo', 24), ('Bob', 'Dou', 25), ('Helen', 'Rice', 21) ",
           "RETURNING id">>
       },
       {
         %%
         {insert, <<"users">>, [],
          [5, "Bob", "Dou", 25],
          [{returning, ["name", <<"age">>, "id"]}]},
         %%
         <<"INSERT INTO users VALUES (5, 'Bob', 'Dou', 25) ",
           "RETURNING `name`, age, id">>
       },
       {
         %%
         {insert_rows, "users", [],
          [[1, "Bob", "Dou", 65], [6, "Bill", "Foo", 31]],
          [{returning, id}]},
         %%
         <<"INSERT INTO users VALUES (1, 'Bob', 'Dou', 65), (6, 'Bill', 'Foo', 31) ",
           "RETURNING id">>
       },
       {
         %%
         {update, "users", [{"first", "Chris"}], [{returning, id}]},
         %%
         <<"UPDATE users SET `first` = 'Chris' RETURNING id">>
       },
       {
         %%
         {update, <<"users">>, [{"first", "?"}],
          [{where, [{"id", "?"}]},
           {returning, [<<"id">>, <<"first">>]}]},
         %%
         <<"UPDATE users SET `first` = ? WHERE id = ? RETURNING id, `first`">>
       },
       {
         %%
         {update, "users", [{"first", "Chris"}, {"last", "?"}],
          [{where, [{"id", "?"}]},
           {returning, ["id", "name", <<"first">>, "last", <<"age">>]}]},
         %%
         <<"UPDATE users SET `first` = 'Chris', `last` = ? WHERE id = ? ",
           "RETURNING id, `name`, `first`, `last`, age">>
       },
       {
         %%
         {update, "users",
          [{"first", "?"}, {"last", "?"}],
          [{returning, ["id"]}]},
         %%
         <<"UPDATE users SET `first` = ?, `last` = ? RETURNING id">>
       },
       {
         %%
         {update, "users",
          [{"first", "Chris"},
           {"last", "Granger"}],
          [{where, [{"id", 3}]},
           {returning, id}]},
         %%
         <<"UPDATE users SET `first` = 'Chris', `last` = 'Granger' ",
           "WHERE id = 3 RETURNING id">>
       },
       {
         %%
         {delete, "users", [{where, [{"id", 3}]}, {returning, id}]},
         %%
         <<"DELETE FROM users WHERE id = 3 RETURNING id">>
       },
       {
         %%
         {delete, "users", [{where, []}, {returning, id}]},
         %%
         <<"DELETE FROM users RETURNING id">>
       },
       {
         %%
         {delete, "users", [{returning, ["name", "age"]}]},
         %%
         <<"DELETE FROM users RETURNING `name`, age">>
       },
       {
         %%
         {delete, "users", [{returning, id}]},
         %%
         <<"DELETE FROM users RETURNING id">>
       }
      ]).
