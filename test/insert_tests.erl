-module(insert_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

insert_test_() ->
    test_utils:generate(
      [{
         %%
         {insert, "users", ["first", "last"], ["Chris", "Granger"]},
         %%
         <<"INSERT INTO users (\"first\", \"last\") VALUES ('Chris', 'Granger')">>
       },
       {
         %%
         {insert, "users", ["first", last, <<"age">>], ["Bob", <<"Dou">>, 25]},
         %%
         <<"INSERT INTO users (\"first\", \"last\", age) VALUES ('Bob', 'Dou', 25)">>
       },
       {
         %%
         {insert, "users", ["first", "last"], ["?", "?"]},
         %%
         <<"INSERT INTO users (\"first\", \"last\") VALUES (?, ?)">>
       },
       {
         %%
         {insert_rows, "users", ["first", "last"],
          [["Chris", "Granger"], ["Bob", "Dou"], ["Helen", "Rice"]]},
         %%
         <<"INSERT INTO users (\"first\", \"last\") ",
           "VALUES ('Chris', 'Granger'), ('Bob', 'Dou'), ('Helen', 'Rice')">>
       },
       {
         %%
         {insert_rows, "users", ["first", "last", "age"],
          [["Bill", "Foo", 24], ["Bob", "Dou", 25], ["Helen", "Rice", 21]]},
         %%
         <<"INSERT INTO users (\"first\", \"last\", age) ",
           "VALUES ('Bill', 'Foo', 24), ('Bob', 'Dou', 25), ('Helen', 'Rice', 21)">>
       },
       {
         %%
         {insert, "users", [], [1, "Bob", "Dou", 25]},
         %%
         <<"INSERT INTO users VALUES (1, 'Bob', 'Dou', 25)">>
       },
       {
         %%
         {insert_rows, "users", [], [[1, "Bob", "Dou", 25], [2, "Bill", "Foo", 31]]},
         %%
         <<"INSERT INTO users VALUES (1, 'Bob', 'Dou', 25), (2, 'Bill', 'Foo', 31)">>
       },
       {
         %%
         {insert_rows, "users", [], [[1, "Bob", "Dou", {function, now, []}], [2, "Bill", "Foo", {function, now, []}]]},
         %%
         <<"INSERT INTO users VALUES (1, 'Bob', 'Dou', now()), (2, 'Bill', 'Foo', now())">>
       }
      ]).
