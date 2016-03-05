-module(unicode_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

unicode_test_() ->
    test_utils:generate(
      [{
         %%
         {select, [], <<"пользователь"/utf8>>},
         %%
         <<"SELECT * FROM \"пользователь\""/utf8>>
       },
       {
         %%
         {select, [], <<"пользователь"/utf8>>, [{where, [{"email", "some@where.com"}]}]},
         %%
         <<"SELECT * FROM \"пользователь\" WHERE email = 'some@where.com'"/utf8>>
       },
       {
         %%
         {select, [<<"имя"/utf8>>, <<"фамилия"/utf8>>, "address.state"], <<"пользователь"/utf8>>,
          [{where, [{"email", <<"кто-то@где.то"/utf8>>}]}]},
         %%
         <<"SELECT \"имя\", \"фамилия\", address.\"state\" "/utf8,
           "FROM \"пользователь\" "/utf8,
           "WHERE email = 'кто-то@где.то'"/utf8>>
       },
       {
         %%
         {select, ["first_name", "last_name", "address.state"], <<"пользователь"/utf8>>,
          [{joins, [{left, <<"адрес"/utf8>>}]}]},
         %%
         <<"SELECT first_name, last_name, address.\"state\" ",
           "FROM \"пользователь\" "/utf8,
           "LEFT JOIN \"адрес\" ON \"адрес\".id = \"пользователь\".\"адрес_id\""/utf8>>
       },
       {
         %%
         {insert, "users", ["first", "last"], [<<"Боб"/utf8>>, <<"Дилан"/utf8>>]},
         %%
         <<"INSERT INTO users (\"first\", \"last\") VALUES ('Боб', 'Дилан')"/utf8>>
       },
       {
         %%
         {insert, <<"артисты"/utf8>>,
          [<<"имя"/utf8>>, <<"фамилия"/utf8>>, <<"возраст"/utf8>>],
          [<<"Фреди"/utf8>>, <<"Меркьюри"/utf8>>, 25]},
         %%
         <<"INSERT INTO \"артисты\" (\"имя\", \"фамилия\", \"возраст\") VALUES ('Фреди', 'Меркьюри', 25)"/utf8>>
       },
       {
         %%
         {update, "users",
          [{"first", <<"Джон"/utf8>>},
           {"last", <<"Леннон"/utf8>>}],
          [{where, [{"id", 3}]}]},
         %%
         <<"UPDATE users SET \"first\" = 'Джон', \"last\" = 'Леннон' WHERE id = 3"/utf8>>
       },
       {
         %%
         {update, <<"чуваки"/utf8>>,
          [{<<"погоняло"/utf8>>, "?"}, {<<"крутизна"/utf8>>, "?"}],
          [{where, [{"id", "?"}]}]},
         %%
         <<"UPDATE \"чуваки\" SET \"погоняло\" = ?, \"крутизна\" = ? WHERE id = ?"/utf8>>
       },
       {
         %%
         {delete, <<"пользователь"/utf8>>, [{where, [{"id", 3}]}]},
         %%
         <<"DELETE FROM \"пользователь\" WHERE id = 3"/utf8>>
       }
      ]).
