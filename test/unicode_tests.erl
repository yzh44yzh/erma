-module(unicode_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


unicode1_test() ->
    TUser = {table, <<"пользователь">>},
    Select1 = {select, TUser, []},
    ?assertEqual(<<"SELECT * FROM `пользователь`">>, erma:build(Select1)),

    Select2 = {select, TUser,
              [{where, [{"email", "some@where.com"}]}]},
    ?assertEqual(<<"SELECT * FROM `пользователь` WHERE email = 'some@where.com'">>,
                 erma:build(Select2)),

    Select3 = {select, TUser,
               [{fields, [<<"имя">>, <<"фамилия">>, "address.state"]},
                {where, [{"email", <<"кто-то@где.то">>}]}
              ]},
    ?assertEqual(<<"SELECT `имя`, `фамилия`, address.`state` ",
                   "FROM `пользователь` ",
                   "WHERE email = 'кто-то@где.то'">>,
                 erma:build(Select3)),

    TAddress = {table, <<"адрес">>},
    Select4 = {select, TUser,
              [{joins, [{left, TAddress}]},
               {fields, ["first_name", "last_name", "address.state"]}
              ]},
    ?assertEqual(<<"SELECT first_name, last_name, address.`state` ",
                   "FROM `пользователь` ",
                   "LEFT JOIN `адрес` ON `адрес`.id = `пользователь`.`адрес_id`">>,
                 erma:build(Select4)),
    ok.


unicode2_test() ->
    Select0 = {select, {table, <<"пользователь">>},
               [{fields, ["id", "username"]},
                {where, [{"email", like, "*@gmail.com"}]}
               ]},
    ?assertEqual(<<"SELECT id, username ",
                   "FROM `пользователь` ",
                   "WHERE email LIKE '*@gmail.com'">>,
                 erma:build(Select0)),

    Select1 = erma:append(Select0, [{where, [{<<"активен">>, true}, {<<"возраст">>, '>', 18}]},
                                    {order, [<<"дата_создания">>]}]),
    ?assertEqual(
       {select, {table, <<"пользователь">>},
        [{fields, ["id", "username"]},
         {where, [{"email", like, "*@gmail.com"}, {<<"активен">>, true}, {<<"возраст">>, '>', 18}]},
         {order, [<<"дата_создания">>]}
        ]},
       Select1),
    ?assertEqual(<<"SELECT id, username ",
                   "FROM `пользователь` ",
                   "WHERE email LIKE '*@gmail.com' ",
                   "AND `активен` = true ",
                   "AND `возраст` > 18 ",
                   "ORDER BY `дата_создания` ASC">>,
                 erma:build(Select1)),
    ok.


unicode3_test() ->
    Q1 = <<"INSERT INTO users (`first`, `last`) VALUES ('Боб', 'Дилан')">>,
    S1 = {insert, {table, "users"},
          [{"first", <<"Боб">>},
           {"last", <<"Дилан">>}]},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"INSERT INTO `артисты` (`имя`, `фамилия`, `возраст`) VALUES ('Фреди', 'Меркьюри', 25)">>,
    S2 = {insert, <<"артисты">>,
          [{<<"имя">>, <<"Фреди">>},
           {<<"фамилия">>, <<"Меркьюри">>},
           {<<"возраст">>, 25}]},
    ?assertEqual(Q2, erma:build(S2)),
    ok.


unicode4_test() ->
    Q1 = <<"UPDATE users SET `first` = 'Джон', `last` = 'Леннон' WHERE id = 3">>,
    U1 = {update, {table, "users"},
          [{"first", <<"Джон">>},
           {"last", <<"Леннон">>}],
          {where, [{"id", 3}]}},
    ?assertEqual(Q1, erma:build(U1)),

    Q2 = <<"UPDATE `чуваки` SET `погоняло` = ?, `крутизна` = ? WHERE id = ?">>,
    U2 = {update, {table, <<"чуваки">>},
          [<<"погоняло">>, <<"крутизна">>],
          {where, [{"id", "?"}]}},
    ?assertEqual(Q2, erma:build(U2)),
    ok.


unicode5_test() ->
    Q = <<"DELETE FROM `пользователь` WHERE id = 3">>,
    D = {delete, <<"пользователь">>, {where, [{"id", 3}]}},
    ?assertEqual(Q, erma:build(D)),
    ok.
