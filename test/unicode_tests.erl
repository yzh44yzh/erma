-module(unicode_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


unicode1_test() ->
    Select1 = {select, [], <<"пользователь"/utf8>>},
    ?assertEqual(<<"SELECT * FROM `пользователь`"/utf8>>, erma:build(Select1)),

    Select2 = {select, [], <<"пользователь"/utf8>>,
               [{where, [{"email", "some@where.com"}]}]},
    ?assertEqual(<<"SELECT * FROM `пользователь` WHERE email = 'some@where.com'"/utf8>>,
                 erma:build(Select2)),

    Select3 = {select, [<<"имя"/utf8>>, <<"фамилия"/utf8>>, "address.state"], <<"пользователь"/utf8>>,
               [{where, [{"email", <<"кто-то@где.то"/utf8>>}]}]},
    ?assertEqual(<<"SELECT `имя`, `фамилия`, address.`state` "/utf8,
                   "FROM `пользователь` "/utf8,
                   "WHERE email = 'кто-то@где.то'"/utf8>>,
                 erma:build(Select3)),

    TAddress = <<"адрес"/utf8>>,
    Select4 = {select, ["first_name", "last_name", "address.state"], <<"пользователь"/utf8>>,
               [{joins, [{left, TAddress}]}]},
    ?assertEqual(<<"SELECT first_name, last_name, address.`state` ",
                   "FROM `пользователь` "/utf8,
                   "LEFT JOIN `адрес` ON `адрес`.id = `пользователь`.`адрес_id`"/utf8>>,
                 erma:build(Select4)),
    ok.


unicode2_test() ->
    Select0 = {select, ["id", "username"], <<"пользователь"/utf8>>,
               [{where, [{"email", like, "*@gmail.com"}]}]},
    ?assertEqual(<<"SELECT id, username ",
                   "FROM `пользователь` "/utf8,
                   "WHERE email LIKE '*@gmail.com'">>,
                 erma:build(Select0)),

    Select1 = erma:append(Select0, [{where, [{<<"активен"/utf8>>, true}, {<<"возраст"/utf8>>, '>', 18}]},
                                    {order, [<<"дата_создания"/utf8>>]}]),
    ?assertEqual(
       {select, ["id", "username"], <<"пользователь"/utf8>>,
        [{where, [{"email", like, "*@gmail.com"}, {<<"активен"/utf8>>, true}, {<<"возраст"/utf8>>, '>', 18}]},
         {order, [<<"дата_создания"/utf8>>]}
        ]},
       Select1),
    ?assertEqual(<<"SELECT id, username ",
                   "FROM `пользователь` "/utf8,
                   "WHERE email LIKE '*@gmail.com' ",
                   "AND `активен` = true "/utf8,
                   "AND `возраст` > 18 "/utf8,
                   "ORDER BY `дата_создания` ASC"/utf8>>,
                 erma:build(Select1)),
    ok.


unicode3_test() ->
    Q1 = <<"INSERT INTO users (`first`, `last`) VALUES ('Боб', 'Дилан')"/utf8>>,
    S1 = {insert, "users", ["first", "last"], [<<"Боб"/utf8>>, <<"Дилан"/utf8>>]},
    ?assertEqual(Q1, erma:build(S1)),

    Q2 = <<"INSERT INTO `артисты` (`имя`, `фамилия`, `возраст`) VALUES ('Фреди', 'Меркьюри', 25)"/utf8>>,
    S2 = {insert, <<"артисты"/utf8>>,
          [<<"имя"/utf8>>, <<"фамилия"/utf8>>, <<"возраст"/utf8>>],
          [<<"Фреди"/utf8>>, <<"Меркьюри"/utf8>>, 25]},
    ?assertEqual(Q2, erma:build(S2)),
    ok.


unicode4_test() ->
    Q1 = <<"UPDATE users SET `first` = 'Джон', `last` = 'Леннон' WHERE id = 3"/utf8>>,
    U1 = {update, "users",
          [{"first", <<"Джон"/utf8>>},
           {"last", <<"Леннон"/utf8>>}],
          [{where, [{"id", 3}]}]},
    ?assertEqual(Q1, erma:build(U1)),

    Q2 = <<"UPDATE `чуваки` SET `погоняло` = ?, `крутизна` = ? WHERE id = ?"/utf8>>,
    U2 = {update, <<"чуваки"/utf8>>,
          [{<<"погоняло"/utf8>>, "?"}, {<<"крутизна"/utf8>>, "?"}],
          [{where, [{"id", "?"}]}]},
    ?assertEqual(Q2, erma:build(U2)),
    ok.


unicode5_test() ->
    Q = <<"DELETE FROM `пользователь` WHERE id = 3"/utf8>>,
    D = {delete, <<"пользователь"/utf8>>, [{where, [{"id", 3}]}]},
    ?assertEqual(Q, erma:build(D)),
    ok.
