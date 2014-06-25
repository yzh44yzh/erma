# Erma

Tasty SQL for Erlang

SQL builder for Erlang inspired by [Korma, Tasty SQL for Clojure](http://sqlkorma.com/)

Erma is a DSL language to describe and generate SQL queries.
Erma is not a db driver. Erma doesn't connect to db, doesn't send requests.


## Samples

Simple select:

```erlang
    Select = {select, TUser,
             [{with, [TAddress]},
              {fields, ["first_name", "last_name", "address.state"]},
              {where, [{"email", "some@where.com"}]}
             ]},

    ?assertEqual(<<"SELECT first_name, last_name, address.state ",
                   "FROM user ",
                   "LEFT JOIN address ON address.id = user.address_id ",
                   "WHERE email = 'some@where.com'">>,
                 erma:build(Select)),
```

Append more details to select:

```erlang
    Select0 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {where, [{"email", like, "*@gmail.com"}]}
               ]},
    ?assertEqual(<<"SELECT id, username ",
                   "FROM user ",
                   "WHERE email LIKE '*@gmail.com'">>,
                 erma:build(Select0)),

    Select1 = erma:append(Select0, [{where, [{"active", true}, {"age", '>', 18}]},
                                    {order, ["created"]}]),
    ?assertEqual(
       {select, {table, "user"},
        [{fields, ["id", "username"]},
         {where, [{"email", like, "*@gmail.com"}, {"active", true}, {"age", '>', 18}]},
         {order, ["created"]}
        ]},
       Select1),
    ?assertEqual(<<"SELECT id, username ",
                   "FROM user ",
                   "WHERE email LIKE '*@gmail.com' ",
                   "AND active = true ",
                   "AND age > 18 ",
                   "ORDER BY created ASC">>,
                 erma:build(Select1))
```

Join tables:

```erlang
    TEmail = {table, "email"},
    TAddress = {table, "address"},
    TAccount = {table, "account"},
    TUser = {table, "user",
             [{has_one, TEmail},
              {has_one, TAddress},
              {has_one, TAccount}
             ]},

    Select = {select, TUser,
              [{with, [TEmail, TAddress, TAccount]},
               {fields, ["email.email", "address.state", "account.name"]}
              ]},

    ?assertEqual(<<"SELECT email.email, address.state, account.name ",
                   "FROM user ",
                   "LEFT JOIN email ON email.id = user.email_id ",
                   "LEFT JOIN address ON address.id = user.address_id ",
                   "LEFT JOIN account ON account.id = user.account_id">>,
                 erma:build(Select))
```

See [unit tests](blob/master/test/erma_tests.erl) for more samples.
