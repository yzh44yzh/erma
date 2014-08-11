# Erma

Tasty SQL for Erlang

SQL builder for Erlang inspired by [Korma, Tasty SQL for Clojure](http://sqlkorma.com/)

Erma is a DSL language to describe and generate SQL queries.
Erma is not a db driver. Erma doesn't connect to db, doesn't send requests.

IT IS JUST PROTOTYPE, NOT READY TO USE.


## Samples

### Simple select

```erlang
    TUser = {table, "user"},
    TAddress = {table, "address"},
    Select = {select, TUser,
             [{joins, [{left, TAddress}]},
              {fields, ["first_name", "last_name", "address.state"]}
             ]},
    erma:build(Select),
```

gives

```erlang
<<"SELECT first_name, last_name, address.state ",
  "FROM user ",
  "LEFT JOIN address ON address.id = user.address_id">>
```

### Append more details to select

```erlang
    Select0 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {where, [{"email", like, "*@gmail.com"}]}
               ]},
    Select1 = erma:append(Select0, [{where, [{"active", true}, {"age", '>', 18}]},
                                    {order, ["created"]}]),
    erma:build(Select1),
```

gives

```erlang
<<"SELECT id, username ",
  "FROM user ",
  "WHERE email LIKE '*@gmail.com' ",
  "AND active = true ",
  "AND age > 18 ",
  "ORDER BY created ASC">>
```

### Join tables

```erlang
    TUser = {table, "user"},
    TEmail = {table, "email"},
    TAddress1 = {table, "address", as, "a1"},
    TAddress2 = {table, "address", as, "a2"},
    TAccount = {table, "account"},
    Select = {select, TUser,
              [{fields, ["email.email", "address1.state",
                         "address2.state", "account.name"]},
               {joins, [{left, TEmail},
                        {right, TAddress1},
                        {inner, TAddress2},
                        {full, TAccount}]}
              ]},
    erma:build(Select)),
```

gives

```erlang
<<"SELECT email.email, address1.state, address2.state, account.name ",
  "FROM user ",
  "LEFT JOIN email ON email.id = user.email_id ",
  "RIGHT JOIN address AS a1 ON a1.id = user.address_id ",
  "INNER JOIN address AS a2 ON a2.id = user.address_id ",
  "FULL JOIN account ON account.id = user.account_id">>
```

See [unit tests](test/erma_tests.erl) for more samples.
