-module(empty_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


empty_where_test() ->
    ?assertEqual(<<"SELECT id, username FROM users">>,
                 erma:build({select, {table, "users"},
                             [{fields, ["id", "username"]},
                              {where, []}]})),
    ok.


empty_and_or_test() ->
    ?assertEqual(<<"SELECT id, username FROM users">>,
                 erma:build({select, {table, "users"},
                             [{fields, ["id", "username"]},
                              {where, [{'and', []}]
                              }]})),
    ?assertEqual(<<"SELECT id, username FROM users">>,
                 erma:build({select, {table, "users"},
                             [{fields, ["id", "username"]},
                              {where, [{'or', []}]
                              }]})),
    ?assertEqual(<<"SELECT id, username FROM users">>,
                 erma:build({select, {table, "users"},
                             [{fields, ["id", "username"]},
                              {where, [{'and', [{'and', []},
                                                {'or', []}]}]}
                              ]})),
    ok.


empty_joins_test() ->
    ?assertEqual(<<"SELECT id, username FROM users">>,
                 erma:build({select, {table, "users"},
                             [{fields, ["id", "username"]},
                              {joins, []}
                              ]})),
    ok.


empty_order_test() ->
    ?assertEqual(<<"SELECT id, username FROM users">>,
                 erma:build({select, {table, "users"},
                             [{fields, ["id", "username"]},
                              {order, []}
                              ]})),
    ok.
