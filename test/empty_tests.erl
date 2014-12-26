-module(empty_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


empty_where_test() ->
    ?assertEqual(<<"SELECT id, username FROM users">>,
                 erma:build({select, ["id", "username"], "users",
                             [{where, []}]})),
    ok.


empty_and_or_test() ->
    ?assertEqual(<<"SELECT id, username FROM users">>,
                 erma:build({select, ["id", "username"], <<"users">>,
                             [{where, [{'and', []}]}]})),
    ?assertEqual(<<"SELECT id, username FROM users">>,
                 erma:build({select, ["id", "username"], users,
                             [{where, [{'or', []}]}]})),
    ?assertEqual(<<"SELECT id, username FROM users">>,
                 erma:build({select, ["id", "username"], "users",
                             [{where, [{'and', [{'and', []}, {'or', []}]}]}]})),
    ok.

empty_order_test() ->
    ?assertEqual(<<"SELECT id, username FROM users">>,
                 erma:build({select, ["id", "username"], "users",
                             [{order, []}]})),
    ok.
