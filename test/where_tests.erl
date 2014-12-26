-module(where_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

where2_test() ->
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE (title LIKE '%funny%' OR subject LIKE '%funny%' OR content LIKE '%funny%')">>,
                 erma:build(
                   {select, [], "post",
                    [{where, [{'or', [{"title", like, "%funny%"},
                                      {"subject", like, "%funny%"},
                                      {"content", like, "%funny%"}]}
                             ]}
                    ]}
                  )),
    ok.


where3_test() ->
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE (posted > '17:15:00' OR posted < '18:05:00')">>,
                 erma:build(
                   {select, [], "post",
                    [{where, [{'or', [{"posted", '>', {time, {17, 15, 0}}},
                                      {"posted", '<', {time, {18, 5, 0}}}]}
                             ]}
                    ]}
                  )),
    ok.


where4_test() ->
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE (NOT (user_id = 20 OR user_id = 30))">>,
                 erma:build(
                   {select, [], "post",
                    [{where, [{'not', {'or', [{"user_id", 20},
                                              {"user_id", 30}]}}
                             ]}
                    ]}
                  )),
    ok.


where5_test() ->
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE `state` IN ('active', 'suspended', 'unknown')">>,
                 erma:build(
                   {select, [], "post",
                    [{where, [{"state", in, ["active", "suspended", "unknown"]}
                             ]}
                    ]}
                  )),
    ok.


where6_test() ->
    DT1 = {datetime, {{2014, 1, 1}, {22, 30, 0}}},
    DT2 = {datetime, {{2013, 12, 20}, {12, 15, 0}}},
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE (title LIKE '%funny%' OR subject LIKE '%funny%' OR content LIKE '%funny%') ",
                   "AND (blocked = false AND "
                   "(posted > '2014-01-01 22:30:00' OR posted < '2013-12-20 12:15:00')) ",
                   "AND (NOT (user_id = 20 OR user_id = 30)) ",
                   "AND `state` IN ('active', 'suspended', 'unknown')">>,
                 erma:build(
                   {select, [], "post",
                    [{where, [{'or', [{"title", like, "%funny%"},
                                      {"subject", like, "%funny%"},
                                      {"content", like, "%funny%"}]},
                              {'and', [{"blocked", false},
                                       {'or', [{"posted", '>', DT1},
                                               {"posted", '<', DT2}]}]},
                              {'not', {'or', [{"user_id", 20},
                                              {"user_id", 30}]}},
                              {"state", in, ["active", "suspended", "unknown"]}
                             ]}
                    ]}
                  )),
    ok.


where7_test() ->
    ?assertEqual(<<"SELECT users.* FROM users WHERE (users.`name` = ? OR users.`name` = ?)">>,
                 erma:build({select, ["users.*"], "users",
                             [{where, [{'or', [{"users.name", "?"},
                                               {"users.name", "?"}]}]}
                              ]})),

    ?assertEqual(<<"SELECT * FROM users WHERE ((`last` = ? AND `name` = ?) OR email = ? OR age > ?)">>,
                 erma:build({select, [], "users",
                              [{where, [{'or', [{'and', [{"last", "?"}, {"name", "?"}]},
                                                {"email", "?"},
                                                {"age", gt, "?"}]}]}
                               ]})),

    ?assertEqual(<<"SELECT * FROM users WHERE (x < 5 OR (y < 3 OR z > 4))">>,
                 erma:build({select, [], "users",
                              [{where, [{'or', [{"x", lt, 5},
                                                {'or', [{"y", lt, 3},
                                                        {"z", gt, 4}]}]}]}
                               ]})),

    ?assertEqual(<<"SELECT * FROM users WHERE (`name` LIKE ? OR `name` LIKE ?)">>,
                 erma:build({select, [], "users",
                              [{where, [{'or', [{"name", like, "?"},
                                                {"name", like, "?"}]}]}
                              ]})),
    ok.


where8_test() ->
    ?assertEqual(<<"SELECT * FROM the_table WHERE id NOT IN (1, 2, 3)">>,
                 erma:build({select, [], "the_table",
                             [{where, [{"id", not_in, [1,2,3]}]}
                              ]})),
    ?assertEqual(<<"SELECT * FROM test WHERE cool IN (77)">>,
                 erma:build({select, [], "test",
                             [{where, [{"cool", in, [77]}]}
                              ]})),
    ?assertEqual(<<"SELECT * FROM test WHERE cool IN (NULL)">>,
                 erma:build({select, [], "test",
                             [{where, [{"cool", in, []}]}
                              ]})),
    ?assertEqual(<<"SELECT * FROM test WHERE cool NOT IN (NULL)">>,
                 erma:build({select, [], "test",
                             [{where, [{"cool", not_in, []}]}
                              ]})),
    ok.


where9_test() ->
    ?assertEqual(<<"SELECT * FROM test WHERE id = 1">>,
                 erma:build({select, [], "test",
                             [{where, [{"id", 1}]}
                              ]})),
    ?assertEqual(<<"SELECT * FROM test WHERE id <> 1">>,
                 erma:build({select, [], "test",
                             [{where, [{"id", '<>', 1}]}
                              ]})),
    ?assertEqual(<<"SELECT * FROM test WHERE id < 10">>,
                 erma:build({select, [], "test",
                             [{where, [{"id", lt, 10}]}
                              ]})),
    ?assertEqual(<<"SELECT * FROM test WHERE id <= 10">>,
                 erma:build({select, [], "test",
                             [{where, [{"id", '<=', 10}]}
                              ]})),
    ?assertEqual(<<"SELECT * FROM test WHERE id BETWEEN 1 AND 10">>,
                 erma:build({select, [], "test",
                             [{where, [{"id", between, 1, 10}]}
                              ]})),
    ok.
