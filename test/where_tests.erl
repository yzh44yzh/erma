-module(where_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

where1_test() ->
    Select0 = {select, {table, "post"}, []},
    ?assertEqual(<<"SELECT * FROM post">>, erma:build(Select0)),

    Select1 = erma:append(Select0, {where, [{"user_id", 10}]}),
    ?assertEqual(<<"SELECT * FROM post WHERE user_id = 10">>,
                 erma:build(Select1)),

    Select2 = erma:append(Select1, {where, [{'not', {"blocked", true}},
                                            {"posted", '>', "2014-02-20"}]}),
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE user_id = 10 AND (NOT blocked = true) AND posted > '2014-02-20'">>,
                 erma:build(Select2)),
    ok.


where2_test() ->
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE (title LIKE '%funny%' OR subject LIKE '%funny%' OR content LIKE '%funny%')">>,
                 erma:build(
                   {select, {table, "post"},
                    [{where, [{'or', [{"title", like, "%funny%"},
                                      {"subject", like, "%funny%"},
                                      {"content", like, "%funny%"}]}
                             ]}
                    ]}
                  )),
    ok.


where3_test() ->
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE (posted > '2014-01-01' OR posted < '2013-12-20')">>,
                 erma:build(
                   {select, {table, "post"},
                    [{where, [{'or', [{"posted", '>', "2014-01-01"},
                                      {"posted", '<', "2013-12-20"}]}
                             ]}
                    ]}
                  )),
    ok.


where4_test() ->
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE (NOT (user_id = 20 OR user_id = 30))">>,
                 erma:build(
                   {select, {table, "post"},
                    [{where, [{'not', {'or', [{"user_id", 20},
                                              {"user_id", 30}]}}
                             ]}
                    ]}
                  )),
    ok.


where5_test() ->
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE state IN ('active', 'suspended', 'unknown')">>,
                 erma:build(
                   {select, {table, "post"},
                    [{where, [{"state", in, ["active", "suspended", "unknown"]}
                             ]}
                    ]}
                  )),
    ok.


where6_test() ->
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE (title LIKE '%funny%' OR subject LIKE '%funny%' OR content LIKE '%funny%') ",
                   "AND (blocked = false AND (posted > '2014-01-01' OR posted < '2013-12-20')) ",
                   "AND (NOT (user_id = 20 OR user_id = 30)) ",
                   "AND state IN ('active', 'suspended', 'unknown')">>,
                 erma:build(
                   {select, {table, "post"},
                    [{where, [{'or', [{"title", like, "%funny%"},
                                      {"subject", like, "%funny%"},
                                      {"content", like, "%funny%"}]},
                              {'and', [{"blocked", false},
                                       {'or', [{"posted", '>', "2014-01-01"},
                                               {"posted", '<', "2013-12-20"}]}]},
                              {'not', {'or', [{"user_id", 20},
                                              {"user_id", 30}]}},
                              {"state", in, ["active", "suspended", "unknown"]}
                             ]}
                    ]}
                  )),
    ok.
