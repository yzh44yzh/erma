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
                                            {"posted", '>', {date, {2014, 2, 20}}}]}),
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
                   "WHERE (posted > '17:15:00' OR posted < '18:05:00')">>,
                 erma:build(
                   {select, {table, "post"},
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
    DT1 = {datetime, {{2014, 1, 1}, {22, 30, 0}}},
    DT2 = {datetime, {{2013, 12, 20}, {12, 15, 0}}},
    ?assertEqual(<<"SELECT * FROM post ",
                   "WHERE (title LIKE '%funny%' OR subject LIKE '%funny%' OR content LIKE '%funny%') ",
                   "AND (blocked = false AND "
                   "(posted > '2014-01-01 22:30:00' OR posted < '2013-12-20 12:15:00')) ",
                   "AND (NOT (user_id = 20 OR user_id = 30)) ",
                   "AND state IN ('active', 'suspended', 'unknown')">>,
                 erma:build(
                   {select, {table, "post"},
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
