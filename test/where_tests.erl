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


%% where7_test() ->

%%         "SELECT \"users\".* FROM \"users\" WHERE (\"users\".\"name\" = ? OR \"users\".\"name\" = ?)"
%%         (select users
%%                 (where (or (= :name "chris")
%%                            (= :name "john"))))
%%         "SELECT \"users\".* FROM \"users\" WHERE ((\"users\".\"name\" = ?) OR (\"users\".\"name\" = ?))"
%%         (select users
%%                 (where (or {:name "chris"}
%%                            {:name "john"})))
%%         "SELECT \"users\".* FROM \"users\" WHERE ((\"users\".\"last\" = ? AND \"users\".\"name\" = ?) OR (\"users\".\"email\" = ?) OR \"users\".\"age\" > ?)"
%%         (select users
%%                 (where (or {:name "drew"
%%                             :last "dreward"}
%%                            {:email "drew@drew.com"}
%%                            (> :age 10))))
%%         "SELECT \"users\".* FROM \"users\" WHERE (\"users\".\"x\" < ? OR (\"users\".\"y\" < ? OR \"users\".\"z\" > ?))"
%%         (select users
%%                 (where (or (< :x 5)
%%                            (or (< :y 3)
%%                                (> :z 4)))))
%%         "SELECT \"users\".* FROM \"users\" WHERE (\"users\".\"name\" LIKE ?)"
%%         (select users
%%                 (where {:name [like "chris"]}))
%%         "SELECT \"users\".* FROM \"users\" WHERE ((\"users\".\"name\" LIKE ?) OR \"users\".\"name\" LIKE ?)"
%%         (select users
%%                 (where (or {:name [like "chris"]}
%%                            (like :name "john")))))))


%% where8_test() ->

%% (deftest not-in
%%   (defentity the_table)
%%   (is (= "SELECT \"the_table\".* FROM \"the_table\" WHERE (\"the_table\".\"id\" NOT IN (?, ?, ?))"
%%          (sql-only
%%           (-> (select* the_table)
%%               (where {:id [not-in [1 2 3]]})
%%               (exec))))))


%%    (are [result query] (= result query)
%%         "SELECT \"test\".* FROM \"test\" WHERE (\"test\".\"cool\" IN (?))"
%%         (select :test (where {:cool [in [1]]}))

%%         "SELECT \"test\".* FROM \"test\" WHERE (\"test\".\"cool\" IN (NULL))"
%%         (select :test (where {:cool [in []]})))))


%% where9_test() ->

%% (deftest predicates-used-with-brackets
%%   (sql-only
%%    (are [result query] (= result query)
%%         "SELECT \"test\".* FROM \"test\" WHERE (\"test\".\"id\" = ?)"
%%         (select :test (where {:id [= 1]}))
%%         "SELECT \"test\".* FROM \"test\" WHERE (\"test\".\"id\" < ?)"
%%         (select :test (where {:id [< 10]}))
%%         "SELECT \"test\".* FROM \"test\" WHERE (\"test\".\"id\" <= ?)"
%%         (select :test (where {:id [<= 10]}))
%%         "SELECT \"test\".* FROM \"test\" WHERE ((\"test\".\"id\" BETWEEN ? AND ?))"
%%         (select :test (where {:id [between [1 10]]}))

%%         ;; clearly this is not an intended use of 'or'!
%%         "SELECT \"test\".* FROM \"test\" WHERE ((\"test\".\"id\" OR (?, ?, ?)))"
%%         (select :test (where {:id [or [1 2 3]]}))

%%         "SELECT \"test\".* FROM \"test\" WHERE (\"test\".\"id\" NOT IN (?, ?, ?))"
%%         (select :test (where {:id [not-in [1 2 3]]}))
%%         "SELECT \"test\".* FROM \"test\" WHERE (\"test\".\"id\" <> ?)"
%%         (select :test (where {:id [not= 1]})))))
