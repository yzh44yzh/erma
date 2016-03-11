-module(placeholders_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

placeholders_test_() ->
    test_utils:generate(
        [
            {
                %%
                {insert, "users", ["first", "last", "age"], ["?", "?", "?"]},
                %%
                <<"INSERT INTO users (\"first\", \"last\", age) VALUES (?, ?, ?)">>
            },
            {
                %%
                {insert, "users", ["first", <<"last">>, age], ["$1", "$2", "$3"]},
                %%
                <<"INSERT INTO users (\"first\", \"last\", age) VALUES ($1, $2, $3)">>
            },
            {
                %%
                {select, [], "users",
                    [{where, [{'or', [{'and', [{"last", "?"}, {"name", "?"}]},
                        {"email", "?"},
                        {"age", gt, "?"}]}]}
                    ]},
                %%
                <<"SELECT * FROM users WHERE ((\"last\" = ? AND \"name\" = ?) OR email = ? OR age > ?)">>
            },
            {
                %%
                {select, [], "users",
                    [{where, [{'or', [{'and', [{"last", "$1"}, {"name", "$1"}]},
                        {"email", "$2"},
                        {"age", gt, "$3"}]}]}
                    ]},
                %%
                <<"SELECT * FROM users WHERE ((\"last\" = $1 AND \"name\" = $1) OR email = $2 OR age > $3)">>
            },
            {
                %%
                {update, "users", [{"first", "Chris"}, {"last", "?"}], [{where, [{"id", "?"}]}]},
                %%
                <<"UPDATE users SET \"first\" = 'Chris', \"last\" = ? WHERE id = ?">>
            },
            {
                %%
                {update, "users", [{"first", "Chris"}, {"last", "$2"}], [{where, [{"id", "$1"}]}]},
                %%
                <<"UPDATE users SET \"first\" = 'Chris', \"last\" = $2 WHERE id = $1">>
            },
            {
                %%
                {delete, "users", [{where, [{"id", "?"}]}]},
                %%
                <<"DELETE FROM users WHERE id = ?">>
            },
            {
                %%
                {delete, "users", [{where, [{"id", "$1"}]}]},
                %%
                <<"DELETE FROM users WHERE id = $1">>
            },
            {
                %%
                {select, ["id", "username"], "user", [{limit, "$1"}]},
                %%
                <<"SELECT id, username FROM \"user\" LIMIT $1">>
            },
            {
                %%
                {select, ["id", "username"], "user", [{offset, "$1", limit, <<"$2">>}]},
                %%
                <<"SELECT id, username FROM \"user\" OFFSET $1 LIMIT $2">>
            },
            {
                %%
                {select, ["id", "username"], "user", [{limit, <<"?">>}]},
                %%
                <<"SELECT id, username FROM \"user\" LIMIT ?">>
            },
            {
                %%
                {select, ["id", "username"], "user", [{offset, "?", limit, "?"}]},
                %%
                <<"SELECT id, username FROM \"user\" OFFSET ? LIMIT ?">>
            }
        ]).


resolve_placeholders_test_() ->
    test_utils:generate(
        {erma, resolve_placeholders},
        [
            {
                %%
                {insert, "users", ["first", "last", "age"], [{pl, "A"}, {pl, "B"}, {pl, "C"}]},
                %%
                {{insert, "users", ["first", "last", "age"], ["$1", "$2", "$3"]}, ["A", "B", "C"]}
            },
            {
                %%
                {insert, "users", ["first", <<"last">>, age], [123, {pl, 456}, {pl, 789}, 4242]},
                %%
                {{insert, "users", ["first", <<"last">>, age], [123, "$1", "$2", 4242]}, [456, 789]}
            },
            {
                %%
                {insert_rows, "users", ["first", "last", "age"],
                    [
                        [{pl, "A"}, {pl, "B"}, {pl, "C"}]
                        ,[{pl, "D"}, {pl, "E"}, {pl, "G"}]
                        ,["H", {pl, "I"}, <<"J">>]
                    ]
                },
                %%
                {
                    {insert_rows, "users", ["first", "last", "age"],
                        [
                            ["$1", "$2", "$3"]
                            ,["$4", "$5", "$6"]
                            ,["H", "$7", <<"J">>]
                        ]
                    },
                    ["A", "B", "C", "D", "E", "G", "I"]
                }
            },
            {
                %%
                {select, [], "users", [{where, [{"name", {pl, "John"}}]}]},
                %%
                {{select, [], "users", [{where, [{"name", "$1"}]}]}, ["John"]}
            },
            {
                %%
                {select, [], "users", [{where, [{"name", {pl, "John"}}, {"age", gt, {pl, 18}}]}]},
                %%
                {{select, [], "users", [{where, [{"name", "$1"}, {"age", gt, "$2"}]}]}, ["John", 18]}
            },
            {
                %%
                {select, [], "users",
                    [{where, [{'or', [{'and', [{"last", {pl, <<"Silver">>}}, {"name", {pl, <<"John">>}}]},
                        {"email", {pl, "some@where.com"}},
                        {"age", gt, {pl, 18}}]}]}
                    ]},
                %%
                {{select, [], "users",
                    [{where, [{'or', [{'and', [{"last", "$1"}, {"name", "$2"}]},
                        {"email", "$3"},
                        {"age", gt, "$4"}]}]}
                    ]}, [<<"Silver">>, <<"John">>, "some@where.com",18]}
            },
            {
                %%
                {update, "users", [{"first", "Chris"}, {"last", {pl, "Pratt"}}], [{where, [{"id", {pl, 42}}]}]},
                %%
                {{update, "users", [{"first", "Chris"}, {"last", "$1"}], [{where, [{"id", "$2"}]}]}, ["Pratt", 42]}
            },
            {
                %%
                {delete, "users", [{where, [{"id", {pl, 777}}]}]},
                %%
                {{delete, "users", [{where, [{"id", "$1"}]}]}, [777]}
            },
            {
                %%
                {select, ["id", "username"], "user", [{limit, {pl, 32}}]},
                %%
                {{select, ["id", "username"], "user", [{limit, "$1"}]}, [32]}
            },
            {
                %%
                {select_distinct, ["id", "username"], "user", [{offset, {pl, 10}, limit, {pl, 20}}]},
                %%
                {{select_distinct, ["id", "username"], "user", [{offset, "$1", limit, "$2"}]}, [10, 20]}
            },
            {
                %%
                {select, [], "users", [{where, [{"id", in, [1,2,3]}]}]},
                %%
                {{select, [], "users", [{where, [{"id", in, [1,2,3]}]}]}, []}
            },
            {
                %%
                {select, [], "users", [{where, [{"id", in, [{pl, 1}, {pl, 2}, {pl, 3}]}]}]},
                %%
                {{select, [], "users", [{where, [{"id", in, ["$1", "$2", "$3"]}]}]}, [1,2,3]}
            },
            {
                %%
                {select, [], "users", [{where, [{"id", in, ["a", {pl, "b"}, "c", {pl, "d"}]}]}]},
                %%
                {{select, [], "users", [{where, [{"id", in, ["a", "$1", "c", "$2"]}]}]}, ["b", "d"]}
            }
        ]).


resolve_placeholders_for_mysql_test_() ->
    test_utils:generate(
        {erma, resolve_placeholders}, [#{database => mysql}],
        [
            {
                %%
                {insert, "users", ["first", <<"last">>, age], [123, {pl, 456}, {pl, 789}, 4242]},
                %%
                {{insert, "users", ["first", <<"last">>, age], [123, "?", "?", 4242]}, [456, 789]}
            },
            {
                %%
                {select, [], "users",
                    [{where, [{'or', [{'and', [{"last", {pl, <<"Silver">>}}, {"name", {pl, <<"John">>}}]},
                        {"email", {pl, "some@where.com"}},
                        {"age", gt, {pl, 18}}]}]}
                    ]},
                %%
                {{select, [], "users",
                    [{where, [{'or', [{'and', [{"last", "?"}, {"name", "?"}]},
                        {"email", "?"},
                        {"age", gt, "?"}]}]}
                    ]}, [<<"Silver">>, <<"John">>, "some@where.com",18]}
            },
            {
                %%
                {update, "users", [{"first", "Chris"}, {"last", {pl, "Pratt"}}], [{where, [{"id", {pl, 42}}]}]},
                %%
                {{update, "users", [{"first", "Chris"}, {"last", "?"}], [{where, [{"id", "?"}]}]}, ["Pratt", 42]}
            }
        ]).


resolve_placeholders_for_postgresql_test_() ->
    test_utils:generate(
        {erma, resolve_placeholders}, [#{database => postgresql}],
        [
            {
                %%
                {insert, "users", ["first", <<"last">>, age], [123, {pl, 456}, {pl, 789}, 4242]},
                %%
                {{insert, "users", ["first", <<"last">>, age], [123, "$1", "$2", 4242]}, [456, 789]}
            },
            {
                %%
                {select, [], "users",
                    [{where, [{'or', [{'and', [{"last", {pl, <<"Silver">>}}, {"name", {pl, <<"John">>}}]},
                        {"email", {pl, "some@where.com"}},
                        {"age", gt, {pl, 18}}]}]}
                    ]},
                %%
                {{select, [], "users",
                    [{where, [{'or', [{'and', [{"last", "$1"}, {"name", "$2"}]},
                        {"email", "$3"},
                        {"age", gt, "$4"}]}]}
                    ]}, [<<"Silver">>, <<"John">>, "some@where.com",18]}
            },
            {
                %%
                {update, "users", [{"first", "Chris"}, {"last", {pl, "Pratt"}}], [{where, [{"id", {pl, 42}}]}]},
                %%
                {{update, "users", [{"first", "Chris"}, {"last", "$1"}], [{where, [{"id", "$2"}]}]}, ["Pratt", 42]}
            }
        ]).


resolve_placeholders_for_complex_query_test() ->
    Q = {select, ["id", "username"], "users",
        [
            {where, [
                {'and', [
                    {'or', [
                        {'and', [
                            {"last", {pl, <<"Silver">>}},
                            {"name", {pl, <<"John">>}}
                        ]},
                        {"email", {pl, "some@where.com"}},
                        {"age", gt, {pl, 18}}
                    ]},
                    {'not', {"age", lt, {pl, 80}}},
                    {"id", 'in', [{pl, 1}, {pl, 2}, {pl, 3}]},
                    {"level", between, {pl, 5}, {pl, 10}}
                ]}
            ]},
            {order, ["created"]},
            {offset, {pl, 0}, limit, {pl, 10}}
        ]},
    WaitRes = {{select, ["id", "username"], "users",
        [
            {where, [
                {'and', [
                    {'or', [
                        {'and', [
                            {"last", "$1"},
                            {"name", "$2"}
                        ]},
                        {"email", "$3"},
                        {"age", gt, "$4"}
                    ]},
                    {'not', {"age", lt, "$5"}},
                    {"id", 'in', ["$6", "$7", "$8"]},
                    {"level", between, "$9", "$10"}
                ]}
            ]},
            {order, ["created"]},
            {offset, "$11", limit, "$12"}
        ]}, [<<"Silver">>, <<"John">>, "some@where.com", 18, 80, 1, 2, 3, 5, 10, 0, 10]},
    ?assertEqual(WaitRes, erma:resolve_placeholders(Q)),
    ok.