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
