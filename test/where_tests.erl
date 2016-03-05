-module(where_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").

where_test_() ->
    test_utils:generate(
      [{
         %%
         {select, [], "post",
          [{where, [{'or', [{"title", like, "%funny%"},
                            {"subject", like, "%funny%"},
                            {"content", like, "%funny%"}]}
                   ]}
          ]},
         %%
         <<"SELECT * FROM post ",
           "WHERE (title LIKE '%funny%' OR subject LIKE '%funny%' OR content LIKE '%funny%')">>
       },
       {
         %%
         {select, [], "post",
          [{where, [{'or', [{"posted", '>', {time, {17, 15, 0}}},
                            {"posted", '<', {time, {18, 5, 0}}}]}
                   ]}
          ]},
         %%
         <<"SELECT * FROM post ",
           "WHERE (posted > '17:15:00' OR posted < '18:05:00')">>
       },
       {
         %%
         {select, [], "post",
          [{where, [{'not', {'or', [{"user_id", 20},
                                    {"user_id", 30}]}}
                   ]}
          ]},
         %%
         <<"SELECT * FROM post WHERE (NOT (user_id = 20 OR user_id = 30))">>
       },
       {
         %%
         {select, [], "post",
          [{where, [{"state", in, ["active", "suspended", "unknown"]}
                   ]}
          ]},
         %%
         <<"SELECT * FROM post WHERE \"state\" IN ('active', 'suspended', 'unknown')">>
       },
       {
         %%
         {select, [], "post",
          [{where, [{'or', [{"title", like, "%funny%"},
                            {"subject", like, "%funny%"},
                            {"content", like, "%funny%"}]},
                    {'and', [{"blocked", false},
                             {'or', [{"posted", '>', {datetime, {{2014, 1, 1}, {22, 30, 0}}}},
                                     {"posted", '<', {datetime, {{2013, 12, 20}, {12, 15, 0}}}}]}]},
                    {'not', {'or', [{"user_id", 20},
                                    {"user_id", 30}]}},
                    {"state", in, ["active", "suspended", "unknown"]}
                   ]}
          ]},
         %%
         <<"SELECT * FROM post ",
           "WHERE (title LIKE '%funny%' OR subject LIKE '%funny%' OR content LIKE '%funny%') ",
           "AND (blocked = false AND "
           "(posted > '2014-01-01 22:30:00' OR posted < '2013-12-20 12:15:00')) ",
           "AND (NOT (user_id = 20 OR user_id = 30)) ",
           "AND \"state\" IN ('active', 'suspended', 'unknown')">>
       },
       {
         %%
         {select, ["users.*"], "users",
          [{where, [{'or', [{"users.name", "?"},
                            {"users.name", "?"}]}]}
          ]},
         %%
         <<"SELECT users.* FROM users WHERE (users.\"name\" = ? OR users.\"name\" = ?)">>
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
          [{where, [{'or', [{"x", lt, 5},
                            {'or', [{"y", lt, 3},
                                    {"z", gt, 4}]}]}]}
          ]},
         %%
         <<"SELECT * FROM users WHERE (x < 5 OR (y < 3 OR z > 4))">>
       },
       {
         %%
         {select, [], "users",
          [{where, [{'or', [{"name", like, "?"},
                            {"name", like, "?"}]}]}
          ]},
         %%
         <<"SELECT * FROM users WHERE (\"name\" LIKE ? OR \"name\" LIKE ?)">>
       },
       {
         %%
         {select, [], "the_table", [{where, [{"id", not_in, [1,2,3]}]}]},
         %%
         <<"SELECT * FROM the_table WHERE id NOT IN (1, 2, 3)">>
       },
       {
         %%
         {select, [], "test", [{where, [{"cool", in, [77]}]}]},
         %%
         <<"SELECT * FROM test WHERE cool IN (77)">>
       },
       {
         %%
         {select, [], "test", [{where, [{"cool", in, []}]}]},
         %%
         <<"SELECT * FROM test WHERE cool IN (NULL)">>
       },
       {
         %%
         {select, [], "test", [{where, [{"cool", not_in, []}]}]},
         %%
         <<"SELECT * FROM test WHERE cool NOT IN (NULL)">>
       },
       {
         %%
         {select, [], "test", [{where, [{"id", 1}]}]},
         %%
         <<"SELECT * FROM test WHERE id = 1">>
       },
       {
         %%
         {select, [], "test", [{where, [{"id", '<>', 1}]}]},
         %%
         <<"SELECT * FROM test WHERE id <> 1">>
       },
       {
         %%
         {select, [], "test", [{where, [{"id", lt, 10}]}]},
         %%
         <<"SELECT * FROM test WHERE id < 10">>
       },
       {
         %%
         {select, [], "test", [{where, [{"id", '<=', 10}]}]},
         %%
         <<"SELECT * FROM test WHERE id <= 10">>
       },
       {
         %%
         {select, [], "test", [{where, [{"id", between, 1, 10}]}]},
         %%
         <<"SELECT * FROM test WHERE id BETWEEN 1 AND 10">>
       }
      ]).
