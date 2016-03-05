-module(erma_utils_tests).

-include_lib("eunit/include/eunit.hrl").


%% eunit tests

valid_name_test() ->
    lists:foreach(
        fun({Name, Wait}) ->
            ?assertEqual(Wait, erma_utils:valid_name(Name))
        end,
        [
            {"user",      false},
            {"alias",     false},
            {"limit",     false},
            {"like",      false},
            {"where",     false},
            {"123user",   false},
            {"user123",   true},
            {"some_user", true},
            {"_user",     true},
            {"user!",     false},
            {"user@boo",  false}
        ]),
    ok.

prepare_name_test() ->
    lists:foreach(
        fun({Name, Wait}) ->
            ?assertEqual(Wait, erma_utils:prepare_name(Name))
        end,
        [
            {user,               "\"user\""},
            {"user",             "\"user\""},
            {<<"user">>,         "\"user\""},
            {["us", "er"],       "\"user\""},
            {"like",             "\"like\""},
            {"some_user",        "some_user"},
            {["some", "_user"],  "some_user"},
            {"_some_other_user", "_some_other_user"},
            {"users.id",         "users.id"},
            {"users.*",          "users.*"},
            {"user.id",          "\"user\".id"},
            {"user.*",           "\"user\".*"},
            {"user.where",       "\"user\".\"where\""},
            {"my_user.where",    "my_user.\"where\""},
            {["my_user", ".", "where"], "my_user.\"where\""}
        ]),
    lists:foreach(
        fun({Name, Database, Wait}) ->
            ?assertEqual(Wait, erma_utils:prepare_name(Name, Database))
        end,
        [
            {<<"user">>,      postgresql, "\"user\""},
            {"like",          postgresql, "\"like\""},
            {"user.id",       postgresql, "\"user\".id"},
            {"user.*",        postgresql, "\"user\".*"},
            {"my_user.where", postgresql, "my_user.\"where\""},
            {<<"user">>,      mysql, "`user`"},
            {"like",          mysql, "`like`"},
            {"user.id",       mysql, "`user`.id"},
            {"user.*",        mysql, "`user`.*"},
            {"my_user.where", mysql, "my_user.`where`"}
        ]),
    ok.

prepare_table_name_test() ->
    lists:foreach(
        fun({Name, Wait}) ->
                ?assertEqual(Wait, erma_utils:prepare_table_name(Name));
            ({Name, Database, Wait}) ->
                ?assertEqual(Wait, erma_utils:prepare_table_name(Name, Database))
        end,
        [
            {<<"smth">>,           "smth"},
            {<<"user">>,           "\"user\""},
            {{"user", as, "u"},    ["\"user\"", " AS ", "u"]},
            {{"smth", as, "user"}, ["smth",     " AS ", "\"user\""]},
            {{"user", as, "like"}, ["\"user\"", " AS ", "\"like\""]},
            {{"smth", as, "s"},    ["smth",     " AS ", "s"]},
            {<<"user">>,           postgresql, "\"user\""},
            {{"user", as, "u"},    postgresql, ["\"user\"", " AS ", "u"]},
            {{"smth", as, "user"}, postgresql, ["smth",     " AS ", "\"user\""]},
            {{"user", as, "like"}, postgresql, ["\"user\"", " AS ", "\"like\""]},
            {{"smth", as, "s"},    postgresql, ["smth",     " AS ", "s"]},
            {<<"user">>,           mysql, "`user`"},
            {{"user", as, "u"},    mysql, ["`user`",   " AS ", "u"]},
            {{"smth", as, "user"}, mysql, ["smth",     " AS ", "`user`"]},
            {{"user", as, "like"}, mysql, ["`user`",   " AS ", "`like`"]},
            {{"smth", as, "s"},    mysql, ["smth",     " AS ", "s"]}
        ]),
    ok.

format_date_test() ->
    lists:foreach(
        fun({Date, Wait}) ->
            ?assertEqual(Wait, erma_utils:format_date(Date))
        end,
        [
            {{2014,  8,  5}, "2014-08-05"},
            {{2000, 12, 15}, "2000-12-15"},
            {{1970, 10,  1}, "1970-10-01"},
            {{1999,  1, 13}, "1999-01-13"},
            {{1873,  2,  8}, "1873-02-08"},
            {{1000, 11, 25}, "1000-11-25"},
            {{2015,  9, 11}, "2015-09-11"}
        ]),
    ok.

format_time_test() ->
    lists:foreach(
        fun({Time, Wait}) ->
            ?assertEqual(Wait, erma_utils:format_time(Time))
        end,
        [
            {{ 0,  0,  0}, "00:00:00"},
            {{ 1,  5,  6}, "01:05:06"},
            {{ 3, 11,  9}, "03:11:09"},
            {{ 6, 24, 14}, "06:24:14"},
            {{10, 45, 36}, "10:45:36"},
            {{15, 51, 49}, "15:51:49"},
            {{23, 59, 59}, "23:59:59"}
        ]),
    ok.

format_datetime_test() ->
    lists:foreach(
        fun({DateTime, Wait}) ->
            ?assertEqual(Wait, erma_utils:format_datetime(DateTime))
        end,
        [
            {{{1970, 1, 1}, {12, 10, 0}},    "1970-01-01 12:10:00"},
            {{{2000, 12, 31}, {23, 59, 59}}, "2000-12-31 23:59:59"},
            {{{2014, 8, 5}, {17, 34, 30}},   "2014-08-05 17:34:30"}
        ]),
    ok.
