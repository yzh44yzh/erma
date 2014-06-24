-module(erma_tests).
-include_lib("eunit/include/eunit.hrl").


%% simple_test() ->
%%     TAddress = {table, "address"},
%%     TUser = {table, "user", [{has_one, TAddress}]},

%%     {select, TUser, [
%%                      {with, [TAddress]},
%%                      {fields, ["first_name", "last_name", "address.state"]},
%%                      {where, [{"email", "some@where.com"}]}
%%                     ]},

%%     ?assertEqual({ok, 5}, {ok, 5}),
%%     ok.


append_test() ->
    Select0 = {select, {table, "user"},
               [{fields, ["id", "username"]},
                {where, [{"email", like, "*@gmail.com"}]}
               ]},
    Select1 = erma:append(Select0, [{where, [{"active", true}]},
                                    {order, "created"}]),
    Select2 = erma:append(Select1, {limit, 20}),

    ?assertEqual(
       {select, {table, "user"},
        [{fields, ["id", "username"]},
         {where, [{"email", like, "*@gmail.com"}, {"active", true}]},
         {order, "created"}
        ]},
       Select1),
    ?assertEqual(
       {select, {table, "user"},
        [{fields, ["id", "username"]},
         {where, [{"email", like, "*@gmail.com"}, {"active", true}]},
         {order, "created"},
         {limit, 20}
        ]},
       Select2),

    ok.
