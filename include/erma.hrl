-type name() :: string() | binary() | atom().

-type value() :: boolean() | integer() | float() | string() | binary() |
                 {date, calendar:date()} |
                 {time, calendar:time()} |
                 {datetime, calendar:datetime()}.

-type select() :: select | select_distinct.

-type table_name() :: name() | {name(), as, name()}.

-type agg_fun() :: atom().

-type field() :: name() | {name(), as, name()} | {agg_fun(), name()}.

-type joins() :: {joins, [join()]}.
-type join() :: {join_type(), join_tables()} |
                {join_type(), join_tables(), [join_prop()]}.
-type join_type() :: inner | left | right | full.
-type join_tables() :: table_name() | {table_name(), table_name()}.
-type join_prop() :: {pk, name()} | {fk, name()}.

-type where() :: {where, [where_condition()]}.

-type where_action() :: '=' | '<>' | '<' | lt | '>' | gt | '>=' | '<=' | like.

-type where_condition() :: {name(), value()} |
                           {name(), where_action(), value()} |
                           {name(), in, [value()]} |
                           {name(), not_in, [value()]} |
                           {name(), between, value(), value()} |
                           {'not', where_condition()} |
                           {'and', [where_condition()]} |
                           {'or', [where_condition()]}.

-type group() :: {group, [name()]}.

-type order() :: {order, [name() | {name(), asc} | {name(), desc}]}.

-type limit() :: {limit, non_neg_integer()} | {offset, non_neg_integer(), limit, non_neg_integer()}.

-type returning() :: {returning, id} | {returning, [name()]}.

-type select_query() :: {select(), [field()], table_name()} |
                        {select(), [field()], table_name(), [joins() | where() | group() | order() | limit()]}.

-type insert_query() :: {insert, table_name(), [name()], [value()]} |
                        {insert, table_name(), [name()], [value()], [returning()]} |
                        {insert_rows, table_name(), [name()], [[value()]]} |
                        {insert_rows, table_name(), [name()], [[value()]], [returning()]}.

-type update_query() :: {update, table_name(), [{name(), value()}]} |
                        {update, table_name(), [{name(), value()}], [where() | returning()]}.

-type delete_query() :: {delete, table_name()} |
                        {delete, table_name(), [where() | returning()]}.

-type sql_query() :: select_query() | insert_query() | update_query() | delete_query().

-type sql() :: binary().
