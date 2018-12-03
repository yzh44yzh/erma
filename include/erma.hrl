-type name() :: string() | binary() | atom().

-type value() :: boolean() | integer() | float() | string() | binary() |
                 {date, calendar:date()} |
                 {time, calendar:time()} |
                 {datetime, calendar:datetime()} |
                 {pl, term()} | % placeholder
                 function_call().

-type select() :: select | select_distinct.

-type table_name() :: name() | {name(), as, name()}.

-type agg_fun() :: atom().

-type field() :: name() |
                 {name(), as, name()} |
                 {raw, name()} |
                 {agg_fun(), name()} |
                 {agg_fun(), name(), as, name()} |
                 function_call().

-type joins() :: {joins, [join()]}.
-type join() :: {join_type(), join_tables()} |
                {join_type(), join_tables(), [join_prop()]}.
-type join_type() :: inner | left | right | full.
-type join_tables() :: table_name() | {table_name(), table_name()}.
-type join_prop() :: {pk, name()} | {fk, name()}.

-type where() :: {where, [where_condition()]}.

-type where_action() :: '=' | '<>' | '<' | lt | '>' | gt | '>=' | '<=' | like.

-type where_key() :: name() | function_call().

-type where_value() :: value() | select_query().

-type where_condition() :: {name(), where_value()} |
                           {name(), where_action(), where_value()} |
                           {name(), in, [where_value()]} |
                           {name(), not_in, [where_value()]} |
                           {name(), between, where_value(), where_value()} |
                           {'not', where_condition()} |
                           {'and', [where_condition()]} |
                           {'or', [where_condition()]}.

-type group() :: {group, [name()]}.

-type order() :: {order, [name() | {name(), asc} | {name(), desc}]}.

-type having() :: {having, [where_condition()]}.

-type limit_value() :: non_neg_integer() | binary() | string() | {pl, term()}.

-type limit() :: {limit, limit_value()} | {offset, limit_value(), limit, limit_value()}.

-type returning() :: {returning, id} | {returning, [name()]}.

-type function_call() :: {function, name(), [value()]}.

-type select_query() :: {select(), [field()], table_name()} |
                        {select(), [field()], table_name(),
                         [joins() | where() | group() | having() | order() | limit()]}.

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

-type database() :: postgresql | mysql.

-type erma_options() :: #{database => database()}.