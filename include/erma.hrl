-type name() :: string() | binary() | atom().

-type value() :: boolean() | integer() | float() | string() | binary() |
                 {date, calendar:date()} |
                 {time, calendar:time()} |
                 {datetime, calendar:datetime()}.

-type select() :: select | select_distinct.

-type insert() :: insert | insert_rows.

-type table_name() :: name() | {name(), as, name()}.

-type agg_fun() :: atom().

-type field() :: name() | {name(), as, name()} | {agg_fun(), name()}.

-type join() :: {join_type(), table_name()} |
                {join_type(), table_name(), [join_prop()]}.
-type join_type() :: inner_join | left_join | right_join | full_join.
-type join_prop() :: {pk, name()} | {fk, name()}.

-type where() :: {where, [where_value()]}.

-type where_action() :: '=' | '<>' | '<' | lt | '>' | gt | '>=' | '<=' | like.

-type where_value() :: {name(), value()} |
                       {name(), where_action(), value()} |
                       {name(), in, [value()]} |
                       {name(), not_in, [value()]} |
                       {name(), between, value(), value()} |
                       {'not', where_value()} |
                       {'and', [where_value()]} |
                       {'or', [where_value()]}.

-type order() :: {order, [name() | {name(), asc} | {name(), desc}]}.

-type limit() :: {limit, non_neg_integer()} | {offset, non_neg_integer(), limit, non_neg_integer()}.

-type returning() :: {returning, id} | {returning, [name()]}.

-type select_query() :: {select(), [field()], table_name()} |
                        {select(), [field()], table_name(), [join() | where() | order() | limit()]}.

-type insert_query() :: {insert(), table_name(), [name()], [value()]} |
                        {insert(), table_name(), [name()], [value()], returning()}.

-type update_query() :: {update, table_name(), [{name(), value()}]} |
                        {update, table_name(), [{name(), value()}], [where() | returning()]}.

-type delete_query() :: {delete, table_name()} |
                        {delete, table_name(), [where() | returning()]}.

-type sql_query() :: select_query() | insert_query() | update_query() | delete_query().

-type sql() :: binary().
