-type name() :: string().
-type sql() :: binary().

-type equery() :: {equery_type(), table(), [entity()]}.
-type equery_type() :: select | insert | update | delete.

-type table() :: {table, name()} | {table, name(), as, name()}.

-type entity() :: fields_entity() |
                  joins_entity() |
                  where_entity() |
                  order_entity() |
                  offset_entity() |
                  limit_entity().

-type fields_entity() :: {fields, [name()]}.

-type joins_entity() :: {joins, [join()]}.

-type join() :: {join_type(), table()} |
                {join_type(), table(), [join_prop()]} |
                {join_type(), table(), table()} |
                {join_type(), table(), table(), [join_prop()]}.
-type join_type() :: inner | left | right | full.
-type join_prop() :: {pk, name()} | {fk, name()}.

-type where_entity() :: {where, [{name(), where_value()} |
                                 {name(), where_action(), where_value()} |
                                 {name(), in, [where_value()]} |
                                 {name(), between, where_value(), where_value()} |
                                 {'not', where_entity()} |
                                 {'and', [where_entity()]} |
                                 {'or', [where_entity()]}]}.

-type order_entity() :: {order, order_value() | [order_value()]}.

-type offset_entity() :: {offset, integer()}.

-type limit_entity() :: {limit, integer()}.

-type where_action() :: '=' | '<>' | '<' | lt | '>' | gt | '>=' | '<=' | like | in | not_in | between.
-type where_value() :: boolean() | integer() | float() | string().
-type order_value() :: name() | {name(), asc} | {name(), desc}.
