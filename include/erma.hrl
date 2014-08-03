-type name() :: string().
-type sql() :: binary().

-type equery() :: {equery_type(), table(), [entity()]}.
-type equery_type() :: select | insert | update | delete.

-type table() :: {table, name()} | {table, name(), [table_prop()]}.
-type table_relation() :: has_one | has_many | belongs_to.
-type table_prop() :: {table_relation(), table()} | {as, name()} | {pk, name()} | {fk, name()}.

-type entity() :: fields_entity() | with_entity() | where_entity() |
                  order_entity() | offset_entity() | limit_entity().

-type fields_entity() :: {fields, [name()]}.
-type with_entity() :: {with, [table()]}.
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

-type where_action() :: '=' | '<' | '>' | '>=' | '<=' | like.
-type where_value() :: boolean() | integer() | float() | string().
-type order_value() :: name() | {name(), asc} | {name(), desc}.
