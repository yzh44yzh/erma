-module(select_tests).

-include("erma.hrl").
-include_lib("eunit/include/eunit.hrl").


%% select1_test() ->
%% (deftest select-function
%%   (is (= "SELECT \"users\".\"id\", \"users\".\"username\" FROM \"users\" WHERE (\"users\".\"username\" = ?) ORDER BY \"users\".\"created\" ASC LIMIT 5 OFFSET 3"
%%          (-> (select* "users")
%%              (fields :id :username)
%%              (where {:username "chris"})
%%              (order :created)
%%              (limit 5)
%%              (offset 3)
%%              as-sql))))

%%     ok.


%% select2_test() ->
%%   (sql-only
%%     (are [query result] (= query result)
%%          (select users)
%%          "SELECT \"users\".* FROM \"users\""
%%          (select users-alias)
%%          "SELECT \"u\".* FROM \"users\" AS \"u\""
%%          (select users-with-entity-fields)
%%          "SELECT \"users\".\"id\", \"users\".\"username\" FROM \"users\""
%%          (select users
%%                  (fields :id :username))
%%          "SELECT \"users\".\"id\", \"users\".\"username\" FROM \"users\""
%%          (select users
%%                  (where {:username "chris"
%%                          :email "hey@hey.com"}))
%%          "SELECT \"users\".* FROM \"users\" WHERE (\"users\".\"email\" = ? AND \"users\".\"username\" = ?)"
%%          (select users
%%                  (where {:username "chris"})
%%                  (order :created))
%%          "SELECT \"users\".* FROM \"users\" WHERE (\"users\".\"username\" = ?) ORDER BY \"users\".\"created\" ASC"
%%          (select users
%%                  (where {:active true})
%%                  (order :created)
%%                  (limit 5)
%%                  (offset 3))
%%          "SELECT \"users\".* FROM \"users\" WHERE (\"users\".\"active\" = ?) ORDER BY \"users\".\"created\" ASC LIMIT 5 OFFSET 3")))


%% select3_test() ->
%% (deftest with-many-batch
%%   (is (= "dry run :: SELECT \"users\".* FROM \"users\" :: []\ndry run :: SELECT \"email\".* FROM \"email\" WHERE (\"email\".\"users_id\" IN (?)) :: [1]\n"
%%          (with-out-str
%%            (dry-run
%%             (select user2
%%                     (with-batch email))))) ))

%% (deftest with-one
%%   (sql-only
%%    (is (= "SELECT \"address\".\"state\", \"users\".\"name\" FROM \"users\" LEFT JOIN \"address\" ON \"address\".\"users_id\" = \"users\".\"id\""
%%           (select user2
%%                   (with address)
%%                   (fields :address.state :name))))))


%% select4_test() ->
%% (deftest join-order
%%   (sql-only
%%    (is (= "SELECT \"users\".* FROM (\"users\" LEFT JOIN \"user2\" ON \"users\".\"id\" = \"user2\".\"users_id\") LEFT JOIN \"user3\" ON \"users\".\"id\" = \"user3\".\"users_id\""
%%           (select users
%%                   (join :user2 (= :users.id :user2.users_id))
%%                   (join :user3 (= :users.id :user3.users_id)))))))

%%         "SELECT \"users\".*, \"address\".\"id\" FROM \"users\" LEFT JOIN \"address\" ON \"address\".\"users_id\" = \"users\".\"id\""
%%         (select user2
%%                 (fields :*)
%%                 (with address (fields :id)))

%%         "SELECT \"users\".*, \"address\".*, \"state\".* FROM (\"users\" LEFT JOIN \"address\" ON \"address\".\"users_id\" = \"users\".\"id\") LEFT JOIN \"state\" ON \"state\".\"id\" = \"address\".\"state_id\" WHERE (\"state\".\"state\" = ?) AND (\"address\".\"id\" > ?)"
%%         (select user2
%%                 (fields :*)
%%                 (with address
%%                       (with state (where {:state "nc"}))
%%                       (where {:id [> 5]})))


%% select5_test() ->
%%         ;;Validate has-many executes the second query
%%         "dry run :: SELECT \"users\".* FROM \"users\" :: []\ndry run :: SELECT \"email\".* FROM \"email\" WHERE \"email\".\"email\" LIKE ? AND (\"email\".\"users_id\" = ?) :: [%@gmail.com 1]\n"
%%         (dry-run
%%          (with-out-str
%%            (select user2
%%                    (with email
%%                          (where (like :email "%@gmail.com"))))))

%%         "dry run :: SELECT \"users\".* FROM \"users\" :: []\ndry run :: SELECT \"email\".* FROM \"email\" WHERE \"email\".\"email\" LIKE ? AND (\"email\".\"users_id\" IN (?)) :: [%@gmail.com 1]\n"

%%         (dry-run
%%          (with-out-str
%%            (select user2
%%                    (with-batch email
%%                      (where (like :email "%@gmail.com")))))))))


%% select6_test() ->
%% (deftest dbname-on-tablename
%%   (are [query result] (= query result)
%%        (sql-only
%%         (select author-with-db (with book-with-db)))
%%        "SELECT \"other\".\"author\".*, \"korma\".\"book\".* FROM \"other\".\"author\" LEFT JOIN \"korma\".\"book\" ON \"korma\".\"book\".\"id\" = \"other\".\"author\".\"book_id\""))

%% (deftest schemaname-on-tablename
%%   (are [query result] (= query result)
%%        (sql-only
%%         (select author-with-schema (with book-with-schema)))
%%        "SELECT \"korma\".\"otherschema\".\"author\".*, \"korma\".\"myschema\".\"book\".* FROM \"korma\".\"otherschema\".\"author\" LEFT JOIN \"korma\".\"myschema\".\"book\" ON \"korma\".\"myschema\".\"book\".\"id\" = \"korma\".\"otherschema\".\"author\".\"book_id\""))
