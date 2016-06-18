-include("macros.hrl").

-export([
    get_registered_users/0,
    get_all_friends/1,
    bulk_insert_into_friends/1,
    insert_into_user/1
]).

get_registered_users() ->
    p1_mysql:fetch(?MYSQL_ID, ?FETCH_ALL_USERS_MYSQL).

get_all_friends() ->
    p1_mysql:fetch(?MYSQL_ID, ?FETCH_ALL_FRIENDS_MYSQL).

bulk_insert_into_friends(Values) ->
    p1_mysql:fetch(?MYSQL_ID, ?INSERT_FRIENDS_INTO_MYSQL ++ Values).

bulk_insert_into_user(Values) ->
    p1_mysql:fetch(?MYSQL_ID, ?INSERT_USERS_INTO_MYSQL ++ Values).
