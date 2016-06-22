-module(mysql_utils).
-include("macros.hrl").

-export([
    get_all_users/0,
    get_all_friends/0,
    bulk_insert_into_friends/1,
    bulk_insert_into_user/1
]).

%% Returns a list of tuples {User, Friend}.
get_all_friends() ->
    Result = p1_mysql:fetch(?MYSQL_ID, ?FETCH_ALL_FRIENDS_MYSQL),
    case Result of
        {data, {p1_mysql_result, _List, FriendshipList, _Id, _OtherList}} ->
            lists:map(
                fun([User, Friend]) ->
                    {User, Friend}
                end,
                FriendshipList);
        _Other ->
            []
    end.

%% Returns a list of usernames.
get_all_users() ->
    Result =p1_mysql:fetch(?MYSQL_ID, ?FETCH_ALL_USERS_MYSQL),
    case Result of
        {data, {p1_mysql_result, _List, UsernameList, _Id, _OtherList}} ->
            lists:map(
                fun([User, Password]) ->
                    {User, Password}
                end,
                UsernameList);
        _Other ->
            []
    end.

bulk_insert_into_friends(Values) ->
    p1_mysql:fetch(?MYSQL_ID, ?INSERT_FRIENDS_INTO_MYSQL ++ Values, infinity).

bulk_insert_into_user(Values) ->
    p1_mysql:fetch(?MYSQL_ID, ?INSERT_USERS_INTO_MYSQL ++ Values, infinity).
