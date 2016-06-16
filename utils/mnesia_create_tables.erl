-module(mnesia_create_tables).

-export([
    init/0,
    populate_user/0,
    get_users/0
]).

% -include_lib("stdlib/include/qlc.hrl").
-include("mnesia_structure.hrl").

init() ->
    mnesia:start(),
    mnesia:create_table(user,
                        [{attributes, record_info(fields, user)}]),
    mnesia:create_table(friends,
                        [{attributes, record_info(fields, friends)}, {type, bag}]),

    mnesia:create_table(courier_pid_to_user,
                        [{attributes, record_info(fields, courier_pid_to_user)}]),

    mnesia:create_table(courier_user_to_pid,
                        [{attributes, record_info(fields, courier_user_to_pid)}]).

populate_user() ->
    insert_user("ana", "parolaana").

get_users() ->
    get_all_users().
